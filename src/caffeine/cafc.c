#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdatomic.h>
#include <stdnoreturn.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>

/* 
	It's annoyingly common for the constants defined in e.g. the POSIX standard
	to actually be macros instead of real constants. As such, there's often
	no symbol for the linker to use to look up the value. So here we'll grab
	any of the constants that might be needed in Fortran code, and give them 
	symbols that we can access via bind(C).
*/
const int C_SC_PAGESIZE   =  _SC_PAGESIZE;

/*
	Structure to hold the "module variables" needed by the functions in this file. 
	C of course doesn't actually have modules, but using a struct with a single instance
	provides some approximation for them.
*/
typedef struct {
	atomic_bool  termination_begun; // all images
	pid_t       *child_pids;        // image 1 only
	int          nchildprocs;       // image 1 only
	int          parent_pipe;       // child images only
	pid_t        parent_pid;        // child images only
	pthread_t    pipe_thread;       // child images only
} cafc_vars_t;

cafc_vars_t module = {
	.termination_begun = false,
};

static noreturn void 
fatal_error (const char * msg) 
{
	/*
		If something goes wrong during the the image creation process, call this function to 
		make sure that any spawned child processes get cleaned up.

		This function should only be called from `cafc_fork_images`. If `cafc_fork_images`
		returns successfully, there's no need to call this, as all processes will get cleaned
		up correctly if any of them crash.
	*/

 const char * safemsg = msg ? msg : "";

	if (module.child_pids) {
		for (size_t i = 0; i < module.nchildprocs; i++) {
			if(module.child_pids[i]) {
				kill (module.child_pids[i], SIGTERM);
			}
		}
	}


	fprintf (stderr, "[Caffeine] %s: %s\n", safemsg, strerror(errno));
	fflush  (stderr);
	exit    (EXIT_FAILURE);
}

void *
detect_img1_death (void * unused) 
{
	/*
		This function (run in a background thread) monitors for the parent process's death.
		That thread should only exist for child processes (this_image() > 1).

		The method used to detect when the parent process has died is as follows:
		A pipe exists between the parent (image 1) and the child process (image N where N > 1).
		The child process (again - in a background thread) does a blocking read on that pipe.
		The parent never writes anything to that pipe, so the read should block forever. 
		However, if the parent process dies, the OS will close the pipe and the blocking read
		will return "end of file" in the child. The child can then check whether parent death
		was expected or not (i.e. have we already begun "normal termination"). If the parent 
		process dies unexpectedly, we treat that as "error termination" (to use the language
		of the Fortran standard). 
	*/
	char useless;

	// Loop in case the read call gets interrupted by a signal, i.e. EINTR
	while (1) {
		errno = 0;
		// Blocking read call
		const ssize_t rc = read (module.parent_pipe, &useless, 1);
		
		if (rc == 0) {
			// End of file - i.e. the parent process died.

			// That's okay if we've already begun normal termination
			if (module.termination_begun) {
				break;
			} else {
				fprintf(stderr, "[Caffeine] Image 1 died, terminating.\n");
				fflush (stderr);
				exit (EXIT_FAILURE);
			}

		} else if (rc == -1 && errno != EINTR) {
			// The read failed for some reason other than the receipt of a signal (EINTR).
			// This shouldn't ever happen, but if it does, let's hear about it.

			fprintf(stderr, "[Caffeine] detect_img1_death: %s\n", strerror(errno));
			fflush (stderr);
			exit (EXIT_FAILURE);

		} else {
			// Oops, we actually read data...
			// This means the fortran programmer used a hard coded I/O unit number, and it collided with the
			// file descriptor used for our pipe

			fprintf(stderr, "[Caffeine] detect_img1_death: conflict detected between a file descriptor used by Fortran code, "
					"and one internal to Caffeine. Probable solution: use `newunit=` in all Fortran open statements, "
					"rather than using hard-coded unit numbers.\n");

			fflush (stderr);
			exit (EXIT_FAILURE);
		}
	}

	return 0;
}

void 
signal_action (int signo, siginfo_t * info, void * context)
{
	/*
		Signal handler for the parent process.
		The means by which the child processes detect the death of the parent process was explained
		in the comment associated with `detect_img1_death`, but this function is what allows the 
		parent process (image 1) to monitor the children.

		If a child process dies, the parent will receive SIGCHLD from the kernel. 
		If "normal termination" has already begun, then that's fine, but if it hasn't, then the child
		crashed, and we treat it as "error termination".
	*/

	if (signo == SIGCHLD) {
		int wstatus;
		errno  = 0;
		int rc = waitpid (info->si_pid, &wstatus, 0);

		if (rc == -1) {
			// `waitpid` failed.
			// This should never happen, but we might as well produce a message if it does.
			fprintf (stderr, "[Caffeine] signal_action/waitpid: %s \n", strerror(errno));
			fflush  (stderr);
			exit (EXIT_FAILURE);
		}

		if (WIFEXITED(wstatus) || WIFSIGNALED(wstatus)) {
			// A child process terminated.
			// That's okay if we've begun normal termination.
			if (module.termination_begun) {
				return;
			} else {
				// Not okay, error termination.
				fprintf(stderr, "[Caffeine] A child process terminated unexpectedly.\n" );
				fflush (stderr);
				exit (EXIT_FAILURE);
			}
		}
	}
}

int 
cafc_fork_images(int num_images)
{
	/*
		Create all the child processes that will serve as our images.
		Returns the image number of this image, or terminates on failure.
	*/

	// check for invalid argument	
	if (num_images < 1) {
		errno = EINVAL;
		fatal_error("cafc_fork_images");
	}

	// allocate our array of process ids	
	module.child_pids = calloc(num_images-1, sizeof(pid_t));
	if (!module.child_pids) {
		fatal_error("cafc_fork_images/calloc");
	}

	module.parent_pid = getpid();

	// Set up our signal handler for the parent
	// This allows image 1 to detect if other images die unexpectedly (see `signal_action`).
	struct sigaction action_caffeine = { .sa_flags = SA_SIGINFO, .sa_sigaction = &signal_action  };

	if (0 != sigaction(SIGCHLD, &action_caffeine, 0 )) {
		fatal_error("cafc_fork_images/sigaction");
	}

	// Fork
	for (int i = 0; i < num_images-1; i++) {

		int    this_image = i+2; 
					
		int    pipe_fds[2]; 

		// See `detect_img1_death` for an explanation of how the child processes monitor for the unexpected
		// termination of image1... A pipe is involved (one per child process). 
		if (0 != pipe (pipe_fds)) 
			fatal_error("caf_fork_images/pipe");

		pid_t  newpid = fork();

		if (newpid == 0) {

			// I am the child

			// Uninstall the parent's signal handlers
			struct sigaction action_default = { .sa_handler = SIG_DFL  };
			if (0 != sigaction(SIGCHLD, &action_default, 0 )) {
				perror ("[Caffeine] sigaction");
				fflush (stderr);
				exit   (EXIT_FAILURE);
			}

			close (pipe_fds[1]); // close the write-end of the pipe
			module.parent_pipe = pipe_fds[0];

			// Spawn a thread that tries to read from that pipe. It will get an EOF if the parent dies.
			if (0 != pthread_create(&module.pipe_thread, 0, detect_img1_death, 0)) {
				perror ("[Caffeine] pthread_create (monitor thread)");
				fflush (stderr);	
				exit   (EXIT_FAILURE);
			}

			// Since we get a copy of the parent process's memory, 
			// we may have a half-constructed list of child processes.
			// We don't need that, so get rid of it.
			free (module.child_pids);
			module.child_pids  = 0;
			module.nchildprocs = 0;

			return this_image;

		} else if (newpid == -1) {

			// It broke

			fatal_error("cafc_fork_images/fork");

		} else {

			// I am the parent

			module.child_pids[i] = newpid;
			module.nchildprocs++;
			close (pipe_fds[0]); // close the read-end of the pipe
			// we don't need to store the pipe fd in the parent, the OS closes it on exit.

		}
	}

	return 1; // parent is the only one that reaches here.
}



void 
cafc_begin_termination()
{
	/*
		Begins "normal termination". 
		After this function has been called, the death of a child process is not considered to be abnormal.
	*/
	module.termination_begun = true;
}



int 
cafc_sharedmem_create(const char * name)
{
	/*
		Creates a shared memory region with the given name.
		The region is opened with read/write access, and it's permissions
		are: owner can read/write, group and others have no access.
		Returns a file descriptor on success, or -1 on failure
	*/
	return shm_open(name, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);
}

int 
cafc_sharedmem_open(const char * name)
{
	/*
		Opens an existing shared memory region with the given name, for read/write access.
		Returns a file descriptor on success, or -1 on failure
	*/
	return shm_open(name, O_RDWR, 0);
}

void 
cafc_get_errmsg(char * buf, size_t bufsz)
{
	/*
		POSIX functions tend to set errno to some value that indicates what went wrong,
		in the event of a failure. Textual representations of those error values 
		can be retrieved with `strerror`. To get those strings back into Fortran,
		this function copies the result of strerror into a user-provided buffer.
	*/
	char * msg = strerror(errno);

	const size_t len    = strlen(msg);
	const size_t cpylen = len < bufsz ? len : bufsz;

	memcpy (buf, msg, cpylen);
	buf[bufsz-1] = 0; // ensure it's null-terminated
}

int 
cafc_ftruncate(int fd, size_t sz)
{
	/*
		Truncates or null-pads a file to specified size.
		We use this for setting the size of a shared memory region.
		Unfortunately the second argument of the actual ftruncate function has the
		type "off_t", which may vary depending on platform.
	*/
	return ftruncate(fd,sz);
}

void *
cafc_mmap(int fd, size_t length)
{
	/*
		Map the shared memory region into our address space.
		Unfortuantely the last argument to mmap is of type `off_t`, which may
		vary depending on platform, so we have to call it from C.
		We also can't do pointer comparison in Fortran, so if mmap gives us
		`MAP_FAILED` we need to return NULL ourselves.
	*/
	void * addr =  mmap(0, length, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
	if (addr == MAP_FAILED) return NULL;
}

long long
cafc_getpid()
{
	/*
		Get the process ID. Unfortunately, getpid returns a `pid_t`, which
		may vary depending on platform.
	*/
	return getpid();
}


