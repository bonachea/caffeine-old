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
		If something goes horribly wrong at any stage in the image creation process, 
		call this function to make sure that any spawned child processes get cleaned up.
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
		That thread should only exist for child processes (this_image() > 1)
	*/
	char useless;

	while (1) {
		errno = 0;
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
			// This shouldn't ever happen... Right?... Right?

			fprintf(stderr, "[Caffeine] detect_img1_death: %s\n", strerror(errno));
			fflush (stderr);
			exit (EXIT_FAILURE);

		} else {
			// Oops, we actually read data...

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
	if (signo == SIGCHLD) {
		int wstatus;
		errno  = 0;
		int rc = waitpid (info->si_pid, &wstatus, 0);

		if (rc == -1) {
			// This should never happen, but might as well produce a message if it does.
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
	struct sigaction action_caffeine = { .sa_flags = SA_SIGINFO, .sa_sigaction = &signal_action  };

	if (0 != sigaction(SIGCHLD, &action_caffeine, 0 )) {
		fatal_error("cafc_fork_images/sigaction");
	}

	// Fork
	for (int i = 0; i < num_images-1; i++) {

		int    this_image = i+2; 
					
		int    pipe_fds[2]; // a pipe, as part of our parent-death-detection strategy.

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
		After this function has been called, the death of a child process is not considered to be abnormal.
	*/
	module.termination_begun = true;
}
