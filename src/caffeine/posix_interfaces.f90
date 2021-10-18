module posix_interfaces_m
    use iso_c_binding
    implicit none

    integer(c_int),    bind(c, name='C_SC_PAGESIZE') :: C_SC_PAGESIZE
    integer(c_int),    bind(c, name='BARRIER_RTN')   :: BARRIER_RTN
    integer(c_size_t), bind(c, name='BARRIER_SZ')    :: BARRIER_SZ

    integer(c_long_long)  :: img1_pid
    integer(c_long)       :: page_size

    interface
        function posix_sysconf (what) bind(C, name='sysconf')
            import :: c_int, c_long
            integer(c_int), intent(in), value  :: what
            integer(c_long) :: posix_sysconf
        end function
        function getpid() bind(C, name='cafc_getpid')
            import :: c_int
            integer(c_int) :: getpid
        end function
        subroutine c_errmsg(buffer, buffer_len) bind(C, name='cafc_get_errmsg')
            import :: c_char, c_size_t, c_ptr
            type(c_ptr),       intent(in), value :: buffer
            integer(c_size_t), intent(in), value :: buffer_len
        end subroutine
        function close_fd(fd) bind(C, name='close')
            import :: c_int
            integer(c_int), intent(in), value :: fd
            integer(c_int)                    :: close_fd
        end function
        function mmap(fd, sz) bind(C, name='cafc_mmap')
            import :: c_int, c_size_t, c_ptr
            integer(c_int),    intent(in), value :: fd
            integer(c_size_t), intent(in), value :: sz
            type(c_ptr)                          :: mmap
        end function
        function munmap(addr, sz) bind(C, name='munmap')
            import :: c_int, c_size_t, c_ptr
            type(c_ptr),       intent(in), value :: addr
            integer(c_size_t), intent(in), value :: sz
            integer(c_int)                       :: munmap
        end function
        function ftruncate(fd, sz) bind(C, name='cafc_ftruncate')
            import :: c_int, c_size_t
            integer(c_int),    intent(in), value    :: fd
            integer(c_size_t), intent(in), value    :: sz
            integer(c_int)                          :: ftruncate
        end function
        function sharedmem_create(shm_name) bind(C, name='cafc_sharedmem_create')
            import :: c_int, c_char, c_ptr
            character(kind=c_char,len=1), intent(in) :: shm_name(*)
            integer(c_int)                           :: sharedmem_create
        end function
        function sharedmem_open(shm_name) bind(C, name='cafc_sharedmem_open')
            import :: c_int, c_char, c_ptr
            character(kind=c_char,len=1), intent(in) :: shm_name(*)
            integer(c_int)                           :: sharedmem_open
        end function
        function sharedmem_unlink(shm_name) bind(C, name='shm_unlink')
            import :: c_int, c_char, c_ptr
            character(kind=c_char,len=1), intent(in) :: shm_name(*)
            integer(c_int)                           :: sharedmem_unlink
        end function
        function barrier_init(barrier, nprocs) bind(C, name='cafc_shared_barrier_init')
            import :: c_int, c_ptr
            type(c_ptr),    intent(in), value :: barrier
            integer(c_int), intent(in), value :: nprocs
            integer(c_int)                    :: barrier_init
        end function
        function barrier_wait(barrier) bind(C, name='pthread_barrier_wait')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: barrier
            integer(c_int) :: barrier_wait
        end function
    end interface

contains 

    subroutine fatal_syscall_error(message)
        use error_termination_m, only: caf_error_stop
        character(len=*), intent(in) :: message
        character(len=1, kind=c_char), target :: errmsg_c
        character(len=:), allocatable         :: errmsg_f

        call c_errmsg (c_loc(errmsg_c), len(errmsg_c,kind=c_size_t))
        errmsg_f = message // ': ' // trim(errmsg_c)

        call caf_error_stop (errmsg_f)
    end subroutine

    subroutine get_shm_path(output_str, suffix)
        use team_type_m, only: team_type, current_team
        use iso_c_binding, only: c_char
        character(len=*,kind=c_char),   intent(in)  :: suffix
        character(len=255,kind=c_char), intent(out) :: output_str
        type(team_type), pointer :: team_stackptr
        
        team_stackptr => current_team

        write(output_str,"(A,I0)") '/caf', img1_pid 

        walk_team_stack: do
            write(output_str,"(A,'_',I0)") trim(output_str), team_stackptr%sibling_id
            if (associated(team_stackptr%parent_team)) then
                team_stackptr => team_stackptr%parent_team
            else
                exit walk_team_stack
            end if
        end do walk_team_stack

        write(output_str, "(A,'_',A)") trim(output_str), trim(suffix) // c_null_char
    end subroutine

    subroutine get_current_shm_path(output_str) bind(c, name='get_current_shm_path')
        character(kind=c_char,len=1), intent(out) :: output_str(255) 

        character(kind=c_char,len=255) :: tmp1
        character(kind=c_char,len=1)   :: tmp2
        integer i

        output_str(:) = c_null_char
        tmp2 = c_null_char
        call get_shm_path(tmp1,tmp2)
        do i = 1, 254
            output_str(i) = tmp1(i:i)
        end do
    end subroutine

end module
