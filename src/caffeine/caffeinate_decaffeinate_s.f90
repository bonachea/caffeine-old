submodule(caffeinate_decaffeinate_m) caffeinate_decaffeinate_s
    use iso_fortran_env, only: error_unit
    use iso_c_binding, only: c_int
    use sync_m, only: caf_sync_all
    use posix_interfaces_m
    use team_type_m, only: current_team
    implicit none

contains
    module procedure caffeinate

        current_team => default_team 
        default_team%parent_team => null()

        ! determine system page size
        page_size = posix_sysconf(C_SC_PAGESIZE)

        ! determine PID of image 1
        img1_pid  = getpid()

        determine_num_images: block
            integer            :: stat
            character(len=100) :: envvar_str
            default_team%num_images_ = 0

            call get_environment_variable("CAF_NUM_IMAGES", value=envvar_str, status=stat)

            if (stat == 0) then
                ! We have been asked for a specific number of images.
                read (envvar_str, '(I20)', iostat=stat) default_team%num_images_

                if (stat /= 0) then
                    write (error_unit, '(A)') &
                        '[Caffeine] Warning: environment variable "CAF_NUM_IMAGES" is present, &
                        & but its value is invalid. Falling back to default number of images.'
                    default_team%num_images_ = 0
                end if
            end if

            if (default_team%num_images_ == 0) then
                ! No (usable) number of images was specified, we need to pick a default.
                ! It's not trivial to portably get the number of cores available. 
                ! Temporary solution: default to 4.
                default_team%num_images_ = 4
            end if
        end block determine_num_images

        create_default_team_barrier: block
            integer rtncode
            type(c_ptr) :: mmap_ptr 

            mmap_ptr = mmap(-1, BARRIER_SZ)
            if (.not. c_associated(mmap_ptr)) call fatal_syscall_error('caffeinate/mmap') 

            rtncode = barrier_init(mmap_ptr, default_team%num_images_)
            if (0 /= rtncode) call fatal_syscall_error('barrier_init')

            default_team%barrier = mmap_ptr
        end block create_default_team_barrier

        create_images: block
            interface
                function fork_images (n_imgs) bind(c, name='cafc_fork_images')
                    ! returns this image's number, either succeeds or terminates.
                    import :: c_int
                    integer(c_int), intent(in), value :: n_imgs
                    integer(c_int)                    :: fork_images 
                end function
            end interface
         
            ! fork_images prints stuff if an error occurrs 
            ! there shouldn't be anything in our error_unit output buffer yet, but just in case...
            flush error_unit 
            
            default_team%this_image_ = fork_images(default_team%num_images_)
           
        end block create_images

    end procedure

    module procedure decaffeinate
        interface
            subroutine begin_termination() bind(c, name='cafc_begin_termination')
            end subroutine
        end interface

        call begin_termination
        call caf_sync_all
    end procedure

end submodule caffeinate_decaffeinate_s
