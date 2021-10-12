submodule(caffeinate_decaffeinate_m) caffeinate_decaffeinate_s
    use iso_fortran_env, only: error_unit
    use iso_c_binding
    use sync_m, only: caf_sync_all
    implicit none

    integer, bind(c, name='C_SC_PAGESIZE') :: C_SC_PAGESIZE

contains
    module procedure caffeinate

        determine_page_size: block
            interface
                function posix_sysconf (what) bind(C, name='sysconf')
                    import :: c_int, c_long
                    integer(c_int)  :: what
                    integer(c_long) :: sysconf
                end function
            end interface

            page_size = posix_sysconf(C_SC_PAGESIZE)
        end block determine_page_size

        determine_num_images: block
            integer            :: stat
            character(len=100) :: envvar_str
            num_images_ = 0

            call get_environment_variable("CAF_NUM_IMAGES", value=envvar_str, status=stat)

            if (stat == 0) then
                ! We have been asked for a specific number of images.
                read (envvar_str, '(I20)', iostat=stat) num_images_

                if (stat /= 0) then
                    write (error_unit, '(A)') &
                        '[Caffeine] Warning: environment varialbe "CAF_NUM_IMAGES" is present, &
                        & but its value is invalid. Falling back to default number of images.'
                    num_images_ = 0
                end if
            end if

            if (num_images_ == 0) then
                ! No (usable) number of images was specified, we need to pick a default.
                ! It's not trivial to portably get the number of cores available. 
                ! Temporary solution: default to 4.
                num_images_ = 4
            end if
        end block determine_num_images

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
            
            this_image_ = fork_images(num_images_)
           
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
