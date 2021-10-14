submodule(team_type_m) team_type_s
    use iso_c_binding 
    use error_termination_m, only: caf_error_stop
    use coarray_m, only: caf_allocate, caf_deallocate, coarray_t, caf_getptr
    use sync_m, only: caf_sync_all
    use image_enumeration_m, only: caf_this_image, caf_num_images
    implicit none


contains

    module procedure caf_change_team
        call caf_sync_all()
        current_team => team
    end procedure

    module procedure caf_end_team
        if (.not. associated(current_team%parent_team)) call caf_error_stop('Cannot end team from the default team.')
        call caf_sync_all()
        current_team => current_team%parent_team
        call caf_sync_all()
    end procedure

    module procedure caf_form_team
        if (present(new_index)) call caf_error_stop('caf_form_team: new_index not implemented.')
        if (present(stat)     ) call caf_error_stop('caf_form_team: stat not implemented.')
        if (present(errmsg)   ) call caf_error_stop('caf_form_team: errmsg not implemented.')

        figure_out_new_imgnums_and_numimgs: block
            type(coarray_t)  :: co_int_obj
            integer, pointer :: co_int_local(:)

            ! create a coarray, integer :: co_int(3)[*]
            ! index 1: the team number passes as `num` to `form_team`
            ! index 2: the new value for `this_image` in the new team
            ! index 3: the new value for `num_images` in the new team

            co_int_obj  = caf_allocate(storage_size(num), [3])
            call c_f_pointer(co_int_obj%mem, co_int_local, [3])

            co_int_local(1) = num

            call caf_sync_all()

            if (caf_this_image() == 1) then
                block
                    integer, pointer :: co_int_remote(:)
                    integer, allocatable :: teamnums(:), newimgnums(:), idxs(:), sorted_teamnums(:)
                    integer i 

                    ! gather the team numbers that each image wants to be assigned to
                    allocate(teamnums(caf_num_images()))
                    do i = 1, size(teamnums)
                        call c_f_pointer(caf_getptr(co_int_obj, i), co_int_remote, [3])
                        teamnums(i) = co_int_remote(1)
                    end do

                    ! figure out the new image numbers
                    idxs = [(i,i=1,size(teamnums))]
                    call sort_idxs(teamnums,idxs)
                    
                    sorted_teamnums = teamnums(idxs) 

                    allocate (newimgnums(size(teamnums)))
                    newimgnums(1) = 1
                    do i = 2, size(teamnums)
                        if (sorted_teamnums(i) /= sorted_teamnums(i-1)) then
                            newimgnums(i) = 1
                        else
                            newimgnums(i) = newimgnums(i-1) + 1
                        end if
                    end do
                    newimgnums(idxs) = newimgnums

                    ! scatter the new image number assignments, and num images
                    do i = 1, size(teamnums)
                        call c_f_pointer(caf_getptr(co_int_obj, i), co_int_remote, [3])
                        co_int_remote(2) = newimgnums(i)
                        co_int_remote(3) = count(teamnums == teamnums(i))
                    end do
                end block 
            end if 

            call caf_sync_all()

            ! let the barrier be a null pointer for now
            team = team_type(co_int_local(3), co_int_local(2), num, c_null_ptr, current_team) 

            call caf_deallocate(co_int_obj)
        end block figure_out_new_imgnums_and_numimgs

        create_sync_barriers_for_new_teams: block
            use posix_interfaces_m
            integer(c_int) :: fd, rtncode

            character(kind=c_char,len=255) :: barrier_shmpath
            character(kind=c_char,len=30)  :: shmpath_suf

            ! get a shared memory path for the new barrier
            write(shmpath_suf, "(I0,'.b')")  team%sibling_id
            call get_shm_path(barrier_shmpath,trim(shmpath_suf))

            ! image 1 on each new team is responsible for creating the shared mem 
            ! for that team's sync barrier
            if (team%this_image_ == 1) then
                fd = sharedmem_create(barrier_shmpath)
                if ( fd == -1 ) call fatal_syscall_error('caf_form_team/shm_open(create)')

                rtncode = ftruncate(fd, BARRIER_SZ)
                if ( rtncode == -1 ) call fatal_syscall_error('caf_form_team/ftruncate')
            end if

            call caf_sync_all()

            if (team%this_image_ /= 1) then
                fd = sharedmem_open(barrier_shmpath)
                if ( fd == -1 ) call fatal_syscall_error('caf_form_team/shm_open(non-create)')

                team%barrier = mmap(fd, BARRIER_SZ)
                if (.not. c_associated(team%barrier)) call fatal_syscall_error('caf_form_team/mmap')
            end if

            team%barrier = mmap(fd, BARRIER_SZ)
            if (.not. c_associated(team%barrier)) call fatal_syscall_error('caf_form_team/mmap')

            rtncode = close_fd(fd)

            ! image 1 on each new team is responsible for initializing the that team's sync barrier
            if (team%this_image_ == 1) then
                rtncode = barrier_init(team%barrier, team%num_images_)
                if (0 /= rtncode) call fatal_syscall_error('caf_form_team/barrier_init')
            end if

            call caf_sync_all()

            if (team%this_image_ == 1) rtncode = sharedmem_unlink(barrier_shmpath)

        end block create_sync_barriers_for_new_teams

        contains
            pure subroutine sort_idxs(array, idxs)
                ! given `array`, produce `idxs`, where 
                ! `array(idxs)` is sorted smallest to largest

                integer, intent(in)  :: array(:)
                integer, intent(out) :: idxs(:)

                integer, allocatable :: copy(:)
                integer :: tmpval
                integer :: i, idxmin

                copy = array
                idxs = [(i, i=1, size(copy))]
                do i = 1, size(copy)-1
                    idxmin = minloc(copy(i+1:),1) + i
                    tmpval = copy(idxmin)
                    if(tmpval < copy(i)) then
                        copy(idxmin) = copy(i)
                        copy(i)      = tmpval

                        tmpval       = idxs(idxmin)
                        idxs(idxmin) = idxs(i)
                        idxs(i)      = tmpval
                    end if
                end do
            end subroutine

    end procedure

end submodule
