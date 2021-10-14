program example_teams
    use caffeine_m
    use iso_c_binding
    implicit none

    call caffeinate()
    block
        type(team_type), target :: team
        integer :: team_num
        real :: r

        r = real(caf_this_image())

        team_num = (caf_this_image()/3)+1
        call caf_form_team(team_num,team)

        call caf_change_team(team)
        block
            integer i

            type(coarray_t) :: co_obj
            real, pointer   :: co_local, co_remote

            co_obj = caf_allocate(storage_size(co_local), [1])
            call c_f_pointer(co_obj%mem, co_local)
    
            co_local = r

            call caf_sync_all() 

            if (caf_this_image() == 1) then
                do i = 1, caf_num_images()
                    call c_f_pointer(caf_getptr(co_obj,i), co_remote) 
                    print *, 'img', i, 'on team', team_num, 'has value', co_remote
                end do
            end if

            call caf_deallocate(co_obj)
        end block 
        call caf_end_team()

        print *, caf_this_image(), "back on default team"

    end block
    call decaffeinate()

contains

end program
