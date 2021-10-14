program example_teams
    use caffeine_m
    use iso_c_binding
    implicit none

    real r
    type(team_type), target :: team
    integer :: team_num

    call caffeinate()

    r = real(caf_this_image())

    if (mod(caf_this_image(),2) == 0) then
        team_num = 1
    else
        team_num = 2
    end if

    call caf_form_team(team_num,team)

    call caf_change_team(team)

        write (*, "('img ',I0,' of ',I0,' on team ',I0,' has value ',G0)") caf_this_image(), caf_num_images(), team_num, r

    call caf_end_team()

    call decaffeinate()

contains

end program
