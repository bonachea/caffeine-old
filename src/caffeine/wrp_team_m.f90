! This file provides c-interoperable wrappers for the caffeine features
! exposed in team_type_m. Ignore it if you're not interested in using 
! caffeine from C or C-compatible languages.
!
! This is part of an experiment, and can be removed from the 
! caffeine repository (moved to an independent project) prior to making
! caffeine open source. 
!
!   -- Harris

module wrp_team_m
    use team_type_m
    use iso_c_binding, only: c_ptr, c_int, c_loc, c_f_pointer
    implicit none

contains

    subroutine wrp_caf_form_team(num, team) bind(c, name='caf_form_team')
        integer(c_int), intent(in), value :: num
        type(c_ptr),    intent(out)       :: team

        type(team_type), pointer :: t
        allocate(t)

        call caf_form_team(num, t)

        team = c_loc(t)
    end subroutine

    subroutine wrp_caf_change_team(team) bind(c, name='caf_change_team')
        type(c_ptr), intent(in), value :: team
        type(team_type), pointer :: t
        call c_f_pointer(team,t)
        call caf_change_team(t)
    end subroutine

    subroutine wrp_caf_end_team() bind(c, name='caf_end_team')
        call caf_end_team()
    end subroutine

end module
