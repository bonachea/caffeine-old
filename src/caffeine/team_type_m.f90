module team_type_m
  use iso_c_binding, only: c_ptr
  implicit none

  private
  public :: team_type, caf_form_team, current_team, caf_end_team, caf_change_team

  type :: team_type
    integer     :: num_images_
    integer     :: this_image_
    integer     :: sibling_id
    type(c_ptr) :: barrier 
    type(team_type), pointer :: parent_team => null()
  end type

  interface
    module subroutine caf_form_team (num, team, new_index, stat, errmsg) 
        integer,          intent(in)  :: num
        type(team_type),  intent(out) :: team
        integer,          intent(in),    optional :: new_index
        integer,          intent(out),   optional :: stat
        character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine caf_change_team(team) 
        type(team_type), target, intent(in) :: team
    end subroutine

    module subroutine caf_end_team() 
    end subroutine

  end interface

  type(team_type), pointer :: current_team

end module team_type_m

