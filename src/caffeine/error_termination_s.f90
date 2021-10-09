submodule(error_termination_m) error_termination_s
  use iso_c_binding, only : c_char, c_null_char
  implicit none

contains

  module procedure caf_error_stop_character

    interface

      subroutine fatal_error(msg) bind(C)
        !! stop all images
        import c_char
        character(kind=c_char), intent(in) :: msg(*)
      end subroutine

    end interface

    call fatal_error(trim(adjustl(stop_code))//c_null_char)

  end procedure 

  module procedure caf_error_stop_integer
    integer, parameter :: max_len=128
    character(kind=c_char, len=max_len) :: character_code
 
    write(character_code, *) stop_code

   call caf_error_stop(character_code)

  end procedure 

end submodule error_termination_s
