submodule(error_termination_m) error_termination_s
  use iso_fortran_env, only : error_unit
  use iso_c_binding, only : c_char, c_int
  implicit none

contains

  module procedure caf_error_stop_character

    integer, parameter :: error_occured = 1
    
    write(error_unit, *) stop_code
    flush error_unit
    call caf_error_stop_integer(error_occured)
 
  end procedure 

  module procedure caf_error_stop_integer
 
    interface
      subroutine system_exit(return_code) bind(C, name="exit")
        import c_int
        integer(c_int), value :: return_code
      end subroutine
    end interface
    
    call system_exit(int(stop_code, c_int))

  end procedure 

end submodule error_termination_s
