submodule(caffeinate_decaffeinate_m) caffeinate_decaffeinate_s
  use iso_c_binding, only : c_int, c_ptr, c_loc, c_char, c_null_char
  use synchronization_m, only : caf_sync_all
  implicit none

contains

  module procedure caffeinate

    interface

      subroutine c_caffeinate(argc, argv) bind(C)
        !! C function prototype: int testhello(int argc, char **argv)
        import c_int, c_ptr
        integer(c_int), value :: argc
        integer(c_int) :: exit_code
        type(c_ptr) argv(*)
      end subroutine

    end interface

  integer i
  integer, parameter :: max_arg_len = 1024

  associate(argc => int(command_argument_count(),c_int))
    associate(argv => [(c_loc(c_interop_arg(i)), i=0,argc)])
      call c_caffeinate(argc, argv)
    end associate
  end associate

  ! TODO: establish non-allocatable coarrays

  call caf_sync_all
  
  exit_code = 0

  contains
 
    function c_interop_arg(argnum) result(arg)
      integer, intent(in) :: argnum
      integer arglen
#ifndef __GFORTRAN__
      character(kind=c_char, len=max_arg_len), target :: arg 
#else
      character(kind=c_char, len=max_arg_len), target :: targ  ! work around gfortran bug
      character(kind=c_char, len=max_arg_len), pointer :: arg 
      arg => targ
#endif
      call get_command_argument(argnum, arg, arglen) 
      if (arglen+1>max_arg_len) error stop "maximum argument length exceeded"
      arg(arglen+1:arglen+1) = c_null_char
    end function
  
  end procedure

  module procedure decaffeinate

    interface

      subroutine c_decaffeinate(exit_code) bind(C)
        import c_int 
        integer(c_int), value :: exit_code
      end subroutine
      
      subroutine c_sync_all() bind(C)
      end subroutine

    end interface

    integer(c_int), parameter :: normal_termination=0
    
    call c_sync_all

    call c_decaffeinate(normal_termination) 
  end procedure

end submodule caffeinate_decaffeinate_s
