module register_deregister_m
  implicit none

  private
  public :: caf_register, caf_deregister

  interface

    module subroutine caf_register(coarray, coarray_shape)
      implicit none
      class(*), intent(out), pointer :: coarray(..)
      integer, intent(in) :: coarray_shape(:)
    end subroutine

    module subroutine caf_deregister(coarray)
      implicit none
      class(*), intent(inout), pointer :: coarray(..)
    end subroutine

  end interface

end module register_deregister_m
