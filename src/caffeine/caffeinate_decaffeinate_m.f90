module caffeinate_decaffeinate_m
  implicit none

  private
  public :: caffeinate, decaffeinate
  public :: this_image_, num_images_ ! public because the module image_enumeration needs access

  integer :: this_image_, num_images_

  interface

    module subroutine caffeinate()
    end subroutine

    module subroutine decaffeinate() bind(c, name='caf_decaffienate')
    end subroutine

  end interface

end module caffeinate_decaffeinate_m
