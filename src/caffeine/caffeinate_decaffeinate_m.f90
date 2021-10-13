module caffeinate_decaffeinate_m
  use iso_c_binding, only: c_size_t
  implicit none

  private
  public :: caffeinate, decaffeinate
  public :: this_image_, num_images_

  integer :: this_image_, num_images_

  interface

    module subroutine caffeinate()
    end subroutine

    module subroutine decaffeinate() 
    end subroutine

  end interface

end module caffeinate_decaffeinate_m
