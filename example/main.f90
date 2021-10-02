program main
  use caffeine_m, only : &
    this_image=>caf_this_image(), num_images=>caf_num_image()
  implicit none

  call caf_init()

  print *, "hello from project caffeine image ", this_image(), " of ", num_images()

  call caf_finalize()
end program main
