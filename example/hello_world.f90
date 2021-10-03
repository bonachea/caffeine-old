program hello_world
  use caffeine_m, only : &
    caffeinate, decaffeinate, &
    this_image => caf_this_image, num_images => caf_num_images
  implicit none

  call caffeinate()

  print *, "hello from project caffeine image ", this_image(), " of ", num_images()

  call decaffeinate()
end program 
