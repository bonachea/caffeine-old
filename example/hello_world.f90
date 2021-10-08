program hello_world
  use caffeine_m, only : &
    caffeinate, decaffeinate, &
    this_image => caf_this_image, num_images => caf_num_images
  implicit none

  call caffeinate()

  call sleep(1) ! gfortran extension, very temporary
  print *, "hello from project caffeine image ", this_image(), " of ", num_images()
  call sleep(1) ! enough time to send signals and make sure all images terminate

  call decaffeinate()
end program 
