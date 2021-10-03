module caffeine_m
  use image_enumeration_m, only : caf_this_image, caf_num_images
  use collective_subroutines_m, only : caf_co_sum, caf_co_max, caf_co_min, caf_co_reduce, caf_co_broadcast
  use caffeinate_decaffeinate_m, only : caffeinate, decaffeinate
  implicit none
end module caffeine_m
