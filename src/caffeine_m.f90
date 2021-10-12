module caffeine_m
  use error_termination_m, only : caf_error_stop
  use image_enumeration_m, only : caf_this_image, caf_num_images
  use collective_subroutines_m, only : caf_co_sum, caf_co_max, caf_co_min, caf_co_reduce, caf_co_broadcast
  use caffeinate_decaffeinate_m, only : caffeinate, decaffeinate
  use coarray_m, only : caf_allocate, caf_deallocate, coarray_t
  use sync_m, only: caf_sync_all

  implicit none
end module caffeine_m
