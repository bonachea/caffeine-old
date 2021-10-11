program error_stop_integer_code
  use caffeine_m, only : caffeinate, decaffeinate, caf_error_stop 
  implicit none

  call caffeinate

  call caf_error_stop(1)

  call decaffeinate

end program 
