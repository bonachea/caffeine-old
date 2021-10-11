program error_stop_character_code
  use caffeine_m, only : caffeinate, decaffeinate, caf_error_stop
  implicit none

  call caffeinate

  call caf_error_stop("")

  call decaffeinate

end program 
