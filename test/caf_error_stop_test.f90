program caf_error_stop_test
  use iso_fortran_env, only : error_unit, output_unit
  !! Test error termination
  implicit none
  integer exit_status_integer, exit_status_character

  integer_stop_code: block 

    call execute_command_line( &
      command = "fpm run --example error_stop_integer_code > /dev/null 2>&1", &
      wait = .true., &
      exitstat = exit_status_integer &
    )
    if (exit_status_integer == 0) then
      write(error_unit, *) "----> Error stop with character code failed to return a non-zero exit_status. <----"
    else
      write(output_unit, *) "----> Error stop with character code returned a non-zero exit_status as expected. <----"
    end if

  end block integer_stop_code

  character_stop_code: block 

    call execute_command_line( &
      command = "fpm run --example error_stop_character_code > /dev/null 2>&1", &
      wait = .true., &
      exitstat = exit_status_character &
    )
    if (exit_status_character == 0) then
      write(error_unit, *) "----> Error stop with integer code failed to return a non-zero exit_status. <----"
    else
      write(output_unit, *) "----> Error stop with integer code returned a non-zero exit_status as expected. <----"
    end if

  end block character_stop_code

end program caf_error_stop_test
