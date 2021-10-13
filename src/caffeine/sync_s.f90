submodule(sync_m) sync_s
    use iso_c_binding
    use posix_interfaces_m
    use error_termination_m, only: caf_error_stop
    implicit none

contains
    module procedure caf_sync_all
        integer(c_int) rtncode
        rtncode = barrier_wait(sync_all_barrier)
        if (rtncode /= 0 .and. rtncode /= BARRIER_RTN) then
            call caf_error_stop("pthread_barrier_wait returned an error")
        end if
    end procedure 
end submodule
