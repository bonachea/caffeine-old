submodule(sync_m) sync_s
    implicit none
contains
    module procedure caf_sync_all
        call sleep(3) ! VERY VERY TEMPORARY
    end procedure 
end submodule
