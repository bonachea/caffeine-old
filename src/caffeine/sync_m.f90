module sync_m
    implicit none
    private
    public :: caf_sync_all
    
    interface
        module subroutine caf_sync_all() bind(c)
        end subroutine
    end interface
end module sync_m
