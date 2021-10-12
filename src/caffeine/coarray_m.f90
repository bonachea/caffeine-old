module coarray_m
    use iso_c_binding, only : c_ptr
    implicit none

    private
    public :: caf_allocate, caf_deallocate, coarray_t

    type, abstract  :: coarray_t
        type(c_ptr) :: mem
    end type

    interface
        module function caf_allocate (element_storage_size, array_shape) 
            integer, intent(in) :: element_storage_size
            integer, intent(in) :: array_shape(:)
            class(coarray_t), allocatable :: caf_allocate
        end function
        module subroutine caf_deallocate(coarray)
            class(coarray_t), intent(inout) :: coarray
        end subroutine
    end interface

end module

