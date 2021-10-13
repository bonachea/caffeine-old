module coarray_m
    use iso_c_binding, only : c_ptr
    use iso_fortran_env, only: int8
    implicit none

    private
    public :: caf_allocate, caf_deallocate, coarray_t

    type :: coarray_t
        type(c_ptr)   :: mem
        integer(int8) :: reserved(56)
    end type

    interface
        module function caf_allocate (element_storage_size, array_shape) 
            integer, intent(in) :: element_storage_size
            integer, intent(in) :: array_shape(:)
            type(coarray_t) :: caf_allocate
        end function
        module subroutine caf_deallocate(coarray)
            type(coarray_t), intent(inout) :: coarray
        end subroutine
    end interface

end module

