module coarray_m
    use iso_c_binding, only : c_ptr
    implicit none

    private

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    type, abstract :: caf_coarray
        contains
        procedure(i_get_element_size) :: get_element_size
        procedure(i_from_c_ptr)       :: from_c_ptr
    end type

    abstract interface
        pure integer function i_get_element_size (this)
            class(caf_coarray), intent(in)    :: this
        end function
        pure subroutine i_from_c_ptr (this, in_c_ptr)
            import :: c_ptr
            class(caf_coarray), intent(inout) :: this
            type(c_ptr),        intent(in)    :: in_c_ptr
        end subroutine
    end interface

    type, extends(caf_coarray) :: coarray_real_sp_r0
        real(sp), pointer :: ptr
        contains
        procedure :: get_element_size =>  rspr0_get_element_size
        procedure :: from_c_pointer   =>  rspr0_from_c_pointer
    end type

    type, extends(caf_coarray) :: coarray_real_sp_r1
        real(sp), pointer :: ptr(:)
        contains
        procedure :: get_element_size =>  rspr1_get_element_size
        procedure :: from_c_pointer   =>  rspr1_from_c_pointer
    end type

    interface

        pure integer module function rspr0_get_element_size (this)
            class(coarray_real_sp_r0), intent(in) :: this
        end function

        pure integer module function rspr1_get_element_size (this)
            class(coarray_real_sp_r1, intent(in)) :: this
        end function

        pure module subroutine rspr0_from_c_ptr (this, in_c_ptr)
            class(coarray_real_sp_r0), intent(inout) :: this
            type(c_ptr),               intent(in)    :: in_c_ptr
        end function

        pure module subroutine rspr1_from_c_ptr (this, in_c_ptr)
            class(coarray_real_sp_r1), intent(inout) :: this
            type(c_ptr),               intent(in)    :: in_c_ptr
        end subroutine
        

    end interface

end module
