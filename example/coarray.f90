program example_coarray
    use caffeine_m
    use iso_c_binding
    implicit none

    class(coarray_t), allocatable :: co
    integer, pointer :: a(:,:)
    integer :: a_shape(2) 

    call caffeinate()

    a_shape = [3,3]

    co = caf_allocate(storage_size(a), a_shape)
    call c_f_pointer(co%mem, a, a_shape)

    a = caf_this_image()

    call print_matrix(a)

    call caf_deallocate(co)
    call decaffeinate()

contains

    subroutine print_matrix(matx)
        integer :: matx(:,:)
        integer i

        do i = 1, size(matx,2)
            print *, matx(:,i)
        end do
    end subroutine

end program
