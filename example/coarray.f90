program example_coarray
    use caffeine_m
    implicit none

    class(coarray_t), allocatable :: co
    integer, pointer :: a(:,:)

    call caffeinate()

    co = caf_allocate([2,2], storage_size(a))
    call c_f_pointer(co%mem, a)

    a = caf_this_image()

    call print_matrix(a)

    call caf_deallocate(a)
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
