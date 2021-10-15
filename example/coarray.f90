program example_coarray
    use caffeine_m
    use iso_c_binding
    implicit none

    type(coarray_t) :: co
    integer, pointer :: a(:,:)
    integer :: a_shape(2) 

    call caffeinate()

    a_shape = [3,3]

    co = caf_allocate(storage_size(a), a_shape)
    call c_f_pointer(co%mem, a, a_shape)

    a = caf_this_image()

    call print_matrix(a)

    call caf_sync_all()

    if (caf_this_image() == 1) then
        block 
            integer i
            type(c_ptr) coindx_ptr
            integer, pointer :: co_a(:,:)
            print *, ''
            do i = 1, caf_num_images()
                coindx_ptr = caf_getptr(co, i)
                call c_f_pointer(coindx_ptr, co_a, a_shape)
                print *, 'value of a(1,1) on image', i, 'is', co_a(1,1)    
            end do
        end block
    end if

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
