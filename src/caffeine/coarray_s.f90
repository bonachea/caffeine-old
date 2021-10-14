submodule(coarray_m) coarray_s
    use iso_fortran_env, only: int8, int64
    use iso_c_binding
    use sync_m, only: caf_sync_all
    use image_enumeration_m, only: caf_this_image, caf_num_images
    use posix_interfaces_m
    use error_termination_m, only: caf_error_stop
    implicit none

    integer(int64) :: coarray_counter = 0
    integer(c_int) :: process_id

    type :: posix_coarray_t
        type(c_ptr)       :: base
        integer(c_size_t) :: length
        integer(c_size_t) :: bytes_per_image
    end type

contains

    pure integer(c_size_t) function  get_bytes_per_image(array_shape, element_storage_size)
        integer, intent(in) :: array_shape(:), element_storage_size

        ! Use the size_t type instead of default integer to prevent overflows
        integer(c_size_t) :: shape_safe(size(array_shape)), elsz_bytes

        shape_safe  = array_shape
        elsz_bytes  = element_storage_size / 8
         
        get_bytes_per_image = max(product(shape_safe),1_c_size_t) * elsz_bytes
        get_bytes_per_image = page_size * (1+(get_bytes_per_image/page_size)) ! round up to page size
    end function

    module procedure caf_allocate
        integer(c_size_t) :: bytes_per_image, total_bytes

        integer(c_int) :: fd, rtncode
        type(c_ptr) :: sharedmem_addr
        integer(int8), pointer :: shmptr(:,:)

        character(kind=c_char, len=255) :: shm_name

        ! TODO make sure the array_shape provided contains only positive values

        bytes_per_image = get_bytes_per_image(array_shape, element_storage_size)
        total_bytes = bytes_per_image * caf_num_images() ! multiply by # of images

        ! Figure out name of the shared memory region we're going to create
        ! This might need to become more sophisticated, I'm not sure.
        coarray_counter = coarray_counter + 1
        block
            character(kind=c_char,len=30) :: pathsuf
            write (pathsuf,"('co',I0)") coarray_counter
            call get_shm_path(shm_name, trim(pathsuf))
        end block 
        

        ! image one creates the shared memory region and sets it to the correct size
        if (caf_this_image() == 1) then
            fd = sharedmem_create(shm_name)
            if ( fd == -1 ) call fatal_syscall_error('shm_open(create)')

            rtncode = ftruncate(fd, total_bytes)
            if ( rtncode == -1 ) call fatal_syscall_error('ftruncate')
        end if

        call caf_sync_all()

        ! other images open the shared memory region that image one created
        if (caf_this_image() /= 1) then
            fd = sharedmem_open(shm_name)
            if ( fd == -1 ) call fatal_syscall_error('shm_open(non-create)')
        end if

        ! all images map the shared memory region into our process
        sharedmem_addr = mmap(fd, total_bytes)
        if (.not. c_associated(sharedmem_addr)) call fatal_syscall_error('mmap')

        ! close the file descriptor associated with the shared memory region.
        ! this does not invalidate the memory mapping. 
        rtncode = close_fd(fd)

        ! each process needs to touch the region of memory that belongs to it,
        ! because on NUMA machines, memory pages usually get allocated on 
        ! whatever NUMA node is closest to the CPU core that first accesses it.
        ! if the images are pinned to specific cores, this could make a significant
        ! difference to performance
        call c_f_pointer(sharedmem_addr, shmptr, [bytes_per_image, int(caf_num_images(),kind=c_size_t)])
        shmptr(:,caf_this_image()) = 0

        call caf_sync_all() 

        ! unlink the shared memory region so that it gets deallocated when the memory is unmapped
        if (caf_this_image() == 1) rtncode = sharedmem_unlink(shm_name)

        ! return the start of the block of memory belonging to this process
        return_data: block
            type(coarray_t)       :: returnval
            type(posix_coarray_t) :: reserved_data  
            reserved_data = posix_coarray_t(sharedmem_addr, total_bytes, bytes_per_image) 
            returnval%mem = c_loc(shmptr(1,caf_this_image()))
            returnval%reserved = transfer(reserved_data, returnval%reserved, size(returnval%reserved))
            caf_allocate  = returnval
        end block return_data
    end procedure

    module procedure caf_deallocate
        integer(c_int) :: rtncode
        type(posix_coarray_t) :: details

        details = transfer (coarray%reserved, details)

        rtncode = munmap(details%base, details%length)
        if (rtncode == -1) call fatal_syscall_error('munmap')
    end procedure

    module procedure caf_getptr
        type(posix_coarray_t)  :: details
        integer(int8), pointer :: memptr(:,:)
        details = transfer (coarray%reserved, details)
        call c_f_pointer(details%base, memptr, [details%bytes_per_image, details%bytes_per_image/details%length])
        caf_getptr = c_loc(memptr(1,coindex))
    end procedure

end submodule
