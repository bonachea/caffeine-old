module caf_this_image_test
    use caffeine_m, only : caf_this_image
    use vegetables, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_payload

contains
    function test_caf_this_image() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "The caf_this_image function", &
                [ it("gives a result in the range [1,num_images()]", check_this_image_in_range) &
                , it("gives a unique result on each image", check_unique_result) &  
                ])
    end function

    function check_this_image_in_range() result(result_)
        type(result_t) :: result_
        logical is_in_range
        
        is_in_range = caf_this_image() >= 1 .and. caf_this_image() <= caf_num_images()
        
        result_ = assert_that(is_in_range, "image out of range")
    end function

    function check_unique_result() result(result_)
        type(result_t) :: result_
        logical is_unique
        integer, allocatable :: image(:)[:]

        call caf_allocate(image(caf_num_images())[*])

        is_unique = count([(i, i=1,caf_num_images())]==caf_this_image()) == 1

        result_ = assert_that(is_in_range, "image out of range")
    end function

end module caf_this_image_test
