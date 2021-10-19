submodule(image_enumeration_m) image_enumeration_s
  use team_type_m, only: current_team
  use error_termination_m, only: caf_error_stop
  implicit none
 
contains

  module procedure num_images_team
    if (present(team)) call caf_error_stop("num_images(team): not implemented")
    image_count = current_team%num_images_
  end procedure

  module procedure num_images_team_number
    image_count = -1
  end procedure

  module procedure this_image_team
    if (present(team)) call caf_error_stop("this_image(team): not implemented")
    this_image_team = current_team%this_image_
  end procedure

  module procedure this_image_coarray_team
    call caf_error_stop("this_image_coarray_team: not implemented")
  end procedure

  module procedure this_image_coarray_dim_team
    call caf_error_stop("this_image_coarray_team: not implemented")
  end procedure

  module procedure num_images_bindc
    num_images_bindc = current_team%num_images_
  end procedure
  module procedure this_image_bindc
    this_image_bindc = current_team%this_image_
  end procedure

end submodule image_enumeration_s
