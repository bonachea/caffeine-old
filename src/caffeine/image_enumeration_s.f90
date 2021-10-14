submodule(image_enumeration_m) image_enumeration_s
  use team_type_m, only: current_team
  implicit none
 
contains

  module procedure num_images_team
    if (present(team)) error stop "num_images(team): not implemented"
    image_count = current_team%num_images_
  end procedure

  module procedure num_images_team_number
    image_count = -1
  end procedure

  module procedure this_image_team
    if (present(team)) error stop "this_image(team): not implemented"
    this_image_team = current_team%this_image_
  end procedure

  module procedure this_image_coarray_team
  end procedure

  module procedure this_image_coarray_dim_team
  end procedure

end submodule image_enumeration_s
