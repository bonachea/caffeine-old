submodule(image_enumeration_m) image_enumeration_s
  implicit none
 
contains

  module procedure num_images_team
    image_count = -1
  end procedure

  module procedure num_images_team_number
    image_count = -1
  end procedure

  module procedure this_image_team
  end procedure

  module procedure this_image_coarray_team
  end procedure

  module procedure this_image_coarray_dim_team
  end procedure

end submodule image_enumeration_s