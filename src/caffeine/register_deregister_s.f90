submodule(register_deregister_m) register_deregister_s
  implicit none

contains

  module procedure caf_register
    select rank(coarray)
      rank(1) 
        select type(coarray)
          type is(integer)
          class default
            error stop "caf_register: unsupported type"
        end select
      rank default
        error stop "caf_register: unsupported rank"
      end select
  end procedure

  module procedure caf_deregister
    select rank(coarray)
      rank(1) 
        select type(coarray)
          type is(integer)
          class default
            error stop "caf_deregister: unsupported type"
        end select
      rank default
        error stop "caf_deregister: unsupported rank"
      end select
  end procedure

end submodule register_deregister_s
