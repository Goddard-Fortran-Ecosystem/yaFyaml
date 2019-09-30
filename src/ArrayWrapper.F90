! Used to allow arrays to be treated as scalars
module fy_ArrayWrapper
  implicit none
  private

  public :: ArrayWrapper

  type :: ArrayWrapper
     class(*), allocatable :: elements(:)
  end type ArrayWrapper

  interface ArrayWrapper
     module procedure new_ArrayWrapper
  end interface ArrayWrapper

contains

  function new_ArrayWrapper(array) result(wrapper)
    type(ArrayWrapper) :: wrapper
    class(*), intent(in) :: array(:)

    wrapper%elements = array
    
  end function new_ArrayWrapper

end module fy_ArrayWrapper
