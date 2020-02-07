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

      ! workaround for gfortran 9.2 - segfault with logical array with
      ! polymorphic assignment.  Instead use explicit ALLOCATE.
#ifdef __GFORTRAN__
      allocate(wrapper%elements, source=array)
#else
      wrapper%elements = array
#endif

   end function new_ArrayWrapper

end module fy_ArrayWrapper
