program main
   use yafyaml
   use gFTL_IntegerVector
   use gftl_StringIntegerMap
   implicit none

   type(newParser) :: p
   class(AbstractNode), allocatable :: node
   class(AbstractNode), pointer :: subnode

   real :: x
   character(:), allocatable :: name
   logical :: flag

   integer, allocatable :: sequence_a(:)
   integer, allocatable :: sequence_b(:)

   integer :: v1, v2, v3

   integer :: status
   

   p = newParser('core')
   ! TODO should a return code
   node = p%load('simple.yaml')

   x = node%at('x')

   if (x == 1.234) then
      print*,'success',__LINE__
   else
      print*,'failure;  expected 1.234 but found ', x
   end if

   ! TODO:  what about Fred?

   flag = .false.
   flag = node%at('flag')

   if (flag) then
      print*,'success',__LINE__
   else
      print*,'failure;  expected .true.'
   end if

   call node%get(sequence_a, 'sequence_a')

   if (all (sequence_a == [1,2,3,4])) then
      print*,'success', __LINE__
   else
      print*,'failure in handling flow sequence;  expected .true.'
   end if

   call node%get(sequence_b, 'sequence_b')

   if (all (sequence_b == [1,2,3,4])) then
      print*,'success', __LINE__
   else
      print*,'failure in handling block sequence;  expected .true.'
   end if

   ! Flow mapping
   v1 = node%at('mapping_a', 'v1')
   v2 = node%at('mapping_a', 'v2')

   if (v1 == 7 .and. v2 == 8) then
      print*,'success', __LINE__
   else
      print*,'failure in handling flow mapping', v1, v2
   end if

   ! Block mapping
   ! clear old values
   v1 = -1
   v2 = -1
   ! get new values
   v1 = node%at('mapping_b', 'v1')
   v2 = node%at('mapping_b', 'v2')

   if (v1 == 7 .and. v2 == 8) then
      print*,'success', __LINE__
   else
      print*,'failure in handling block mapping', v1, v2
   end if

   v1 = -1
   call node%get(v1, 'mapping_b', 'v1', rc=status)
   v2 = -1
   call node%get(v2, 'mapping_b', 'v2', rc=status)
   if (v1 == 7 .and. v2 == 8 .and. status == YAFYAML_SUCCESS) then
      print*,'success', __LINE__
   else
      print*,'failure in handling block mapping', v1, v2, status
   end if

   ! Handle missing values
   v3 = -1
   if (node%has('mapping_b','v3')) then
      call node%get(v3, 'mapping_b', 'v3', rc=status)
   else
      print*,'expected failure "v3" not found'
   end if


   ! error if wrong type:
   call node%get(flag, 'mapping_b', 'v2', rc=status)

   if (status == YAFYAML_TYPE_MISMATCH) then
      print*,'expected failure (type mismatch)'
   else
      print*,'should have failed, but did not'
   end if
   
end program main
