program main
   use yafyaml
   use gFTL_IntegerVector
   use gftl_StringIntegerMap
   implicit none

   type(Parser) :: p
   type(Configuration) :: config
   type(Configuration) :: subconfig

   real :: x
   character(:), allocatable :: name
   logical :: flag

   integer, allocatable :: sequence_a(:)
   integer, allocatable :: sequence_b(:)

   integer :: v1, v2, v3

   logical :: found
   integer :: status
   

   p = Parser('core')
   config = p%load(FileStream('simple.yaml'))

   x = config%at('x')

   if (x == 1.234) then
      print*,'success'
   else
      print*,'failure;  expected 1.234 but found ', x
   end if


   flag = .false.
   flag = config%at('flag')

   if (flag) then
      print*,'success'
   else
      print*,'failure;  expected .true.'
   end if

   call config%get(sequence_a, 'sequence_a')

   if (all (sequence_a == [1,2,3,4])) then
      print*,'success'
   else
      print*,'failure in handling flow sequence;  expected .true.'
   end if

   call config%get(sequence_b, 'sequence_b')

   if (all (sequence_b == [1,2,3,4])) then
      print*,'success'
   else
      print*,'failure in handling block sequence;  expected .true.'
   end if

   ! Flow mapping
   v1 = config%at('mapping_a', 'v1')
   v2 = config%at('mapping_a', 'v2')

   if (v1 == 7 .and. v2 == 8) then
      print*,'success'
   else
      print*,'failure in handling flow mapping', v1, v2
   end if

   ! Block mapping
   v1 = config%at('mapping_b', 'v1')
   v2 = config%at('mapping_b', 'v2')

   if (v1 == 7 .and. v2 == 8) then
      print*,'success'
   else
      print*,'failure in handling block mapping', v1, v2
   end if

   v1 = -1
   call config%get(v1, 'mapping_b', 'v1', found=found, rc=status)
   v2 = -1
   call config%get(v2, 'mapping_b', 'v2', found=found, rc=status)
   if (v1 == 7 .and. v2 == 8 .and. found .and. status == YAFYAML_SUCCESS) then
      print*,'success'
   else
      print*,'failure in handling block mapping', v1, v2, found, status
   end if

   ! Handle missing values
   v3 = -1
   call config%get(v3, 'mapping_b', 'v3', found=found, rc=status)
   if (v3 == -HUGE(1) .and. (.not. found) .and. status == YAFYAML_SUCCESS) then
      print*,'success'
   else
      print*,'failure in handling block mapping', found, v3
   end if

   ! error if wrong type:
   call config%get(flag, 'mapping_b', 'v2', found=found, rc=status)
   if (found .and. status == YAFYAML_TYPE_MISMATCH) then
      print*,'expected failure'
   else
      print*,'should have failed, but did not'
   end if
   
end program main
