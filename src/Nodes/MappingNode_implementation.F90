submodule (fy_MappingNode) MappingNode_implementation
   use fy_AbstractNode
   implicit none


contains
   ! ordering:
   ! (bool, int, string, float, sequence, mapping)

   ! The innards of this algorithm should eventually migrate to gFTL algorithms.
   module function less_than(a, b)
      logical :: less_than
      class(MappingNode), intent(in) :: a
      class(AbstractNode), intent(in) :: b

      integer :: i, na, nb

!!$      select type (b)
!!$      type is (MappingNode)
!!$
!!$         na = a%value%size()
!!$         nb = b%value%size()
!!$         do i = 1, min(na, nb)
!!$            if (a%of(i) < b%of(i)) then
!!$               less_than = .true.
!!$            else if (b%of(i) < a%of(i)) then
!!$               less_than = .false.
!!$               return
!!$            end if
!!$         end do
!!$
!!$         less_than = (na < nb)
!!$      class default
!!$         less_than = .false.
!!$      end select
!!$
   end function less_than

end submodule MappingNode_implementation
   
