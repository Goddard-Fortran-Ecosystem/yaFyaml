submodule (fy_SequenceNode) SequenceNode_implementation
   use fy_AbstractNode
   use fy_MappingNode
   implicit none


contains
   ! ordering:
   ! (bool, int, string, float, sequence, mapping)

   ! The innards of this algorithm should eventually migrate to gFTL algorithms.
   module function less_than(a, b)
      logical :: less_than
      class(SequenceNode), intent(in) :: a
      class(AbstractNode), intent(in) :: b

      integer :: i, na, nb

      select type (b)
      type is (MappingNode)
         less_than = .true.
      type is (SequenceNode)

         na = a%value%size()
         nb = b%value%size()
         do i = 1, min(na, nb)
            if (a%value%of(i) < b%value%of(i)) then
               less_than = .true.
               return
            else if (b%value%of(i) < a%value%of(i)) then
               less_than = .false.
               return
            end if
         end do

         less_than = (na < nb)
      class default
         less_than = .false.
      end select

   end function less_than

end submodule SequenceNode_implementation
   
