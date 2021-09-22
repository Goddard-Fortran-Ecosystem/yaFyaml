submodule (fy_IntNode) IntNode_implementation
   use fy_AbstractNode
   use fy_BoolNode
   implicit none


contains
   ! ordering:
   ! (bool, int, string, float, sequence, mapping)

   ! The innards of this algorithm should eventually migrate to gFTL algorithms.
   module function less_than(a, b)
      logical :: less_than
      class(IntNode), intent(in) :: a
      class(AbstractNode), intent(in) :: b

      integer :: i, na, nb

      select type (b)
      type is (BoolNode)
         less_than = .false.
      type is (IntNode)
         less_than = a%value < b%value
      class default
         less_than = .true.
      end select

   end function less_than

end submodule IntNode_implementation
   
