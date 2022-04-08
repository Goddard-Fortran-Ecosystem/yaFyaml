submodule (fy_StringNode) StringNode_implementation
   use fy_YAML_Node
   use fy_BoolNode
   use fy_IntNode
   implicit none


contains
   ! ordering:
   ! (bool, int, string, float, sequence, mapping)

   ! The innards of this algorithm should eventually migrate to gFTL algorithms.
   module function less_than(a, b)
      logical :: less_than
      class(StringNode), intent(in) :: a
      class(YAML_Node), intent(in) :: b

      integer :: i, na, nb

      select type (b)
      type is (BoolNode)
         less_than = .false.
      type is (IntNode)
         less_than = .false.
      type is (StringNode)
         less_than = a%value < b%value
      class default
         less_than = .true.
      end select

   end function less_than

end submodule StringNode_implementation
   
