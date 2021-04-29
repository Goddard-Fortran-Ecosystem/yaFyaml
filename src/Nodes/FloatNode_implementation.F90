submodule (fy_FloatNode) FloatNode_implementation
   use fy_AbstractNode
   use fy_MappingNode
   use fy_SequenceNode
   implicit none


contains
   ! ordering:
   ! (bool, int, string, float, sequence, mapping)

   ! The innards of this algorithm should eventually migrate to gFTL algorithms.
   module function less_than(a, b)
      logical :: less_than
      class(FloatNode), intent(in) :: a
      class(AbstractNode), intent(in) :: b

      integer :: i, na, nb

      select type (b)
      type is (MappingNode)
         less_than = .true.
      type is (SequenceNode)
         less_than = .true.
      type is (FloatNode)
         less_than = a%value < b%value
      class default
         less_than = .false.
      end select

   end function less_than

end submodule FloatNode_implementation
   
