submodule (fy_BoolNode) BoolNode_implementation
   use fy_YAML_Node
   implicit none


contains
   ! ordering:
   ! (bool, int, string, float, sequence, mapping)

   ! The innards of this algorithm should eventually migrate to gFTL algorithms.
   ! Comparing logicals convention: false < true
   module function less_than(a, b)
      logical :: less_than
      class(BoolNode), intent(in) :: a
      class(YAML_Node), intent(in) :: b

      select type (b)
      type is (BoolNode)
         less_than =  (.not. a%value) .and. b%value
      class default
         less_than = .true.
      end select

   end function less_than

end submodule BoolNode_implementation
   
