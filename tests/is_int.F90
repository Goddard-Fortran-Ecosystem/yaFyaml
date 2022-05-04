



program main
   use fy_Nodes
   use fy_ErrorCodes
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none

   call test_is_bool()
   call test_is_mapping()

contains
   
   subroutine test_is_bool()
      class(YAML_Node), allocatable :: node
      logical :: flag

      allocate(node,source=BoolNode(.false.))
      select type (node)
      type is (BoolNode)
         node = BoolNode(.false.)
      end select
      flag = node%is_bool()


   end subroutine test_is_bool

   subroutine test_is_mapping()
      class(YAML_Node), allocatable :: node
      type(MappingNode), target, allocatable :: m_node
      logical :: flag

      node = MappingNode()
      m_node = MappingNode()
      flag = m_node%is_mapping()
      flag = node%is_mapping()
      node = IntNode(1_INT32)
      flag = node%is_mapping()
      flag = node%is_int()

   end subroutine test_is_mapping

end program main
