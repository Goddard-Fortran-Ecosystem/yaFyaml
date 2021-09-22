module fy_MappingNode
   use fy_AbstractNode
   use fy_NodeNodeMap
   implicit none
   private

   type, extends(AbstractNode) :: MappingNode
      private
      type(NodeNodeMap) :: value
   contains
      
   end type MappingNode


   type(NodeNodeMap), target :: EMPTY_MAP
   
contains

   
end module fy_MappingNode
