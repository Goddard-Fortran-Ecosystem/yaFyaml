program main
   use yafyaml, only : Parser, YAML_Node, FileStream
   implicit none

   type(Parser) p
   type(YAML_Node) c
   integer, allocatable :: nodes(:)

   p = Parser('core')
   c = p%load('integer-array.json')
   call c%get(nodes, 'nodes')

   if (any(nodes/=[1,2,3])) error stop "Test failed: wrong nodes values."

   print *,"Test passed"

end program
