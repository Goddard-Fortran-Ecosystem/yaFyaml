program main
   use yafyaml, only : newParser, AbstractNode
   implicit none

   type(newParser) p
   class(AbstractNode), allocatable :: c
   integer, allocatable :: nodes(:)

   p = newParser('core')
   c = p%load('integer-array.json')
   call c%get(nodes, 'nodes')

   if (any(nodes/=[1,2,3])) error stop "Test failed: wrong nodes values."

   print *,"Test passed"

end program
