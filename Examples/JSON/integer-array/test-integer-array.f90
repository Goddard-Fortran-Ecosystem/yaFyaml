program main
   use yafyaml, only : Parser, Configuration, FileStream
   implicit none

   type(Parser) p
   type(Configuration) c
   integer, allocatable :: nodes(:)

   p = Parser('core')
   c = p%load(FileStream('integer-array.json'))
   nodes = c%at('nodes')

   if (any(nodes/=[1,2,3])) error stop "Test failed: wrong nodes values."

   sync all
   if (this_image()==1) print *,"Test passed"
end program
