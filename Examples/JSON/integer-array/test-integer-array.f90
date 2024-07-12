program main
   use yafyaml
   implicit none

   class(YAML_Node), allocatable :: c
   integer, allocatable :: nodes(:)
   integer :: rc

   call load(c, 'integer-array.json', rc=rc)
   call c%get(nodes, 'nodes')

   if (any(nodes/=[1,2,3])) error stop "Test failed: wrong nodes values."

   print *,"Test passed"

end program
