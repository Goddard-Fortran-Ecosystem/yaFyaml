program main
   use yafyaml, only : Parser, YAML_Node, assignment(=)
   implicit none

   type(Parser) p
   class(YAML_Node), allocatable :: c
   logical :: science = .false.

   p = Parser('core')
   c = p%load('trivial.json')
   science = c%at('science') ! this should overwrite science with .true.

   if (.not. science) error stop "Test failed"

   print *,"Test passed"

end program
