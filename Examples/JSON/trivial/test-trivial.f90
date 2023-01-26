program main
   use yafyaml
   use fy_CoreSchema
   implicit none

   type(Parser) p
   class(YAML_Node), allocatable :: c
   logical :: science = .false.
   integer :: rc

   p = Parser()
   c = p%load('trivial.json', rc=rc)
   science = c%at('science') ! this should overwrite science with .true.

   if (.not. science) error stop "Test failed"

   print *,"Test passed"

end program
