program main
   use yafyaml, only : newParser, AbstractNode, to_bool
   implicit none

   type(newParser) p
   class(AbstractNode), allocatable :: c
   logical :: science = .false.

   p = newParser('core')
   c = p%load('trivial.json')
   science = to_bool(c%at('science')) ! this should overwrite science with .true.

   if (.not. science) error stop "Test failed"

   print *,"Test passed"

end program
