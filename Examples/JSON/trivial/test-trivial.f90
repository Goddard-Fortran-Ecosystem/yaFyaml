program main
   use yafyaml, only : newParser, AbstractNode, assignment(=)
   implicit none

   type(newParser) p
   class(AbstractNode), allocatable :: c
   logical :: science = .false.

   p = newParser('core')
   c = p%load('trivial.json')
   science = c%at('science') ! this should overwrite science with .true.

   if (.not. science) error stop "Test failed"

   print *,"Test passed"

end program
