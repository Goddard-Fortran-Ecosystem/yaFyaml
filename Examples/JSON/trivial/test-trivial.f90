program main
   use yafyaml, only : Parser, Configuration, FileStream
   implicit none

   type(Parser) p
   type(Configuration) c
   logical :: science=.false.

   p = Parser('core')
   c = p%load(FileStream('trivial.json'))
   science = c%at('science') ! this should overwrite science with .true.

   if (.not. science) error stop "Test failed"

!   sync all
   if (this_image()==1) print *,"Test passed"
end program
