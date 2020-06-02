program main
   use yafyaml, only : Parser, Configuration, FileStream
   implicit none

   type(Parser) p
   type(Configuration) c
   logical :: fauxnews=.true.

   p = Parser('core')
   c = p%load(FileStream('trivial.json'))
   fauxnews = c%at('fauxnews') ! this should overwrite fauxnews with .false.

   if (fauxnews) error stop "Test failed"

   sync all
   if (this_image()==1) print *,"Test passed"
end program
