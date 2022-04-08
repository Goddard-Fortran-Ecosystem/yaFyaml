program main
   use yafyaml
   implicit none
   
   type(Parser) :: p
   class(AbstractNode), allocatable :: node
   integer :: prime
   integer :: status
   
   p = Parser('core')
   node = p%load(FileStream('trivial.yaml'))

   prime = node%of('prime')
   prime = node%at('prime',rc=status)

   if (prime == 17) then
      print*,'success'
   else
      print*,'failure;  expected 17 but found ', prime
   end if
end program main
