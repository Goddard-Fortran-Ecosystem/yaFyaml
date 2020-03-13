! This example does minimal error checking.  The point is to
! demonstrate how to use iterators.  In the absence of exceptions,
! error checking becomes verbose and obscures the main features.

program main
   use yafyaml
   use gFTL_IntegerVector
   use gftl_StringIntegerMap
   implicit none

   type(Parser) :: p
   type(Configuration) :: config
   type(Configuration) :: subcfg
   type(Configuration) :: shape_cfg

   type(ConfigurationIterator) :: iter, iter2

   integer :: status
   character(:), pointer :: key
   integer :: prime
   character(:), pointer :: shape
   integer :: n_edges
   

   p = Parser('core')
   config = p%load(FileStream('iterator.yaml'))

   ! Outer loop over mapping
   iter = config%begin()
   do while (iter /= config%end())
      ! Access mapping values with key()/value() methods on iterator.
      key => iter%key()
      subcfg = iter%value()

      select case (key)
      case ('primes')

         ! loop over primes (a yaml sequence)
         iter2 = subcfg%begin()
         do while (iter2 /= subcfg%end())
            ! Access sequence values with get() method on iterator
            prime = iter2%get() ! cast as integer
            print*,'another prime: ', prime
            call iter2%next()
         end do

      case ('shapes')

         ! loop over shapes (a yaml mapping)
         iter2 = subcfg%begin()
         do while (iter2 /= subcfg%end())
            shape => iter2%key()
            shape_cfg = iter2%value()
            n_edges = shape_cfg%at('num_edges')
            print*,'shape: ',shape,' has ',n_edges, 'sides'
            call iter2%next()
         end do

      end select

      
      call iter%next()
   end do

end program main
