! This example does minimal error checking.  The point is to
! demonstrate how to use iterators.  In the absence of exceptions,
! error checking becomes verbose and obscures the main features.

program main
   use yafyaml
   use gFTL_IntegerVector
   use gFTL_StringIntegerMap
   implicit none

   type(Parser) :: p
   type(Configuration) :: config
   type(Configuration) :: subcfg, subsubcfg
   type(Configuration) :: shape_cfg

   type(MappingIterator) :: iter, iter2
   type(Sequence), pointer :: s

   integer :: status
   class(AbstractNode), pointer :: node
   integer :: prime
   character(:), allocatable :: shape, key
   integer :: n_edges
   integer :: i
   type(Mapping), pointer :: m

   p = Parser('core')
   config = p%load(FileStream('animal.yaml'))
   write(10,'(dt)', iostat=status) config

   config = p%load(FileStream('iterator.yaml'))
   write(10,'(dt)', iostat=status) config

   ! Iterating over a sequence
   subcfg = config%of('primes')
   do i = 1, subcfg%size()
      prime = subcfg%of(i)
      print*,'prime: ', prime
   end do
   print*,subcfg

   ! Iterating over a mapping
   subcfg = config%of('shapes')
   associate (b => subcfg%begin(), e => subcfg%end())
     iter = b
     do while (iter /= e)

        node => iter%first()
        shape = node
!!$        shape = iter%first() ! should return a subconfig
        node => iter%second()
        call node%get(n_edges, 'num_edges')
!!$        n_edges = node%of('num_edges')
        print*,'Shape: ', shape, ' has ', n_edges, 'sides.'
        call iter%next()
        
     end do
   end associate

   
   ! Outer loop over mapping
   m => to_mapping(config%node)
   associate(b => m%begin(), e=> m%end())
     iter = b
     do while (iter /= e)

        ! Access mapping values with key()/value() methods on iterator.
!!$        key = iter%first()
!!$        
!!$        select case (key)
!!$        case ('primes')
!!$           
!!$           ! We expect a yaml sequence here
!!$           node => iter%second()
!!$           
!!$           do i = 1, node%size()
!!$              prime = node%of(i)
!!$              print*,'another prime: ', prime
!!$           end do
!!$           
!!$        case ('shapes')
!!$           
!!$           ! We expect a yaml mapping here  (shape: #sides)
!!$           node => iter%second()
!!$           m => to_mapping(node)
!!$
!!$           associate (bm => m%begin(), be => m%end())
!!$             iter2 = bm
!!$             do while (iter2 /= be)
!!$                shape = iter2%first()
!!$                n_edges = node%of(shape,'num_edges')
!!$                print*,'shape: ',shape,' has ',n_edges, 'sides'
!!$                call iter2%next()
!!$             end do
!!$           end associate
!!$           
!!$        end select
        call iter%next()
     end do
   end associate

      

end program main
