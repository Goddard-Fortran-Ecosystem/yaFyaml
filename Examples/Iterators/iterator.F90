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
   integer :: status

   p = Parser('core')
   config = p%load(FileStream('iterator.yaml'))
   write(10,'(dt)', iostat=status) config

   call optimistic(config)
   call pessimistic(config)

contains
   
   ! This procedure does not check any potential errors
   ! but is easier to follow as a result.
   subroutine optimistic(config)
      type(Configuration), intent(in) :: config

      integer :: i
      integer :: prime
      type(Configuration) :: subcfg
      type(MappingIterator) :: iter
      class(AbstractNode), pointer :: node
      character(:), allocatable :: shape, key
      integer :: n_edges

      ! Iterating over a sequence
      subcfg = config%of('primes')
      do i = 1, subcfg%size()
         prime = subcfg%of(i)
         print*,'prime: ', prime
      end do
      
      ! Iterating over a mapping
      subcfg = config%of('shapes')
      associate (b => subcfg%begin(), e => subcfg%end())
        iter = b
        do while (iter /= e)
           shape = iter%first() ! key
           node => iter%second() ! value
           call node%get(n_edges, 'num_edges')
           print*,'Shape: ', shape, ' has ', n_edges, 'sides.'
           call iter%next()
        end do
      end associate
   end subroutine optimistic

   
   ! This procedure carefully checks all return codes.
   subroutine pessimistic(config)
      type(Configuration), intent(in) :: config

      integer :: i
      integer :: prime
      type(Configuration) :: subcfg
      type(MappingIterator) :: iter
      class(AbstractNode), pointer :: node
      character(:), allocatable :: shape, key
      integer :: n_edges
      integer :: status

      ! Iterating over a sequence
      subcfg = config%at('primes', rc=status)
      if (status /= YAFYAML_SUCCESS) return

      
      do i = 1, subcfg%size()
         prime = subcfg%at(i, rc=status)
         if (status /= YAFYAML_SUCCESS) return
         print*,'prime: ', prime
      end do
      
      ! Iterating over a mapping
      subcfg = config%at('shapes', rc=status)
      if (status /= YAFYAML_SUCCESS) return
      associate (b => subcfg%begin(), e => subcfg%end())
        iter = b
        do while (iter /= e)
           shape = iter%first() ! key
           node => iter%second() ! value
           call node%get(n_edges, 'num_edges', rc=status)
           if (status /= YAFYAML_SUCCESS) return
           print*,'Shape: ', shape, ' has ', n_edges, 'sides.'
           call iter%next()
        end do
      end associate

   end subroutine pessimistic

end program main
