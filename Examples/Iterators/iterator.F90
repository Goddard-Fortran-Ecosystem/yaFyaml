! This example does minimal error checking.  The point is to
! demonstrate how to use iterators.  In the absence of exceptions,
! error checking becomes verbose and obscures the main features.

program main
   use yafyaml
   use gFTL_IntegerVector
   use gFTL_StringIntegerMap
   implicit none

   type(Parser) :: p
   class(YAML_Node), allocatable :: node
   integer :: status

   p = Parser()
   node = p%load('iterator.yaml')
   write(10,'(dt)', iostat=status) node

   call optimistic(node)  ! no error code checking
   call pessimistic(node) ! with error code checking

contains
   
   ! This procedure does not check any potential errors
   ! but is easier to follow as a result.
   subroutine optimistic(node)
      class(YAML_Node), intent(in) :: node

      integer :: i
      integer :: prime
      class(YAML_Node), pointer :: subcfg, subsubcfg
      class(NodeIterator), allocatable :: iter
      character(:), pointer :: shape
      integer :: n_edges

      ! Iterating over a sequence
      subcfg => node%of('primes')
      do i = 1, subcfg%size()
         prime = subcfg%of(i)
         print*,'prime: ', prime
      end do
      
      ! Iterating over a mapping
      subcfg => node%of('shapes')
      associate (b => subcfg%begin(), e => subcfg%end())
        iter = b
        do while (iter /= e)
           shape => to_string(iter%first())
           subcfg => iter%second()
           call subcfg%get(n_edges,'num_edges')
           print*,'Shape: ', shape, ' has ', n_edges, 'sides.'
           call iter%next()
        end do
      end associate

   end subroutine optimistic

   
   ! This procedure carefully checks all return codes.
   subroutine pessimistic(node)
      class(YAML_Node), intent(in) :: node

      integer :: i
      integer :: prime
      class(YAML_Node), pointer :: subcfg, subsubcfg
      class(NodeIterator), allocatable :: iter
      character(:), pointer :: shape, key
      integer :: n_edges
      integer :: status, status_b, status_e
      logical :: found

      ! Iterating over a sequence
      subcfg => node%at('primes', rc=status)
      if (status /= YAFYAML_SUCCESS) return

      do i = 1, subcfg%size()
         prime = subcfg%at(i, rc=status)
         if (status /= YAFYAML_SUCCESS) return
         print*,'prime: ', prime
      end do
      
      ! Iterating over a mapping
      subcfg => node%at('shapes', rc=status)
      if (status /= YAFYAML_SUCCESS) return
      associate (b => subcfg%begin(rc=status_b), e => subcfg%end(rc=status_e))
        if (any([status_b, status_e] /= YAFYAML_SUCCESS)) then
           print*,'Cannot iterate on this node.'
        end if

        iter = b
        do while (iter /= e)
           shape => to_string(iter%first(), rc=status)
           if (status/= YAFYAML_SUCCESS) then
              print*,"failed to obtain string for key"
           end if

           subsubcfg => iter%second()
           if (subsubcfg%has('num_edges')) then
              call subsubcfg%get(n_edges, 'num_edges', rc=status)
              if (status /= YAFYAML_SUCCESS) return
              print*,'Shape: ', shape, ' has ', n_edges, 'sides.'
           else
              print*,'Shape: ', shape, 'num_edges not found.'
           end if
           call iter%next()
        end do
      end associate

   end subroutine pessimistic

end program main
