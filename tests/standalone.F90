program standalone
   use funit
   use yafyaml
   implicit none
   
   call test_single_scalar()
   call test_simple_anchor()

contains

   subroutine test_single_scalar()
      type(Parser) :: p
      class(YAML_Node), allocatable :: cfg
      character(:), allocatable :: scalar

      p = Parser()
      cfg = p%load(EscapedTextStream("--- a\n..."))

      scalar = to_string(cfg)

#ifdef __GFORTRAN__
#else
      call assert_that(scalar, is("a"))
#endif
      
   end subroutine test_single_scalar

   subroutine test_simple_anchor()
      type(Parser) :: p
      class(YAML_Node), allocatable :: cfg

      integer :: i_a, i_b

      p = Parser()
      cfg = p%load(EscapedTextStream( &
           & "---\n" // &
           & " A: &anchor \n" // &
           & "    i: 1 \n" // &
           & " B: *anchor \n" // &
           & "..."))

      i_a = to_int(cfg%at('A', 'i'))
      call assert_that(i_a, is(equal_to(1)))
      
      i_b = to_int(cfg%at('B', 'i'))
      call assert_that(i_b, is(equal_to(1)))

   end subroutine test_simple_anchor

end program standalone
