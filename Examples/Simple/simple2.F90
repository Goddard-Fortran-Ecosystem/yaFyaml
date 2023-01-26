program Simple2

   use yaFyaml
   use gFTL_IntegerVector
   use gftl_StringIntegerMap
   
   implicit none

   type(Parser)           :: p
   class(YAML_Node), allocatable :: node
   character(:), allocatable :: name
   
   ! Define the parser object and load the file
   p = Parser('core')
   node = p%load(FileStream('simple2.yaml'))

   !--------------------------------------------------------
   ! Test reading a string
   !--------------------------------------------------------

   ! Deallocate if allocated for safety's sake
   if ( allocated( name ) ) deallocate( name )

   ! Read the name from the YAML file
   name = node%at( 'name' )
   print*, '### name is: ', name

   ! Deallocate again
   if ( allocated( name ) ) deallocate( name )

end program Simple2
