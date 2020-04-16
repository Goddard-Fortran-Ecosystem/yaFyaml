PROGRAM Simple2

   USE yaFyaml
   USE gFTL_IntegerVector
   USE gftl_StringIntegerMap

   IMPLICIT NONE

   TYPE(Parser)              :: p
   TYPE(Configuration)       :: config
   character(:), allocatable :: name

   ! Define the parser object and load the file
   p = Parser('core')
   config = p%load(FileStream('simple2.yaml'))

   !--------------------------------------------------------
   ! Test reading a string
   !--------------------------------------------------------

   ! Deallocate if allocated for safety's sake
   IF ( ALLOCATED( name ) ) DEALLOCATE( name )

   ! Read the name from the YAML file
   name = config%at( 'name' )
   print*, '### name is: ', name

   ! Deallocate again
   IF ( ALLOCATED( name ) ) DEALLOCATE( name )

END PROGRAM Simple2
