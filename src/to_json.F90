program main
   use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
   use yafyaml

   type(Parser) :: p
   type(Configuration) :: cfg

   character(:), allocatable :: filename
   integer :: n
   integer :: status

   
   call get_command_argument(1, length=n, status=status)
   allocate(character(len=n) :: filename)
   call get_command_argument(1, value=filename, status=status)

   p = Parser()
   cfg = p%load(FileStream(filename))

   write(OUTPUT_UNIT,'(DT)') cfg

end program main
    
