program main
   use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
   use yaFyaml

   type(newParser) :: p
   class(AbstractNode), allocatable :: node

   character(:), allocatable :: filename
   integer :: n
   integer :: status

   
   call get_command_argument(1, length=n, status=status)
   allocate(character(len=n) :: filename)
   call get_command_argument(1, value=filename, status=status)

   p = newParser()
   node = p%load(FileStream(filename))

   write(OUTPUT_UNIT,'(DT)') node

end program main
    
