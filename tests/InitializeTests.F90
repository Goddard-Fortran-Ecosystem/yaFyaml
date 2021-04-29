module InitializeTests_mod
   use fy_ErrorHandling

contains

   subroutine Initialize()
   
      call set_throw_method(throw_to_pfunit)

   end subroutine Initialize


   subroutine throw_to_pfunit(file_name, line_number, message)
      use funit, only: SourceLocation
      use funit, only: funit_throw => throw
      character(len=*), intent(in) :: file_name
      integer, intent(in) :: line_number
      character(len=*), intent(in) :: message

      call funit_throw(message, SourceLocation(file_name, line_number))

   end subroutine throw_to_pfunit
   
   
end module InitializeTests_mod
