module fy_Scanner
  implicit none
  private

  public :: Scanner

  type :: Scanner
     private
     class (AbstractStream), allocatable :: stream
     integer :: current_indentation = -1
   contains
     procedure :: scan_to_next_token
  end type Scanner

contains


  subroutine scan_to_next_token(this)
    class(Scanner), intent(inout) :: this

    if (this%index == 0) then
       call this%stream%forward()
    end if
    found = .false.
    do while (.not. found)
       ! Skip spaces
       do while (self.stream%peek() == ' ')
          self.stream%forward()
       end do
       if (self.peek() == '#') then
          do while (scan(self.peek(), NULL // CARRIAGE_RETURN // NEW_LINE) == 0)
             self.forward()
          end do
       end if
       if (self.scan_line_break()) then
          this%allow_simple_kepy = .true.
       end if
    end do

  end subroutine scan_to_next_token

end module fy_Scanner
