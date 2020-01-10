module fy_EscapedTextStream
  use fy_AbstractTextStream
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR              ! "\0"
  use, intrinsic :: iso_c_binding, only: NL => C_NEW_LINE         ! "\n"
  use, intrinsic :: iso_c_binding, only: CR => C_CARRIAGE_RETURN  ! "\r"
  use, intrinsic :: iso_c_binding, only: TAB => C_HORIZONTAL_TAB  ! "\t"
  implicit none
  private


  public :: EscapedTextStream

  type, extends(AbstractTextStream) :: EscapedTextStream
     private
     character(:), allocatable :: buffer
     integer :: idx = 1
   contains
     procedure :: read ! read n characters from base stream
  end type EscapedTextStream

  interface EscapedTextStream
     module procedure new_EscapedTextStream
  end interface EscapedTextStream

  character(*), parameter :: BACKSLASH_ = "\\\\"
  character(*), parameter :: BACKSLASH = BACKSLASH_(1:1)

contains


  function new_EscapedTextStream(buffer) result(stream)
    character(*), intent(in) :: buffer
    type(EscapedTextStream) :: stream

    stream%buffer = buffer
    stream%idx = 1

  end function new_EscapedTextStream

  function read(this, n_characters) result(buffer)
    character(:), allocatable :: buffer
    class(EscapedTextStream), intent(inout) :: this
    integer, intent(in) :: n_characters

    character :: ch, next_ch

    buffer = ''
    associate (idx => this%idx)

      do while (idx <= len(this%buffer))
         ch = this%buffer(idx:idx)
         if (ch == BACKSLASH) then
            if (idx == len(this%buffer)) then
               buffer = buffer // BACKSLASH
               return
            else
               idx = idx + 1
               next_ch = this%buffer(idx:idx)
               select case(next_ch)
               case ('n')
                  buffer = buffer // NL
               case ('t')
                  buffer = buffer // TAB
               case ('r')
                  buffer = buffer // CR
               end select
            end if
         else
            buffer = buffer // ch
         end if
         
         idx = idx + 1
      end do
    end associate

  end function read


end module fy_EscapedTextStream
