module fy_StringStream
  use fy_AbstractTextStream
  implicit none
  private
  

  public :: StringStream

  type, extends(AbstractTextStream) :: StringStream
     private
     character(:), allocatable :: buffer
     integer :: idx
   contains
     procedure :: read
  end type StringStream

  interface StringStream
     module procedure new_StringStream
  end interface StringStream


contains


  function new_StringStream(buffer) result(stream)
    character(*), intent(in) :: buffer
    type(StringStream) :: stream

    stream%buffer = buffer
    stream%idx = 0

  end function new_StringStream
    

  function read(this, n_characters) result(buffer)
    character(:), allocatable :: buffer
    class(StringStream), intent(inout) :: this
    integer, intent(in) :: n_characters

    integer :: last_idx

    last_idx = min(this%idx + n_characters - 1, len(this%buffer))
    buffer = this%buffer(this%idx:last_idx)
    this%idx = last_idx + 1

  end function read


end module fy_StringStream
