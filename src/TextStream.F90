module fy_TextStream
  use fy_AbstractTextStream
  implicit none
  private
  

  public :: TextStream

  type, extends(AbstractTextStream) :: TextStream
     private
     character(:), allocatable :: buffer
     integer :: idx = 1
   contains
     procedure :: read ! read n characters from base stream
  end type TextStream

  interface TextStream
     module procedure new_TextStream
  end interface TextStream


contains


  function new_TextStream(buffer) result(stream)
    character(*), intent(in) :: buffer
    type(TextStream) :: stream

    stream%buffer = buffer
    stream%idx = 1

  end function new_TextStream

  function read(this, n_characters) result(buffer)
    character(:), allocatable :: buffer
    class(TextStream), intent(inout) :: this
    integer, intent(in) :: n_characters

    integer :: last_idx
    integer :: i

    last_idx = min(this%idx + n_characters, len(this%buffer))
    buffer = this%buffer(this%idx:last_idx)

    this%idx = last_idx + 1

  end function read


end module fy_TextStream
