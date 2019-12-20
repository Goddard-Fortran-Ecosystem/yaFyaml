module fy_Reader
  use fy_AbstractTextStream
  use fy_KeywordEnforcer
  use, intrinsic :: iso_c_binding, only: NL => C_NEW_LINE, C_NULL_CHAR, CR => C_CARRIAGE_RETURN
  use, intrinsic :: iso_c_binding, only: TAB => C_HORIZONTAL_TAB
  implicit none
  private

!!$  public :: Mark
  public :: Reader

!!$  type :: Mark
!!$     character(:), allocatable :: name
!!$     integer :: index
!!$     integer :: line
!!$     integer :: column
!!$     character(:), allocatable :: buffer
!!$     integer :: position
!!$  end type Mark


  type :: Reader
     private
     class(AbstractTextStream), allocatable :: stream
     character(:), allocatable :: raw_buffer
     character(:), allocatable :: buffer
     integer :: local_pos = 1  ! position in local buffer of current character
     integer :: global_pos = 1 ! global position
     integer :: stream_pos = 1 ! position in stream (read in chunks)

     integer :: line = 0
     integer :: column = 0
     integer :: index = 0
     logical :: eof = .false.
   contains
     procedure :: peek     ! return next character (or later) but keep pos
     procedure :: prefix   ! return next n characters but keep pos
     procedure :: forward  ! move pos forward
     procedure :: update   ! update internal buffer from stream
     procedure :: update_raw
     procedure :: is_eof

     procedure :: get_column
     procedure :: get_index
  end type Reader


  interface Reader
     module procedure new_Reader
  end interface Reader

contains

  function new_Reader(stream) result(r)
    type(Reader) :: r
    class(AbstractTextStream), intent(in) :: stream
    r%stream = stream
    r%raw_buffer = ''
    r%buffer = ''
  end function new_Reader
  
  
  function peek(this, unused, offset) result(buffer)
    character(1) :: buffer
    class(Reader), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    integer, optional, intent(in) :: offset

    integer :: offset_
    integer :: pos

    offset_ = 0
    if (present(offset))  offset_ = offset

    
    if (this%local_pos + offset_ +1 > len(this%buffer)) then
       call this%update(offset_ + 1)
    end if

    pos = this%local_pos + offset_
    buffer = this%buffer(pos:pos)

  end function peek

  function prefix(this, n) result(buffer)
    integer, intent(in) :: n
    character(:), allocatable :: buffer
    class(Reader), intent(inout) :: this

    integer :: pos
    integer :: last

    if (this%local_pos + n > len(this%buffer)) then
       call this%update(n)
    end if

    pos = this%local_pos + n-1
    last = len(this%buffer)
    buffer = this%buffer(this%local_pos:min(pos,last))

  end function prefix

  subroutine forward(this, unused, offset)
    class(Reader), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    integer, optional, intent(in) :: offset

    integer :: offset_
    integer :: k
    character(1) :: ch

    offset_ = 1
    if (present(offset)) offset_ = offset

    if (this%local_pos + offset_ + 1 >= len(this%buffer)) then
       call this%update(offset_+1)
    end if

    do k = 1, offset_
       associate(p => this%local_pos)
         ch = this%buffer(p:p)
         p = p + 1
         this%global_pos = this%global_pos + 1

         if (scan(ch, NL) /= 0) then
            this%line = this%line + 1
            this%column = 0
         elseif (p+1 <= len(this%buffer)) then
            if (ch == CR .and. this%buffer(p+1:p+1) /= NL) then
               this%line = this%line + 1
               this%column = 0
            end if
         else
            this%column = this%column + 1
         end if
       end associate
    end do

  end subroutine forward


  subroutine update(this, n_characters)
    class(Reader), intent(inout) :: this
    integer, intent(in) :: n_characters

    integer :: n_raw

    this%buffer = this%buffer(this%local_pos:)
    this%local_pos = 1

    do while (len(this%buffer) < n_characters)
       if (.not. this%is_eof()) then
          call this%update_raw()
       end if

       n_raw = len(this%raw_buffer)
       this%buffer = this%buffer // this%raw_buffer
       this%raw_buffer = '' ! this%raw_buffer(n_raw:)

       if (this%is_eof()) then
          this%buffer = this%buffer // C_NULL_CHAR
          this%raw_buffer = ''
          exit
       end if
    end do
    
  end subroutine update

  subroutine update_raw(this, unused, n)
    class(Reader), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    integer, optional, intent(in) ::n

    integer :: n_
    character(:), allocatable :: buffer

    n_ = 1024
    if (present(n)) n_ = n

    buffer = this%stream%read(n_)
    if (len(buffer) > 0) then
       this%raw_buffer = this%raw_buffer // buffer
       this%stream_pos = this%stream_pos + len(buffer)
    else
       this%eof = .true.
    end if
    
  end subroutine update_raw

  logical function is_eof(this) result(eof)
    class(Reader), intent(in) :: this
    eof = this%eof
  end function is_eof


  integer function get_column(this) result(column)
    class(Reader), intent(in) :: this

    column = this%column
  end function get_column

  integer function get_index(this) result(index)
    class(Reader), intent(in) :: this

    index = this%index
  end function get_index

end module fy_Reader
