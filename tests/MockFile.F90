module MockFile_mod
  use yaFyaml
  use fy_KeywordEnforcer
  use gFTL_StringVector
  use fy_AbstractFile
  implicit none
  private

  public :: MockFile

  type, extends(AbstractFile) :: MockFile
     type(StringVector) :: lines
     integer :: current_line = 0
   contains
     procedure :: close
     procedure :: read_line
     procedure :: write_line
     procedure :: end_of_file
     procedure :: rewind
  end type MockFile

  interface MockFile
     module procedure new_MockFile
  end interface MockFile

contains

  function new_MockFile() result(f)
    type(MockFile) :: f
    f%current_line = 0
  end function new_MockFile

  subroutine close(this, unused, delete)
    class(MockFile), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    logical, optional :: delete
    call this%lines%clear()
  end subroutine close

  function read_line(this) result(line)
    class(MockFile), intent(inout) :: this
    character(:), allocatable :: line
    this%current_line = this%current_line + 1
    line = this%lines%at(this%current_line)
  end function read_line

  subroutine write_line(this, line)
    class(MockFile), intent(inout) :: this
    character(*), intent(in) :: line

    call this%lines%push_back(line)
    this%current_line = this%current_line + 1
  end subroutine write_line

  logical function end_of_file(this)
    class(MockFile), intent(in) :: this

    end_of_file = (this%current_line ==this%lines%size())
  end function end_of_file

  subroutine rewind(this)
    class(MockFile), intent(inout) :: this

    this%current_line = 0
  end subroutine rewind
    
end module MockFile_mod
