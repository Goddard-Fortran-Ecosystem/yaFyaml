#include "unused_dummy.fh"

module fy_File
  use fy_AbstractFile
  use fy_KeywordEnforcer
  use, intrinsic :: iso_fortran_env, only: IOSTAT_EOR
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  private

  public :: File

  type, extends(AbstractFile) :: File
     private
     integer :: unit
     character(1) :: mode
   contains
     procedure :: close
     procedure :: read_line
     procedure :: write_line
     procedure :: end_of_file
     procedure :: rewind
  end type File


  interface File
     module procedure new_file
  end interface File

contains

  function new_File(filename, mode) result(f)
    type(File) :: f
    character(len=*), intent(in) :: filename
    character(len=1), intent(in) :: mode

    select case (mode)
    case ('w','W')
       open(file=filename,newunit=f%unit,status='new',form='formatted')
    case ('r','R')
       open(file=filename,newunit=f%unit,status='old',form='formatted')
    case default
       ! error
    end select

  end function new_File


  subroutine close(this, unused, delete)
    class(File), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    logical, optional :: delete

    logical :: delete_

    _UNUSED_DUMMY(unused)

    delete_ = .false.
    if (present(delete)) delete = delete

    if (delete_) then
       close(this%unit, status='delete')
    else
       close(this%unit)
    end if
  end subroutine close


  function read_line(this) result(line)
    character(:), allocatable :: line
    class(File), intent(inout) :: this

    integer :: n
    integer :: status
    character(80) :: buffer

    line = ''
    do
       read(this%unit,'(80a)',size=n,iostat=status,advance='no') buffer
       line = line // buffer(1:n)
       if (status == IOSTAT_EOR) exit
    end do
  end function read_line

  subroutine write_line(this, line)
    class(File), intent(inout) :: this
    character(*), intent(in) :: line

    write(this%unit,'(a)') line
    
  end subroutine write_line

  logical function end_of_file(this)
    class(File),intent(in) :: this

    character(1) :: char
    integer :: status
    
    read(this%unit,'(1a)',iostat=status,advance='no') char
    end_of_file = (status == IOSTAT_END)
  end function end_of_file

  subroutine rewind(this)
    class(File),intent(inout) :: this
    rewind(this%unit)
  end subroutine rewind

end module fy_File
  
