module fy_FileStream
  use fy_AbstractTextStream
  use, intrinsic :: iso_fortran_env, only: IOSTAT_EOR
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  private

  public :: FileStream

  type, extends(AbstractTextStream) :: FileStream
     private
     integer :: unit
   contains
     procedure :: read
     procedure :: close
  end type FileStream


  interface FileStream
     module procedure new_FileStream
  end interface FileStream


contains


  function new_FileStream(filename) result(file)
    character(*), intent(in) :: filename
    type(FileStream) :: file

    open(file=filename, newunit= file%unit, access='stream', form='unformatted', status='old')

  end function new_FileStream


  function read(this, n_characters) result(buffer)
    character(:), allocatable :: buffer
    class(FileStream), intent(inout) :: this
    integer, intent(in) :: n_characters

    logical :: eof
    integer :: status
    character(:), allocatable :: tmp
    character(1) :: char
    integer :: i

    allocate(character(n_characters) :: tmp)
    status = 0
    i = 0
    print*,'enter'
    do 

       if (status == IOSTAT_END .or. i == n_characters) then
          buffer = tmp(1:i)
          print*,'return: ', i, buffer
          return
       end if

       read(this%unit,iostat=status) char
       if (status == 0 .or. status == IOSTAT_EOR) then
          i = i + 1
          if (status == 0) then
             tmp(i:i) = char
          else
             tmp(i:i) = new_line('a')
          end if
       end if

    end do
       
    
  end function read

  subroutine close(this)
    class(FileStream), intent(inout) :: this

    close(this%unit)
  end subroutine close
  



end module fy_FileStream
