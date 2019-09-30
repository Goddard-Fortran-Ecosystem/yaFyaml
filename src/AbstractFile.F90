module fy_AbstractFile
  implicit none
  private

  public :: AbstractFile

  type, abstract :: AbstractFile
     private
   contains
     procedure(close), deferred :: close
     procedure(read_line), deferred :: read_line
     procedure(write_line), deferred :: write_line
     procedure(end_of_file), deferred :: end_of_file
     procedure(rewind), deferred :: rewind
  end type AbstractFile


  abstract interface

     function read_line(this) result(line)
       import AbstractFile
       character(:), allocatable :: line
       class(AbstractFile), intent(inout) :: this
     end function read_line

     subroutine write_line(this, line)
       import AbstractFile
       class(AbstractFile), intent(inout) :: this
       character(*), intent(in) :: line
     end subroutine write_line

     subroutine close(this, unused, delete)
       use fy_KeywordEnforcer
       import AbstractFile
       class(AbstractFile), intent(inout) :: this
       class(KeywordEnforcer), optional, intent(in) :: unused
       logical, optional :: delete
     end subroutine close

     logical function end_of_file(this)
       import AbstractFile
       class(AbstractFile), intent(in) :: this
     end function end_of_file

     subroutine rewind(this)
       import AbstractFile
       class(AbstractFile), intent(inout) :: this
     end subroutine rewind
  end interface


end module fy_AbstractFile
