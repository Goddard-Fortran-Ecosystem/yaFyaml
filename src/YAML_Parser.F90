module fy_YAML_Parser
  use fy_Configuration
  use fy_AbstractFile
  implicit none
  private

  public :: YAML_Parser

  type :: YAML_Parser
     private
   contains
     procedure :: load
  end type YAML_Parser

contains

  subroutine load(this, file, config)
    class(YAML_Parser), intent(inout) :: this
    class(AbstractFile), intent(inout) :: file
    type(Configuration), intent(inout) :: config

    character(:), allocatable :: line
    integer :: i

    line = file%read_line()
    read(line,'(i10)') i
    
    config = Configuration(node=i)


  end subroutine load
  

end module fy_YAML_Parser
