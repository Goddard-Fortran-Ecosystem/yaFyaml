module fy_YAML_Parser
  use, intrinsic :: iso_fortran_env, only: REAL32
  use fy_Configuration
  use fy_AbstractFile
  use fy_OrderedStringUnlimitedMap
  use gFTL_StringVector
  implicit none
  private

  public :: YAML_Parser

  type :: YAML_Parser
     private
   contains
     procedure :: load
     procedure, nopass :: interpret
     procedure, nopass :: split
  end type YAML_Parser

contains

  subroutine load(this, file, config)
    class(YAML_Parser), intent(inout) :: this
    class(AbstractFile), intent(inout) :: file
    type(Configuration), intent(inout) :: config

    character(:), allocatable :: line
    integer :: n
    character(:), allocatable :: key
    character(:), allocatable :: value


    logical :: first_line
    type(OrderedStringUnlimitedMap), allocatable :: map
    character(:), allocatable :: token
    type(StringVector) :: tokens

    first_line = .true.
    do 
       line = file%read_line()
       if (first_line .and. trim(line) == '---') then ! skip
          first_line = .false.
          cycle
       end if

       tokens = this%split(line)
       token = tokens%at(1)
       n = len(token)
       if (token(n:n) == ':') then
          key = token(:n-1)
          value = tokens%at(2)
          allocate(map)
          call map%insert(key,this%interpret(value))
          config = Configuration(scalar=map)
          deallocate(map)
       else
          config = Configuration(scalar=this%interpret(line))

       endif
       if (file%end_of_file()) exit
    end do
    
  end subroutine load
  

  function interpret(token) result(value)
    class(*), allocatable :: value
    character(*), intent(in) :: token

    integer :: status
    integer :: i
    real(kind=REAL32) :: x
    
    select case (token)
    case ('n','N','no','No','NO','false','False','FALSE','off','Off','OFF')
       value = .false.
    case ('y','Y','yes','Yes','YES','true','True','TRUE','on','On','ON')
       value  = .true.
    case default
       read(token,*,iostat=status) i
       if (status == 0) then ! integer
          value = i
       else
          read(token,*,iostat=status) x
          if (status == 0) then ! integer
             value = x
          else
             value = token
          end if
       end if
    end select
  end function interpret


  function split(line) result(tokens)
    type (StringVector) :: tokens
    character(*), intent(in) :: line

    integer :: i, j
    integer :: n

    n = len(line)

    if (line == '') return ! no tokens
    
    ! i is now start of 1st token
    i = 1
    do
       ! skip leading spaces
       do
          if (i > n) return ! no more tokens
          if (line(i:i) == ' ') then
             i = i + 1
          else
             exit
          end if
       end do

       j = i
       do
          j = j + 1
          if (j > n) then
             call tokens%push_back(line(i:j-1))
             return
          end if
          if (line(j:j) == ' ') then
             call tokens%push_back(line(i:j-1))
             i = j + 1
             exit
          end if
       end do
    end do
    
  end function split

end module fy_YAML_Parser
