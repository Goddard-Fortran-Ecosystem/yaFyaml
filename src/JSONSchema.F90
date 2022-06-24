#include "error_handling.h"
module fy_JSONSchema
  use fy_AbstractSchema
  use fy_ErrorCodes
  use fy_ErrorHandling
  use, intrinsic :: iso_fortran_env, only: REAL64, INT64

  implicit none
  private

  public :: JSONSchema

  type, extends(AbstractSchema) :: JSONSchema
     private
   contains
     procedure, nopass :: matches_null
     procedure, nopass :: matches_logical
     procedure, nopass :: matches_integer
     procedure, nopass :: matches_real

     procedure, nopass :: to_logical
     procedure, nopass :: to_integer
     procedure, nopass :: to_real
  end type JSONSchema

contains


  logical function matches_null(text) result(matches)
    character(*), intent(in) :: text

    matches = (text == 'null')

  end function matches_null


  logical function matches_logical(text) result(matches)
    character(*), intent(in) :: text
    select case (text)
    case ('true', 'false')
       matches = .true.
    case default
       matches = .false.
    end select
  end function matches_logical
  

  ! Matches: 0 | -? [1-9] [0-9]*
  logical function matches_integer(text) result(matches)
    character(*), intent(in) :: text

    integer :: first
    
    matches = (len(text) >= 1)
    if (.not. matches) return
    
    if (text(1:1) == '0') then
       matches = (len(text) == 1)
       return
    elseif (text(1:1) == '-') then
       first = 2
    else
       first = 1
    end if

    matches = (len(text) >= first)
    if (.not. matches) return

    ! leading digit cannot be 0
    matches = (scan(text(first:first),'123456789') > 0)
    if (.not. matches) return
    matches = (verify(text(first+1:),'0123456789') == 0)
          
  end function matches_integer
  

  ! Matches: -? ( 0 | [1-9] [0-9]* ) ( \. [0-9]* )? ( [eE] [-+]? [0-9]+ )?
  logical function matches_real(text) result(matches)
    character(*), intent(in) :: text

    ! Position markers
    integer :: sep  ! '.'
    integer :: e    ! e | E

    ! Reals must have a '.' and it must be preceded by at least one digit.
    sep = scan(text,'.')
    matches = (sep > 1)
    if (.not. matches) return
    
    ! Position of start of exponent:
    e = scan(text, 'eE')

    ! Exponent part must either not exist or it must be between the '.'
    ! and the end of text with at least one character after.
    matches = (e == 0 .or. (e > sep))
    if (.not. matches) return
    
    matches = matches_whole(text(:sep-1))
    if (.not. matches) return
    
    if (e == 0) then
       matches = matches_fraction(text(sep+1:))
    else
       matches = matches_fraction(text(sep+1:e-1))
       if (.not. matches) return
       matches = matches_exponent(text(e+1:))
    end if

  contains
    
    ! This procedure checks the floor part or the exponent part.  The
    ! diference is that '+' is allowed in the sign of the exponent
    ! part.
   
    logical function matches_whole(text) result(matches)
      character(*), intent(in) :: text


      integer :: first_digit 
      ! text is guaranteed to have at least one character from above.
      ! if leading character is '-',  must have more chars
      if (verify(text(1:1), '-') == 0) then
         first_digit = 2
         matches = (len(text) > 1)
         if (.not. matches) return
      else
         first_digit = 1
      end if

      ! If leading digit is 0, then any other digits must be in the
      ! fraction.
      if (text(first_digit:first_digit) == '0') then
         matches = (len(text) == first_digit)
         if (.not. matches) return
      else
         ! remaining characters must be digits
         matches = (verify(text(first_digit:),'0123456789') == 0)
      end if

    end function matches_whole

    logical function matches_fraction(text) result(matches)
      character(*), intent(in) :: text

      ! Easy - must consist of only digits
      matches = (verify(text,'0123456789') == 0)

    end function matches_fraction

    ! Like matches_whole(), except sign char can be "+" and
    ! Multiple leading 0's are permitted
    logical function matches_exponent(text) result(matches)
      character(*), intent(in) :: text

      integer :: first_digit

      ! text is guaranteed to have at least one character from above.
      ! if leading character is '-+',  must have more chars
      if (verify(text(1:1), '-+') == 0) then
         first_digit = 2
         matches = (len(text) > 1)
         if (.not. matches) return
      else
         first_digit = 1
      end if

      ! remaining chars must be digits
      matches = (verify(text(first_digit:),'0123456789') == 0)
      if (.not. matches) return

    end function matches_exponent

    
  end function matches_real
  

  logical function to_logical(text,rc)
    character(*), intent(in) :: text
    integer, optional, intent(out) :: rc

    select case (text)
    case ('true')
       to_logical = .true.
    case ('false')
       to_logical = .false.
    end select
    __RETURN__(YAFYAML_SUCCESS)   

  end function to_logical

  integer(kind=INT64) function to_integer(text,rc)
    character(*), intent(in) :: text
    integer, optional, intent(out) :: rc

    integer :: status

    read(text,*, iostat=status)
    __VERIFY__(status)
    __RETURN__(YAFYAML_SUCCESS)   
  end function to_integer


  real(kind=REAL64) function to_real(text,rc)
    character(*), intent(in) :: text
    integer, optional, intent(out) :: rc

    integer :: status

    read(text,*, iostat=status) 
    __VERIFY__(status)
    __RETURN__(YAFYAML_SUCCESS) 
  end function to_real

end module fy_JSONSchema

  
