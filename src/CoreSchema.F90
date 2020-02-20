module fy_CoreSchema
  use fy_AbstractSchema
  implicit none
  private

  public :: CoreSchema

  type, extends(AbstractSchema) :: CoreSchema
     private
   contains
     ! analyzers
     procedure, nopass :: matches_null
     procedure, nopass :: matches_logical
     procedure, nopass :: matches_integer
     procedure, nopass :: matches_real

     ! converters
     procedure, nopass :: to_logical
     procedure, nopass :: to_integer
     procedure, nopass :: to_real

  end type CoreSchema

contains


  logical function matches_null(text) result(matches)
    character(*), intent(in) :: text

    select case (text)
    case ('null', 'Null', 'NULL')
       matches = .true.
    case default
       matches = .false.
    end select
  end function matches_null


  logical function matches_logical(text) result(matches)
    character(*), intent(in) :: text

    select case (text)
    case ('true', 'True', 'TRUE', 'false', 'False', 'FALSE')
       matches = .true.
    case default
       matches = .false.
    end select

  end function matches_logical
  

  
  ! Base 10:    [-+]? [0-9]+
  ! Octal:      0o [0-7]+
  ! Hex:        0x [0-9a-fA-F]+
  logical function matches_integer(text) result(matches)
    character(*), intent(in) :: text

    integer :: first
    
    matches = (len(text) >= 1)
    if (.not. matches) return

    if (text(1:1) == '0') then
       if (len(text) > 1) then
          if (text(2:2) == 'o') then
             matches = matches_octal(text)
             return
          elseif (text(2:2) == 'x') then
             matches = matches_hex(text)
             return
          end if
       end if
    end if

    ! Base 10
    if (verify(text(1:1), '-+') == 0) then
       matches = (len(text) > 1)
       if (.not. matches) return
       first = 2
    else
       first = 1
    end if

    matches = (verify(text(first:),'0123456789') == 0)
    return

  contains

    logical function matches_octal(text) result(matches)
      character(*), intent(in) :: text


      matches = (len(text) > 2)
      if (.not. matches) return

      matches = (text(1:2) == '0o')
      if (.not. matches) return

      matches = (verify(text(3:),'01234567') == 0)
    end function matches_octal
      

    logical function matches_hex(text) result(matches)
      character(*), intent(in) :: text

      matches = (len(text) > 2)
      if (.not. matches) return

      matches = (text(1:2) == '0x')
      if (.not. matches) return

      matches = (verify(text(3:),'0123456789abcdefABCDEF') == 0)
    end function matches_hex
      
      
  end function matches_integer
  

  ! Float:        [-+]? ( \. [0-9]+ | [0-9]+ ( \. [0-9]* )? ) ( [eE] [-+]? [0-9]+ )?
  ! Infinity:     [-+]? ( \.inf | \.Inf | \.INF )
  ! Not a Number:  \.nan | \.NaN | \.NAN
  logical function matches_real(text) result(matches)
    character(*), intent(in) :: text

    ! Position markers
    integer :: sep  ! '.'
    integer :: e    ! e | E

    
    ! special cases
    if (matches_inf(text) .or. matches_nan(text)) then
       matches = .true.
       return
    end if
    
    ! Reals must have a '.' and it must be preceded by at least one digit.
    sep = scan(text,'.')
    matches = (sep > 0)
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
    
    logical function matches_inf(text) result(matches)
      character(*), intent(in) :: text

      integer :: first

      matches = (len(text) > 0)
      if (.not. matches) return

      ! Sign char:
      if (verify(text(1:1),'-+') == 0) then
         first = 2
      else
         first = 1
      end if

      select case (text(first:))
      case ('.inf','.Inf','.INF')
         matches = .true.
      case default
         matches = .false.
      end select
    end function matches_inf

    logical function matches_nan(text) result(matches)
      character(*), intent(in) :: text

      integer :: first

      matches = (len(text) > 0)
      if (.not. matches) return

      ! Sign char:
      if (verify(text(1:1),'-+') == 0) then
         first = 2
      else
         first = 1
      end if

      select case (text(first:))
      case ('.nan','.NaN','.NAN')
         matches = .true.
      case default
         matches = .false.
      end select
    end function matches_nan


    ! This procedure checks the floor part or the exponent part.  The
    ! diference is that '+' is allowed in the sign of the exponent
    ! part.
   
    logical function matches_whole(text) result(matches)
      character(*), intent(in) :: text

      integer :: first 

      if (len(text) == 0) then
         matches = .true.
         return
      end if

      ! Leading character can be '-' or '+'
      if (verify(text(1:1), '-+') == 0) then
         first = 2
      else
         first = 1
      end if

      ! remaining chars must be digits
      matches = (verify(text(first:),'0123456789') == 0)


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

  logical function to_logical(text)
    character(*), intent(in) :: text

    select case (text)
    case ('true', 'True', 'TRUE')
       to_logical = .true.
    case ('false', 'False', 'FALSE')
       to_logical = .false.
    end select

  end function to_logical

  integer function to_integer(text)
    character(*), intent(in) :: text

    integer :: status
    read(text,*, iostat=status) to_integer
    if (status /= 0) then
       error stop 'could not convert to integer'
    end if
    
  end function to_integer


  real function to_real(text)
    character(*), intent(in) :: text

    integer :: status
    read(text,*, iostat=status) to_real
    if (status /= 0) then
       error stop 'could not convert to real'
    end if
    
  end function to_real


end module fy_CoreSchema

  
