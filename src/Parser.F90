!!! The Parser imports a sequence of tokens and constructs a
!!! configuration object.  I naively expect this to be rather simple
!!! compared to the Lexer, but reading suggests that it should be the
!!! opposite.  The difference may in part be that this package restricts
!!! keys to be simple strings.  

module fy_Parser
  use fy_Lexer
  use fy_Tokens
  use fy_Reader
  use fy_AbstractTextStream
  use fy_Configuration
  use gFTL_UnlimitedVector
  use gFTL_StringUnlimitedMap
  implicit none
  private

  public :: Parser
  public :: matches_logical
  public :: matches_integer
  public :: matches_real

  type :: Parser
     private
     character(:), allocatable :: schema
   contains
     procedure :: load
     procedure :: top
     procedure :: process_sequence
     procedure :: process_mapping
     procedure :: interpret
  end type Parser


  interface Parser
     module procedure new_Parser
  end interface Parser


contains

  function new_Parser() result(p)
    type(Parser) :: p

    p%schema = 'JSON'
  end function new_Parser

  function load(this, stream) result(cfg)
    type(AllocatableConfiguration) :: cfg
    class(Parser), intent(inout) :: this
    class(AbstractTextStream), intent(in) :: stream

    type(Lexer) :: lexr

    lexr = Lexer(Reader(stream))

    call this%top(cfg, lexr)

  end function load


  subroutine top(this, cfg, lexr)
    class(Parser), intent(in) :: this
    type(AllocatableConfiguration), intent(inout) :: cfg
    type(Lexer), intent(inout) :: lexr

    class(AbstractToken), allocatable :: token
    logical :: done
    class(*), pointer :: node

    done = .false.

    do
       token = lexr%get_token()
       print*,__FILE__,__LINE__,'id: ', token%get_id()
       select type (token)
       type is (StreamStartToken)
          print*,'stream start'
       type is (StreamEndToken)
          print*,'stream start'
          exit
       type is (DocumentStartToken)
          print*,'document start'
       type is (DocumentEndToken)
          print*,'document end'
          exit
       type is (ScalarToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(this%interpret(token))
          done = .true.
       type is (FlowSequenceStartToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(scalar=UnlimitedVector())
          call cfg%get_node(node)
          call this%process_sequence(node, lexr)
          done = .true.
       type is (BlockSequenceStartToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(scalar=UnlimitedVector())
          call cfg%get_node(node)
          call this%process_sequence(node, lexr)
          done = .true.
       type is (FlowMappingStartToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(scalar=StringUnlimitedMap())
          call cfg%get_node(node)
          call this%process_mapping(node, lexr)
          done = .true.
       class default
          error stop 'unsupported token type in top'
       end select
    end do

  end subroutine top


  recursive subroutine process_sequence(this, node, lexr)
    class(Parser), intent(in) :: this
    class(*), pointer, intent(in) :: node ! pointer association is not modified
    type(Lexer), intent(inout) :: lexr 

    class(AbstractToken), allocatable :: token
    logical :: expect_another
    type(AllocatableConfiguration) :: sub
    class(*), pointer :: sub_node

    expect_another = .false.
    
    select type (node)
    type is (UnlimitedVector)
       do
          token = lexr%get_token()
          print*,__FILE__,__LINE__,'id: ', token%get_id()


          select type (token)

          type is (ScalarToken)
             call node%push_back(this%interpret(token))

          type is (FlowNextEntryToken)
             print*,__FILE__,__LINE__,token%get_id()
             expect_another = .true.

          type is (BlockNextEntryToken)
             print*,__FILE__,__LINE__,token%get_id()
             expect_another = .true.

          type is (FlowSequenceEndToken)
             ! TODO must match block/flow 
!!$             if (expect_another) then
!!$                __ASSERT__("dangling comma in flow sequence", expect_another)
!!$             else
!!$                exit
!!$             end if
             exit
          type is (BlockEndToken)
             ! TODO must match block/flow 
             exit

          type is (FlowSequenceStartToken)
             sub = Configuration(UnlimitedVector())
             call node%push_back(sub)
             call this%process_sequence(node%back(), lexr)

          type is (FlowMappingStartToken)
             sub = Configuration(StringUnlimitedMap())
             call node%push_back(sub)
             call this%process_mapping(node%back(), lexr)

          class default
             error stop 'illegal token encountered'
          end select
          print*,__FILE__,__LINE__
          deallocate(token)
       end do
       print*,__FILE__,__LINE__
       
    class default
       error stop 'inconsistent state in parser'

    end select
    
  end subroutine process_sequence

     
  recursive subroutine process_mapping(this, node, lexr)
    class(Parser), intent(in) :: this
    class(*), pointer, intent(in) :: node
    type(Lexer), intent(inout) :: lexr

    class(AbstractToken), allocatable :: token
    logical :: expect_another
    type(AllocatableConfiguration) :: sub
    character(:), allocatable :: key
    class(AbstractToken), allocatable :: next_token

    expect_another = .false.
    
    select type (node)
    type is (StringUnlimitedMap)
       do
          token = lexr%get_token()
          print*,__FILE__,__LINE__,'id: ', token%get_id()

          select type (token)

          type is (KeyToken)
             next_token = lexr%get_token()
             select type(next_token)
             type is (ScalarToken)
                key = next_token%value ! always a string
             class default
                error stop
             end select
             next_token = lexr%get_token()
             select type(next_token)
             type is (ValueToken)
                ! mandatory before value
             class default
                error stop
             end select
             next_token = lexr%get_token()
             select type(next_token)
             type is (ScalarToken)
                call node%insert(key, this%interpret(next_token))
             type is (FlowSequenceStartToken)
                sub = Configuration(UnlimitedVector())
                call node%insert(key,sub)
                call this%process_sequence(node%at(key), lexr)

             type is (FlowMappingStartToken)
                sub = Configuration(StringUnlimitedMap())
                call node%insert(key,sub)
                call this%process_mapping(node%at(key), lexr)
             class default
                error stop
             end select

          type is (FlowNextEntryToken)
             expect_another = .true.

          type is (FlowMappingEndToken)
             exit
          class default
             error stop 'illegal token encountered'
          end select

       end do
       
    class default
       error stop 'inconsistent state in parser'

    end select
    
  end subroutine process_mapping

     

  function interpret(this, scalar) result(value)
    class(*), allocatable :: value
    class(Parser), intent(in) :: this
    type(ScalarToken) :: scalar

    character(:), allocatable :: text
    integer :: status

    text = scalar%value
    print*,__FILE__,__LINE__,'interpreting: <',text,'> ',this%schema
    select case (this%schema)
    case ('JSON')
          print*,'logical?'
       if (matches_logical(text,this%schema)) then
          print*,'logical'
          value = to_logical(text, this%schema)
          return
       end if
          print*,'integer?'
       if (matches_integer(text, this%schema)) then
          print*,'integer'
          value = to_integer(text, this%schema)
          return
       end if
       if (matches_real(text, this%schema)) then
          value = to_real(text, this%schema)
          return
       end if
       ! else is a string
       value = text
    end select
    
  end function interpret

  logical function matches_logical(text, schema)
    character(*), intent(in) :: text
    character(*), intent(in) :: schema

    select case (schema)
    case ('Core')
       select case (text)
       case ('true', 'True', 'TRUE')
          matches_logical = .true.
       case ('false', 'False', 'FALSE')
          matches_logical = .true.
       case default
          matches_logical = .false.
       end select
    case ('JSON')

       select case (text)
       case ('true', 'false')
          matches_logical = .true.
       case default
          matches_logical = .false.
       end select
    case default
       error stop 'schema not supported'
    end select
    
  end function matches_logical


  logical function to_logical(text, schema)
    character(*), intent(in) :: text
    character(*), intent(in) :: schema

    select case (schema)
    case ('JSON')
       select case (text)
       case ('true')
          to_logical = .true.
       case ('false')
          to_logical = .false.
       case default
          error stop 'not interpretable as logical'
       end select
    case default
       error stop 'schema not supported'
    end select
    
  end function to_logical

  
  logical function matches_integer(text, schema)
    character(*), intent(in) :: text
    character(*), intent(in) :: schema

    integer :: leading_digit
    
    select case (schema)
    case ('Core')
       matches_integer = (len(text) >= 1)
       if (.not. matches_integer) return

       if (verify(text(1:1),'-+') == 0) then
          matches_integer = (len(text) >= 2)
          if (.not. matches_integer) return
          leading_digit = 2
       else
          leading_digit = 1
       end if

       ! remaining chars are digits (can be multiple leading 0's)
       matches_integer = (verify(text(leading_digit:),'0123456789') == 0)

    case ('JSON')

       matches_integer = (len(text) >= 1)
       if (.not. matches_integer) return

       if (text(1:1) == '0') then
          matches_integer = (len(text) == 1)
          return
       elseif (text(1:1) == '-') then
          leading_digit = 2
       else
          leading_digit = 1
       end if

       matches_integer = (len(text) >= leading_digit)
       if (.not. matches_integer) return

       ! leading digit cannot be 0
       matches_integer = (scan(text(leading_digit:leading_digit),'123456789') > 0)
       if (.not. matches_integer) return
       matches_integer = (verify(text(leading_digit+1:),'0123456789') == 0)
          
    case default
       error stop 'schema not supported'
    end select
    
  end function matches_integer

  
  integer function to_integer(text, schema)
    character(*), intent(in) :: text
    character(*), intent(in) :: schema

    integer :: status
    select case (schema)
    case ('JSON')
       read(text,*, iostat=status) to_integer
       if (status /= 0) then
          error stop 'could not convert to integer'
       end if
    case default
       error stop 'schema not supported'
    end select
    
  end function to_integer

  
  ! From the YAML 1.2 spec: <https://yaml.org/spec/1.2/spec.html#id2803828>
  ! Float regexp is: -? ( 0 | [1-9] [0-9]* ) ( \. [0-9]* )? ( [eE] [-+]? [0-9]+ )?
  logical function matches_real(text, schema) result(matches)
    character(*), intent(in) :: text
    character(*), intent(in) :: schema

    integer :: sep
    integer :: e


    select case (schema)
    case ('JSON')

       ! special cases
       if (matches_inf(text) .or. matches_nan(text)) then
          matches = .true.
          return
       end if

       ! Reals must have a '.' and it must be preceded by at least one digit.
       sep = scan(text,'.')
       matches = (sep > 1)
       if (.not. matches) return

       print*,'sep: ', sep, text
       ! Position of start of exponent:
       e = scan(text, 'eE')
       print*,__LINE__,'e: ', e
       ! Exponent part must either not exist or it must be between the '.'
       ! and the end of text with at least one character after.
       matches = (e == 0 .or. (e > sep))
       if (.not. matches) return
       print*,__LINE__,'e: ', e

       matches = matches_whole(text(:sep-1))
       if (.not. matches) return

       if (e == 0) then
          print*,__LINE__,'e: ', e
          matches = matches_fraction(text(sep+1:))
       else
          matches = matches_fraction(text(sep+1:e-1))
          if (.not. matches) return
          matches = matches_exponent(text(e+1:))
       end if

    case default
       error stop 'schema not supported'
    end select

  contains

    logical function matches_inf(text)
      character(*), intent(in) :: text

      ! Possible leading sign
      select case (text)
      case ('inf','Inf','INF')
         matches_inf = .true.
      case default
         matches_inf = .false.
      end select
    end function matches_inf

    logical function matches_nan(text)
      character(*), intent(in) :: text

      select case (text)
      case ('nan','NaN','NAN')
         matches_nan = .true.
      case default
         matches_nan = .false.
      end select
    end function matches_nan


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

  real function to_real(text, schema)
    character(*), intent(in) :: text
    character(*), intent(in) :: schema

    integer :: status
    select case (schema)
    case ('JSON')
       read(text,*, iostat=status) to_real
       if (status /= 0) then
          error stop 'could not convert to real'
       end if
    case default
       error stop 'schema not supported'
    end select
    
  end function to_real

  


  
end module fy_Parser
