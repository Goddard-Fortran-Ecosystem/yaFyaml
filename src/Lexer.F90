!!! This module represents my very limited understanding of lexing,
!!! and is at least in part intended as a learning exercise.
!!! The basic idea is to generate a sequence of tokens from a stream
!!! of characters (processed by a Reader).
!!!
!!! Some elements are reverse engineered from Python's PyYAML, but not
!!! very much in the end.  This is partly because I could not follow
!!! and partly because Fortran lacks key abstractions that would
!!! facilitate a similar design.  Mostly the former.

!!! Useful guidance on strings from
!!! http://blogs.perl.org/users/tinita/2018/03/strings-in-yaml---to-quote-or-not-to-quote.html

#include "error_handling.h"

module fy_Lexer
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR              ! "\0"
  use, intrinsic :: iso_c_binding, only: NL => C_NEW_LINE         ! "\n"
  use, intrinsic :: iso_c_binding, only: CR => C_CARRIAGE_RETURN  ! "\r"
  use, intrinsic :: iso_c_binding, only: TAB => C_HORIZONTAL_TAB  ! "\t"
  use fy_Reader
  use fy_Tokens
  use fy_TokenVector
  use fy_ErrorCodes
  use fy_SimpleKey
  use fy_ErrorHandling
  use fy_IntegerSimpleKeyMap
  use fy_KeywordEnforcer
  use gFTL_IntegerVector
  use gFTL_StringStringMap
  implicit none
  private

  public :: Lexer

  ! A (limited) YAML lexer.
  type :: Lexer
     type(Reader) :: r

     logical :: reached_end_of_stream = .false.
     integer :: current_flow_level = 0
     type(TokenVector) :: processed_tokens  ! not yet given
     integer :: num_tokens_given = 0
     integer :: indent = -1
     type(IntegerVector) :: level_indentations

     ! A dictionary of potential keys indexd by flow level.
     ! 
     ! A Keytoken is emitted before all keys (simple and otherwise), but
     ! cannot always be identified immediately.
     ! Simple keys should be limited to a single line and 1024 characters. (spec)
     
     type(IntegerSimpleKeyMap) :: possible_simple_keys ! indexed by flow level
     logical :: allow_simple_key = .true.

   contains
     ! public access
     procedure :: get_token

     procedure :: pop_token
     procedure :: need_more_tokens
     procedure :: lex_tokens

     procedure :: scan_to_next_token
     procedure :: scan_line_break
     procedure :: scan_plain_spaces
     procedure :: scan_flow_scalar
     procedure :: scan_flow_scalar_breaks
     procedure :: scan_flow_scalar_spaces
     procedure :: scan_flow_scalar_non_spaces

     procedure :: unwind_indentation

     procedure :: is_at_document_boundary

     procedure :: process_beginning_of_stream
     procedure :: process_end_of_stream
     procedure :: process_document_boundary
     procedure :: process_flow_collection_start
     procedure :: process_flow_collection_end

     procedure :: process_flow_next_entry
     procedure :: process_block_next_entry
     procedure :: is_block_next_entry
     procedure :: add_indentation

     procedure :: process_key
     procedure :: is_key
     procedure :: process_value
     procedure :: is_value
     procedure :: is_plain_scalar
     procedure :: process_quoted_scalar
     procedure :: process_plain_scalar

     procedure :: process_alias
     procedure :: process_anchor
     procedure :: scan_anchor_or_alias

     procedure :: remove_stale_simple_keys
     procedure :: remove_possible_simple_key
     procedure :: get_nearest_possible_simple_key
     procedure :: save_simple_key

     ! Pass through to reader (for clarity)
     procedure :: peek
     procedure :: prefix
     procedure :: forward
     procedure :: column
     procedure :: line
     procedure :: index
  end type Lexer

  interface Lexer
     module procedure new_Lexer
  end interface Lexer



  character(*), parameter :: WHITESPACE_CHARS = C_NULL_CHAR // ' ' // TAB // CR // NL
  character(*), parameter :: BACKSLASH_ = "\\\\"
  character(*), parameter :: BACKSLASH = BACKSLASH_(1:1)

  character(*), parameter :: ESCAPES = '0tnr ' // TAB // '"' // BACKSLASH
  type(StringStringMap) :: ESCAPE_REPLACEMENTS

  logical, save :: initialized = .false.
  
contains

  function new_Lexer(r) result(lexr)
    type(Lexer) :: lexr
    type(Reader), intent(in) :: r

    if (.not. initialized) call initialize()

    lexr%r = r
    call lexr%process_beginning_of_stream()
    
  end function new_Lexer


  ! Note only implementing the escape sequences that I understand
  ! This is probably sufficient for the target audience.
  subroutine initialize()
    call ESCAPE_REPLACEMENTS%insert('0', C_NULL_CHAR)
    call ESCAPE_REPLACEMENTS%insert('t', TAB)
    call ESCAPE_REPLACEMENTS%insert(TAB, TAB)
    call ESCAPE_REPLACEMENTS%insert('n', NL)
    call ESCAPE_REPLACEMENTS%insert('r', CR)
    call ESCAPE_REPLACEMENTS%insert(' ', ' ')
    call ESCAPE_REPLACEMENTS%insert('"', '"')
    call ESCAPE_REPLACEMENTS%insert(BACKSLASH, BACKSLASH)
    initialized = .false.
  end subroutine initialize

  ! return the next token
  function get_token(this, unusable, rc) result(token)
    class(AbstractToken), allocatable :: token
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    logical :: need_more
    integer :: status
    
    do
       need_more = this%need_more_tokens(rc=status)
       if (status /= SUCCESS) then
          token = NullToken()
          __RETURN__(status)
       end if
       if (need_more) then
          call this%lex_tokens(rc=status)
          if (status /= SUCCESS) token = NullToken()
          __VERIFY__(status)
       else
          exit
       end if
    end do

    if (this%processed_tokens%size() >= 0) then
       this%num_tokens_given = this%num_tokens_given + 1
       token = this%pop_token()
    else
       token = NullToken()
    end if

    __RETURN__(SUCCESS)

  end function get_token

  logical function need_more_tokens(this, unusable, rc)
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    integer :: status

    rc = SUCCESS ! unless

    if (this%reached_end_of_stream) then
       need_more_tokens = .false.
    elseif (this%processed_tokens%size() == 0) then
       need_more_tokens = .true.
    else
       ! Still have some processed tokens to give, but the current
       ! token is possibly a simple key and lexing must continue until
       ! a full token is produced.
       call this%remove_stale_simple_keys(__RC__)

       if (this%get_nearest_possible_simple_key() == this%num_tokens_given) then
          need_more_tokens = .true.
       else
          need_more_tokens = .false.
       end if
    end if

    if (present(rc)) rc = SUCCESS
    return

  end function need_more_tokens


  ! Remove entries in possible simple keys that are no longer, er,
  ! possible.
  subroutine remove_stale_simple_keys(this, unusable, rc)
    class(Lexer), target, intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    type(SimpleKey), pointer :: possible_key
    type (IntegerSimpleKeyMapIterator) :: iter

    iter = this%possible_simple_keys%begin()
    do while (iter /= this%possible_simple_keys%end())
       possible_key => iter%value()
       if ((possible_key%line /= this%line()) .or. (this%index() - possible_key%index > 1024)) then
          __ASSERT__(.not. possible_key%required, MISSING_COLON_WHILE_SCANNING_A_SIMPLE_KEY)
          call this%possible_simple_keys%erase(iter)
       end if
       call iter%next()
    end do

    __RETURN__(SUCCESS)

  end subroutine remove_stale_simple_keys


  ! Remove possible key saved at the current flow level
  subroutine remove_possible_simple_key(this)
    class(Lexer), intent(inout) :: this

    type(SimpleKey) :: key

    associate (level => this%current_flow_level)
      if (this%possible_simple_keys%count(level) > 0) then
         key = this%possible_simple_keys%at(level)
      end if
    end associate
  end subroutine remove_possible_simple_key


  integer function get_nearest_possible_simple_key(this) result(token_number)
    class(Lexer), intent(inout) :: this

    type(SimpleKey), pointer :: possible_key
    type (IntegerSimpleKeyMapIterator) :: iter
    integer :: min_token_number

    min_token_number = huge(1)
    iter = this%possible_simple_keys%begin()
    do while (iter /= this%possible_simple_keys%end())
       possible_key => iter%value()
       if (possible_key%token_number < min_token_number) then
          min_token_number = possible_key%token_number
       end if
       call iter%next()
    end do

    token_number = min_token_number
    
  end function get_nearest_possible_simple_key


  subroutine save_simple_key(this, unusable, rc)
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    logical :: required
    type(SimpleKey) :: key
    integer :: token_number
    integer :: status

    required = (this%current_flow_level == 0) .and. (this%indent == this%column())
    __ASSERT__(this%allow_simple_key .or. (.not. required), IMPOSSIBLE_COMBINATION)
    if (this%allow_simple_key) then
       call this%remove_stale_simple_keys(__RC__)
       token_number = this%num_tokens_given + this%processed_tokens%size()
       key = SimpleKey(token_number, required, this%index(), this%line(), this%column())
       call this%possible_simple_keys%insert(this%current_flow_level,key)
    end if

    __RETURN__(SUCCESS)
  end subroutine save_simple_key



  ! Have not implemented stack in gFTL, so
  ! vector will have to suffice
  function pop_token(this) result(token)
    class(AbstractToken), allocatable :: token
    class(Lexer), intent(inout) :: this

    associate (tokens => this%processed_tokens)
      token = tokens%at(1)
      call tokens%erase(tokens%begin())
    end associate

  end function pop_token
    

  ! All the different cases ...
  subroutine lex_tokens(this, unusable, rc)
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    character(1) :: ch
    integer :: status

    ! White spaces and comments before a token are irrelevant
    call this%scan_to_next_token()

    call this%remove_stale_simple_keys(__RC__)

    ! Indentation at start of token matters (unless in flow)
    call this%unwind_indentation(this%column())

    ! Determine type of token from first character
    ch = this%peek()
    ! Cannot quite use SELECT CASE here.  Some cases require further
    ! processing to ascertain their relevancy.

    if (ch == C_NULL_CHAR) then
       call this%process_end_of_stream()
       __RETURN__(SUCCESS)
    end if
!!$ !!$    if (ch == DIRECTIVE_INDICATOR .and. this%is_at_directive())  then
!!$ !!$       call this%process_directive()
!!$           __RETURN__(SUCCESS)
!!$ !!$    end if
    if ((ch == DOCUMENT_START_INDICATOR) .and. this%is_at_document_boundary('---')) then
       call this%process_document_boundary(DocumentStartToken())
       __RETURN__(SUCCESS)
    end if
    if (ch == DOCUMENT_END_INDICATOR .and. this%is_at_document_boundary('...')) then
       call this%process_document_boundary(DocumentEndToken())
       __RETURN__(SUCCESS)
    end if

    if (ch == FLOW_SEQUENCE_START_INDICATOR) then
       call this%process_flow_collection_start(FlowSequenceStartToken())
       __RETURN__(SUCCESS)
    end if
    if (ch == FLOW_SEQUENCE_END_INDICATOR) then
       call this%process_flow_collection_end(FlowSequenceEndToken())
       __RETURN__(SUCCESS)
    end if
    if (ch == FLOW_MAPPING_START_INDICATOR) then
       call this%process_flow_collection_start(FlowMappingStartToken())
       __RETURN__(SUCCESS)
    end if
    if (ch == FLOW_MAPPING_END_INDICATOR) then
       call this%process_flow_collection_end(FlowMappingEndToken())
       __RETURN__(SUCCESS)
    end if
    if (ch == FLOW_NEXT_ENTRY_INDICATOR) then
       call this%process_flow_next_entry()
       __RETURN__(SUCCESS)
    end if
    if (ch == BLOCK_NEXT_ENTRY_INDICATOR) then
       if (this%is_block_next_entry()) then
          call this%process_block_next_entry(__RC__)
          __RETURN__(SUCCESS)
       end if
    end if
    if (ch == KEY_INDICATOR) then
       if (this%is_key()) then
          call this%process_key(rc=status)
          __RETURN__(SUCCESS)
       end if
    end if

    if (ch == VALUE_INDICATOR .and. this%is_value()) then
       call this%process_value(__RC__)
       __RETURN__(SUCCESS)
    end if

    if (ch == ALIAS_INDICATOR) then
       call this%process_alias(__RC__)
       __RETURN__(SUCCESS)
    end if
    if (ch == ANCHOR_INDICATOR) then
       call this%process_anchor(__RC__)
       __RETURN__(SUCCESS)
    end if
!!$    if (ch == TAG_INDICATOR) then
!!$       call this%process_TAG()
!!$       __RETURN__(SUCCESS)
!!$    end if

    if (ch == SINGLE_QUOTED_SCALAR_INDICATOR) then
       call this%process_quoted_scalar(style="'",__RC__)
       __RETURN__(SUCCESS)
    end if

    if (ch == DOUBLE_QUOTED_SCALAR_INDICATOR) then
       call this%process_quoted_scalar(style='"')
       __RETURN__(SUCCESS)
    end if
    
    if (this%is_plain_scalar()) then
       call this%process_plain_scalar()
       __RETURN__(SUCCESS)
    end if

    ! Error: ch cannot start any token
    __FAIL__(UNEXPECTED_CHARACTER)

  end subroutine lex_tokens

  ! Skip over spaces, line breaks and comments.
  subroutine scan_to_next_token(this)
    class(Lexer), intent(inout) :: this

    logical :: found
    
    found = .false.

    do while (.not. found)
       do while (this%peek() == ' ')
          call this%forward()
       end do
       if (this%peek() == COMMENT_INDICATOR) then
          do while (scan(this%peek(), C_NULL_CHAR//CR//NL) == 0)
             call this%forward()
          end do
       end if
       if (this%scan_line_break() /= '') then
          if (this%current_flow_level == 0) then
             this%allow_simple_key = .true.
          end if
       else
          found = .true.
       end if
    end do

  end subroutine scan_to_next_token

  ! Very simple (ignores international chars).  No test.
  function scan_line_break(this) result(line_break)
    character(:), allocatable :: line_break
    class(Lexer), intent(inout) :: this

    character :: ch

    ch = this%peek()
    if (scan(ch, NL//CR) > 0) then
       if (this%prefix(2) == CR//NL) then
          call this%forward(offset=2)
       else
          call this%forward()
       end if
       line_break = NL
       return
    end if

    line_break = ''
    return
  end function scan_line_break


  subroutine process_beginning_of_stream(this)
    class(Lexer), intent(inout) :: this

    call this%processed_tokens%push_back(StreamStartToken())

  end subroutine process_beginning_of_stream

  subroutine process_end_of_stream(this)
    class(Lexer), intent(inout) :: this

    call this%unwind_indentation(-1)
    this%allow_simple_key = .false.
    this%possible_simple_keys = IntegerSimpleKeyMap()

    call this%processed_tokens%push_back(StreamEndToken())

    this%reached_end_of_stream = .true.

  end subroutine process_end_of_stream


  logical function is_at_document_boundary(this, text)
    class(Lexer), intent(inout) :: this
    character(3), intent(in) :: text

    is_at_document_boundary = .false. ! unless
    
    if (this%column() == 0) then
       if (this%prefix(3) == text) then
          if (scan(this%peek(offset=3), WHITESPACE_CHARS) > 0) then
             is_at_document_boundary = .true.
          end if
       end if
    end if

  end function is_at_document_boundary

  subroutine process_document_boundary(this, token)
    class(Lexer), intent(inout) :: this
    class(AbstractToken), intent(in) :: token

    call this%unwind_indentation(-1)
    call this%remove_possible_simple_key()
    this%allow_simple_key = .false.
    this%possible_simple_keys = IntegerSimpleKeyMap()
    call this%forward(offset=3)

    call this%processed_tokens%push_back(token)
    
  end subroutine process_document_boundary


  subroutine process_flow_collection_start(this, token)
    class(Lexer), intent(inout) :: this
    class(AbstractToken), intent(in) :: token

    call this%save_simple_key()
    this%current_flow_level = this%current_flow_level + 1
    this%allow_simple_key = .true.
    call this%forward()
    call this%processed_tokens%push_back(token)
    
  end subroutine process_flow_collection_start


  subroutine process_flow_collection_end(this, token)
    class(Lexer), intent(inout) :: this
    class(AbstractToken), intent(in) :: token

    this%current_flow_level = this%current_flow_level - 1
    call this%forward()
    call this%processed_tokens%push_back(token)
  end subroutine process_flow_collection_end
  

  subroutine process_flow_next_entry(this)
    class(Lexer), intent(inout) :: this

    this%allow_simple_key = .true.
    call this%remove_possible_simple_key()
    call this%forward()
    call this%processed_tokens%push_back(FlowNextEntryToken())
  end subroutine process_flow_next_entry

  logical function is_block_next_entry(this)
    class(Lexer), intent(inout) :: this

    is_block_next_entry = (scan(this%peek(offset=1),WHITESPACE_CHARS) > 0)
  end function is_block_next_entry

  subroutine process_block_next_entry(this, unusable, rc)
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    if (this%current_flow_level == 0) then
       __ASSERT__(this%allow_simple_key, ILLEGAL_SEQUENCE_ENTRY)
       if (this%add_indentation(this%column())) then
          call this%processed_tokens%push_back(BlockSequenceStartToken())
       end if
    end if

    this%allow_simple_key = .true.
    call this%remove_possible_simple_key()

    call this%forward()
    call this%processed_tokens%push_back(BlockNextEntryToken())

    __RETURN__(SUCCESS)

  end subroutine process_block_next_entry


  logical function add_indentation(this, column)
    class(Lexer), intent(inout) :: this
    integer, intent(in) :: column

    add_indentation = (this%indent < column)
    
    if (add_indentation) then
       call this%level_indentations%push_back(this%indent)
       this%indent = column
       add_indentation = .true.
    else
       add_indentation = .false.
    end if

  end function add_indentation


  subroutine process_value(this, unusable, rc)
    class(Lexer), target, intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc
    

    type(IntegerSimpleKeyMapIterator) :: iter
    type(SimpleKey) :: key


    if (this%possible_simple_keys%count(this%current_flow_level) > 0) then
       iter = this%possible_simple_keys%find(this%current_flow_level)
       key = iter%value()
       call this%possible_simple_keys%erase(iter)
       call this%processed_tokens%insert(key%token_number-this%num_tokens_given+1, KeyToken())
       
       if (this%current_flow_level == 0) then
          if (this%add_indentation(key%column)) then
             call this%processed_tokens%insert(key%token_number-this%num_tokens_given+1, BlockMappingStartToken())
          end if
       end if
       ! simple key cannot be followed immediately by another simple key
       this%allow_simple_key = .false.

    else

       if (this%current_flow_level == 0) then
          __ASSERT__(this%allow_simple_key, ILLEGAL_VALUE_IN_MAPPING)
       end if

       this%allow_simple_key = (this%current_flow_level == 0)
       call this%remove_possible_simple_key()

    end if
             
    call this%forward()
    call this%processed_tokens%push_back(ValueToken())
    __RETURN__(SUCCESS)
  end subroutine process_value

  ! In block context, a leading ":" indicates a ValueToken only if it
  ! is followed by whitespace.  Otherwise it might just be a
  ! ScalarToken that happens to start with ":".  Flow context has no
  ! such restriction.
  logical function is_value(this)
    class(Lexer), intent(inout) :: this

    if (this%current_flow_level > 0) then
       is_value = .true.
    else
       is_value = (scan(this%peek(offset=1), WHITESPACE_CHARS) > 0)
    end if
    
  end function is_value
  
  logical function is_plain_scalar(this)
    class(Lexer), intent(inout) :: this

    character(1) :: ch

    ch = this%peek()
    
    is_plain_scalar = scan(ch, WHITESPACE_CHARS//"-?:,[]{}#&*!|>%@`"//"'"//'"') == 0
    if (.not. is_plain_scalar) then
       ch = this%peek(offset=1)
       is_plain_scalar = (scan(ch, WHITESPACE_CHARS) == 0) &
            .and. (ch == '-' .or. (this%current_flow_level > 0 .and. scan(ch,'?:')> 0))
    end if
  end function is_plain_scalar

  subroutine process_plain_scalar(this, unusable, rc)
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    integer :: n
    integer :: indent
    character(1) :: ch
    character(:), allocatable :: chunks, spaces

    call this%save_simple_key()
    this%allow_simple_key = .false.
    
    indent = this%indent + 1
    chunks = ''
    spaces = ''
    do
       n = 0
       if (this%peek() == '#') exit
       do
          ch = this%peek(offset=n)
          if ((scan(ch, WHITESPACE_CHARS) > 0) &
               & .or. (this%current_flow_level == 0 .and. ch == ':' &
               &       .and. scan(this%peek(offset=n+1), WHITESPACE_CHARS) > 0) &
               & .or. (this%current_flow_level > 0 .and. scan(ch,',:?[]{}')> 0)) then
             exit
          end if
          n = n + 1
       end do

       ! Copying this error handling situation from Python implementation
       if ((this%current_flow_level > 0) .and. ch == ':') then
          if (scan(this%peek(offset=n+1), WHITESPACE_CHARS) == 0) then
             call this%forward(offset=n)
             __FAIL__(UNEXPECTED_COLON_IN_PLAIN_SCALAR)
          end if
       end if

       if (n == 0) exit
       this%allow_simple_key = .true.
       chunks = chunks // spaces // this%prefix(n)
       call this%forward(offset=n)
       spaces = this%scan_plain_spaces()
       if ((len(spaces) == 0 .or. this%peek() == '#') .or. &
            & (this%current_flow_level == 0 .and. this%column() < indent)) then
          exit
       end if
    end do

    call this%processed_tokens%push_back(ScalarToken(chunks, is_plain=.true.))

    if (present(rc)) rc = SUCCESS
    return
    
  end subroutine process_plain_scalar


  function scan_plain_spaces(this) result(chunks)
    character(:), allocatable :: chunks
    class(Lexer), intent(inout) :: this

    character(:), allocatable :: whitespaces
    character(:), allocatable :: line_break
    character(:), allocatable :: breaks
    integer :: n
    character :: ch
    character(:), allocatable :: pfix
    
    chunks = ''
    n = 0

    do while (scan(this%peek(offset=n),' ') > 0)
       n = n + 1
    end do

    whitespaces = this%prefix(n)
    call this%forward(offset=n)

    ch = this%peek()
    if (scan(ch, CR//NL) > 0) then
       line_break = this%scan_line_break()
       this%allow_simple_key = .true.
       pfix = this%prefix(3)
       if ( &
            & (pfix == '---' .or. pfix == '...') .and. &
            & scan(this%peek(offset=3), WHITESPACE_CHARS) > 0) then
          chunks = ''
          return
       end if

       breaks = ''
       do while (scan(this%peek(), CR // NL) > 0)
          if (this%peek() == ' ') then
             call this%forward()
          else
             breaks = breaks // this%scan_line_break()
             pfix = this%prefix(3)
             if ( &
                  & (pfix == '---' .or. pfix == '...') .and. &
                  & scan(this%peek(offset=3), WHITESPACE_CHARS) > 0) then
                chunks = ''
                return
             end if
          end if
       end do
       if (line_break /= NL) then
          chunks = chunks // line_break
       elseif (len(line_break) == 0) then
          chunks = chunks // ' '
       end if
       chunks = chunks // breaks
    else
       chunks = chunks // whitespaces
    end if

  end function scan_plain_spaces

  subroutine process_quoted_scalar(this, style, unusable, rc)
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style ! "'" or '"'
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    integer :: status
    class(AbstractToken), allocatable :: token

    call this%save_simple_key()
    this%allow_simple_key = .false.
    token = this%scan_flow_scalar(style, __RC__)
    call this%processed_tokens%push_back(token)
    __RETURN__(SUCCESS)
  end subroutine process_quoted_scalar


  function scan_flow_scalar(this, style, unusable, rc) result(token)
    class(AbstractToken), allocatable :: token
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    character(:), allocatable :: chunks
    integer :: status
    
    chunks = ''
    call this%forward()
    chunks = chunks // this%scan_flow_scalar_non_spaces(style, __RC__)
    do while (this%peek() /= style)
       chunks = chunks // this%scan_flow_scalar_spaces(style)
       chunks = chunks // this%scan_flow_scalar_non_spaces(style)
    end do
    call this%forward()

    token = ScalarToken(chunks,is_plain=.false.,style=style)

    __RETURN__(status)


  end function scan_flow_scalar


  function scan_flow_scalar_spaces(this, style, unusable, rc) result(text)
    character(:), allocatable :: text
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    integer :: n
    character(:), allocatable :: whitespaces
    character :: ch
    character(:), allocatable :: line_break, breaks

    integer :: status

    if (present(rc)) rc = SUCCESS ! unless
    
    text = ''
    n = 0

    do while (scan(this%peek(offset=n), ' ' //TAB) > 0)
       n = n + 1
    end do

    whitespaces = this%prefix(n)
    call this%forward(offset=n)

    ch = this%peek()
    if (ch == C_NULL_CHAR) then
       __FAIL__(END_OF_STREAM_INSIDE_QUOTES)
    elseif (scan(ch, CR//NL) > 0) then
       line_break = this%scan_line_break()
       breaks = this%scan_flow_scalar_breaks(style,__RC__)
       if (line_break /= NL) then
          text = text // line_break
       elseif (len(breaks) > 0) then
          text = text // ' '
       end if
    else
       text = text // whitespaces
    end if

    __RETURN__(SUCCESS)
    
  end function scan_flow_scalar_spaces

  function scan_flow_scalar_breaks(this, style, unusable, rc) result(text)
    character(:), allocatable :: text
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    character(:), allocatable :: pfix
    
    text = ''
    do
       pfix = this%prefix(3)
       if ((pfix == '---' .or. pfix == '...') .and. scan(this%peek(offset=3), WHITESPACE_CHARS)>0) then
          __FAIL__(UNEXPECTED_DOCUMENT_SEPARATOR)
       end if
       do while (scan(this%peek(),' '//TAB) > 0)
          call this%forward()
       end do
       if (scan(this%peek(), CR//NL) > 0) then
          text = text // this%scan_line_break()
       else
          __RETURN__(SUCCESS)
          return
       end if
    end do

    __RETURN__(SUCCESS)
  end function scan_flow_scalar_breaks


  function scan_flow_scalar_non_spaces(this, style, unusable, rc) result(text)
    character(:), allocatable :: text
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    integer :: n
    character :: ch
    character(:), allocatable :: line_break
    integer :: status

    text = ''

    do
       n = 0
       do while (scan(this%peek(offset=n), "'"//'"'//BACKSLASH//' '//C_NULL_CHAR//TAB//CR//NL) == 0)
          n = n + 1
       end do
       if (n > 0) then
          text = text // this%prefix(n)
          call this%forward(offset=n)
       end if
       ch = this%peek()
       if ((style /= '"') .and. (ch == "'") .and. (this%peek(offset=1) == "'")) then
          text = text // "'"
          call this%forward(offset=2)
       elseif ((style == '"' .and. (ch == "'")) .or. (style == "'" .and. scan(ch,'"'//BACKSLASH) > 0)) then
          text = text // ch
          call this%forward()
       elseif ((style == '"') .and. ch == BACKSLASH) then
          call this%forward()
          ch = this%peek()
          if (scan(ch, ESCAPES) > 0) then
             text = text // ESCAPE_REPLACEMENTS%at(ch)
             call this%forward()
!!$          elseif (scan(ch, ESCAPE_CODES))
!!$             ...
          elseif (scan(ch, CR//NL) > 0) then
             line_break = this%scan_line_break() ! updates internal state, but disregard output value
             text = text // this%scan_flow_scalar_breaks(style)
          else
             __FAIL__(UNKNOWN_ESCAPE_CHARACTER_IN_DOUBLE_QUOTED_SCALAR)
          end if
       else
          __RETURN__(SUCCESS)
       end if
    end do

    __RETURN__(SUCCESS)

  end function scan_flow_scalar_non_spaces

  ! Following python's example and using a strict requirement
  ! indentation == column rather than the spec (indentation >= column)
  subroutine unwind_indentation(this, column)
    class(Lexer), intent(inout) :: this
    integer, intent(in) :: column

    ! flow context ignores indentation
    if (this%current_flow_level > 0) then ! nothing to do
       return
    end if

    ! In a block context, we need to end each block
    ! that is indented more than the current column
    do while (this%indent > column)
       associate (indents => this%level_indentations)
         this%indent = indents%back()
         call indents%erase(indents%end())
         call this%processed_tokens%push_back(BlockEndToken())
       end associate
    end do
       
  end subroutine unwind_indentation

  ! In block context, a leading "?" indicates a KeyToken only if it is
  ! followed by whitespace.  Otherwise it might just be a ScalarToken
  ! that happens to start with "?".  Flow context has no such
  ! restriction.
  logical function is_key(this)
    class(Lexer), intent(inout) :: this

    if (this%current_flow_level >0) then
       is_key = .true.
    else
       is_key = (scan(this%peek(offset=1), WHITESPACE_CHARS) > 0)
    end if
    
  end function is_key

  subroutine process_key(this, unusable, rc)
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    ! If not in flow context, may need to add a mapping start token.
    if (this%current_flow_level == 0) then

       __ASSERT__(this%allow_simple_key,UNEXPECTED_MAPPING_KEY)

       if (this%add_indentation(this%column())) then
          call this%processed_tokens%push_back(BlockMappingStartToken())
       end if

    end if

    this%allow_simple_key = (this%current_flow_level == 0)
    call this%remove_possible_simple_key()
    call this%forward()
    call this%processed_tokens%push_back(KeyToken())

    __RETURN__(SUCCESS)

  end subroutine process_key

  ! The following functions are simple wrappers for invoking the
  ! corresponding methods on the reader component.  These are just for
  ! convenience/clarity.
  
  function peek(this, unusable, offset) result(ch)
    character(len=1) :: ch
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(in) :: offset
    ch = this%r%peek(offset=offset)
  end function peek

  function prefix(this, n) result(buffer)
    integer, intent(in) :: n
    character(len=:), allocatable :: buffer
    class(Lexer), intent(inout) :: this
    buffer = this%r%prefix(n)
  end function prefix


  subroutine forward(this, unusable, offset)
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(in) :: offset
    call this%r%forward(offset=offset)
  end subroutine forward


  integer function column(this)
    class(Lexer), intent(in) :: this
    column = this%r%get_column()
  end function column

  integer function line(this)
    class(Lexer), intent(in) :: this
    line = this%r%get_line()
  end function line


  integer function index(this)
    class(Lexer), intent(in) :: this
    index = this%r%get_index()
  end function index

  subroutine process_alias(this, unusable, rc)
     class(Lexer), intent(inout) :: this
     class(KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     class(AbstractToken), allocatable :: token
     integer :: status

     token = NullToken() ! unless
     
     ! Starting a simple key?
     call this%save_simple_key()

     ! No simple keys after this
     this%allow_simple_key = .false.

     ! Add anchor
     token = this%scan_anchor_or_alias('alias',__RC__)
     call this%processed_tokens%push_back(token)


  end subroutine process_alias

  subroutine process_anchor(this, unusable, rc)
     class(Lexer), intent(inout) :: this
     class(KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     class(AbstractToken), allocatable :: token
     integer :: status

     ! Starting a simple key?
     call this%save_simple_key()

     ! No simple keys after this
     this%allow_simple_key = .false.

     ! Add anchor
     token = this%scan_anchor_or_alias('anchor', __RC__)
     call this%processed_tokens%push_back(token)
     
  end subroutine process_anchor


  function scan_anchor_or_alias(this, type, unusable, rc) result(token)
     class(AbstractToken), allocatable :: token
     class(Lexer), intent(inout) :: this
     character(*), intent(in) :: type
     class(KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     character(1) :: ch
     character(:), allocatable :: name
     character(:), allocatable :: value
     integer :: length
     integer :: status

     token = NullToken() ! unless ...
     ! Following Python YAML: aliases must be numbers and ASCII
     ! characters.
     ch = this%peek()
     if (ch == '*') then
        name = 'alias'
     else
        name = 'anchor'
     end if

     call this%forward()
     length = 0
     ch = this%peek(offset=length)
     do while (is_alphanumeric(ch))
        length = length + 1
        ch = this%peek(offset=length)
     end do
     if (length == 0) then
        __FAIL__(NON_ALPHANUMERIC_CHARACTER)
     end if

     value = this%prefix(n=length)
     call this%forward(offset=length)
     
     ch = this%peek()

     if (scan(ch, C_NULL_CHAR // ' ' // TAB // CR // NL // '?:,]{%@') == 0) then
        __FAIL__(UNEXPECTED_CHARACTER)
     end if

     select case (type)
     case ('anchor')
        token = AnchorToken(value)
     case ('alias')
        token = AliasToken(value)
     case default
        __FAIL__(NONSPECIFIC_ERROR)
     end select

     __RETURN__(SUCCESS)
  end function scan_anchor_or_alias

  logical  function is_alphanumeric(c)
     character(1), intent(in) :: c
     
     is_alphanumeric = &
          & ('0' <= c .and. c <= '9') .or. &
          & ('a' <= c .and. c <= 'z') .or. &
          & ('A' <= c .and. c <= 'Z') .or. &
          & (c == '-' .or. c == '_')
     
  end function is_alphanumeric


end module fy_Lexer
