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

module fy_Lexer
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR              ! "\0"
  use, intrinsic :: iso_c_binding, only: NL => C_NEW_LINE         ! "\n"
  use, intrinsic :: iso_c_binding, only: CR => C_CARRIAGE_RETURN  ! "\r"
  use, intrinsic :: iso_c_binding, only: TAB => C_HORIZONTAL_TAB  ! "\t"
  use fy_Reader
  use fy_Tokens
  use fy_TokenVector
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
     type(TokenVector) :: processed_tokens  ! not yet emitted
     integer :: num_tokens_emitted = 0
     integer :: indent = -1
     type(IntegerVector) :: level_indentations

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

     procedure :: process_end_of_stream
     procedure :: process_document_boundary
     procedure :: process_flow_collection_start
     procedure :: process_flow_collection_end

     procedure :: process_flow_next_entry
     procedure :: process_block_next_entry
     procedure :: add_indentation

     procedure :: process_value
     procedure :: is_plain_scalar
     procedure :: process_quoted_scalar
     procedure :: process_plain_scalar

     ! Pass through to reader (for clarity)
     procedure :: peek
     procedure :: prefix
     procedure :: forward
     procedure :: column
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
    call lexr%processed_tokens%push_back(StreamStartToken())

    
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
  function get_token(this) result(token)
    class(AbstractToken), allocatable :: token
    class(Lexer), intent(inout) :: this

    do while (.not. this%reached_end_of_stream)
       call this%lex_tokens()
    end do

    if (this%processed_tokens%size() >= 0) then
       this%num_tokens_emitted = this%num_tokens_emitted + 1
       token = this%pop_token()
    else
       token = NULL_TOKEN
    end if
    
  end function get_token

  logical function need_more_tokens(this)
    class(Lexer), intent(inout) :: this

    if (this%reached_end_of_stream) then
       need_more_tokens = .false.
       return
    end if

    if (this%processed_tokens%size() == 0) then
       need_more_tokens = .true.
       return
    end if

    need_more_tokens = .false.

  end function need_more_tokens

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
  subroutine lex_tokens(this)
    class(Lexer), intent(inout) :: this

    character(1) :: ch

    ! White spaces and comments before a token are irrelevant
    call this%scan_to_next_token()

!!$
!!$    ! Do something with simple keys ...
!!$
!!$
!!$    ! Indentation at start of token matters (unless in flow)
!!$    call this%unwind_indentation(this%column())
!!$
    ! Determine type of token from first character
    ch = this%peek()

    ! Cannot quite use SELECT CASE here.  Some cases require further
    ! processing to ascertain their relevancy.

    if (ch == C_NULL_CHAR) then
       call this%process_end_of_stream()
       print*,'huh:', this%processed_tokens%size()
       return
    end if
!!$ !!$    if (ch == DIRECTIVE_INDICATOR .and. this%is_at_directive())  then
!!$ !!$       call this%process_directive()
!!$ !!$       return
!!$ !!$    end if
    if ((ch == DOCUMENT_START_INDICATOR) .and. this%is_at_document_boundary('---')) then
       call this%process_document_boundary(DocumentStartToken())
       return
    end if
    if (ch == DOCUMENT_END_INDICATOR .and. this%is_at_document_boundary('...')) then
       call this%process_document_boundary(DocumentEndToken())
       return
    end if

    if (ch == FLOW_SEQUENCE_START_INDICATOR) then
       call this%process_flow_collection_start(FlowSequenceStartToken())
       return
    end if
    if (ch == FLOW_SEQUENCE_END_INDICATOR) then
       print*,__FILE__,__LINE__,ch
       call this%process_flow_collection_end(FlowSequenceEndToken())
       print*,__FILE__,__LINE__,ch
       return
    end if
    if (ch == FLOW_MAPPING_START_INDICATOR) then
       call this%process_flow_collection_start(FlowMappingStartToken())
       return
    end if
    if (ch == FLOW_MAPPING_END_INDICATOR) then
       call this%process_flow_collection_end(FlowMappingEndToken())
       return
    end if
    if (ch == FLOW_NEXT_ENTRY_INDICATOR) then
       call this%process_flow_next_entry()
       return
    end if
    if (ch == BLOCK_NEXT_ENTRY_INDICATOR) then
       call this%process_block_next_entry()
       return
    end if
    if (ch == VALUE_INDICATOR) then
       call this%process_value()
       return
    end if
!!$ !!$    if (ch == ALIAS_INDICATOR) then
!!$ !!$       call this%process_alias()
!!$ !!$       return
!!$ !!$   end if
!!$ !!$    if (ch == ANCHOR_INDICATOR) then
!!$ !!$       call this%process_anchor()
!!$ !!$       return
!!$ !!$   end if
!!$ !!$    if (ch == TAG_INDICATOR) then
!!$ !!$       call this%process_TAG()
!!$ !!$       return
!!$ !!$   end if
!!$    if (ch == TAG_INDICATOR) then
!!$       call this%process_TAG()
!!$       return
!!$    end if
    if (ch == SINGLE_QUOTED_SCALAR_INDICATOR) then
       call this%process_quoted_scalar(style="'")
       return
    end if

    if (ch == DOUBLE_QUOTED_SCALAR_INDICATOR) then
       call this%process_quoted_scalar(style='"')
       return
    end if
!!$    if (ch == DOUBLE_QUOTED_SCALAR_INDICATOR) then
!!$       call this%process_double_quoted_scalar()
!!$       return
!!$    end if
!!$    
    if (this%is_plain_scalar()) then
       call this%process_plain_scalar()
       return
    end if

!!$    print*,'Cannot start token with <',ch,'>'
!!$       
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


  subroutine process_end_of_stream(this)
    class(Lexer), intent(inout) :: this

!!$    call this%unwind_indentation(-1)
    call this%processed_tokens%push_back(StreamEndToken())
    this%reached_end_of_stream = .true.

  end subroutine process_end_of_stream
!!$
!!$  subroutine process_document_start(this)
!!$    class(Lexer), intent(inout) :: this
!!$
!!$    call this%process_document(DocumentStartToken())
!!$  end subroutine process_document_start
!!$
!!$  subroutine process_document_end(this)
!!$    class(Lexer), intent(inout) :: this
!!$
!!$    call this%process_document(DocumentEndToken())
!!$  end subroutine process_document_end
!!$
!!$  subroutine process_document(this, token)
!!$    class(Lexer), intent(inout) :: this
!!$    class(AbstractToken), intent(in) :: token
!!$
!!$    call this%unwind_indentation(-1)
!!$
!!$    call this%forward(3)
!!$  end subroutine process_document
!!$

!!$
!!$  subroutine unwind_stack(this)
!!$    class(Lexer), intent(inout) :: this
!!$
!!$    do while (this%flow_level > 0)
!!$       call this%tokens%push_back(BlockSequenceEndToken())
!!$       this%flow_level = this%flow_level - 1
!!$    end do
!!$
!!$    call this%tokens%push_back(DocumentEndToken())
!!$    
!!$  end subroutine unwind_stack
!!$  
!!$  subroutine eat_whitespace(this)
!!$    class(Lexer), intent(inout) :: this
!!$
!!$    character(len=1) :: ch
!!$    
!!$    ch = this%r%peek()
!!$    do while (ch == ' ')
!!$       call this%r%forward(offset=1)
!!$       ch = this%r%peek()
!!$    end do
!!$
!!$  end subroutine eat_whitespace
!!$


  logical function is_at_document_boundary(this, text)
    class(Lexer), intent(inout) :: this
    character(3), intent(in) :: text

    character :: ch
    integer :: i

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

!!$    call this%unwind_indentation(-1)
    call this%forward(offset=3)
    call this%processed_tokens%push_back(token)
    
  end subroutine process_document_boundary


  subroutine process_flow_collection_start(this, token)
    class(Lexer), intent(inout) :: this
    class(AbstractToken), intent(in) :: token

    this%current_flow_level = this%current_flow_level + 1
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

    call this%forward()
    call this%processed_tokens%push_back(FlowNextEntryToken())
  end subroutine process_flow_next_entry
  

  subroutine process_block_next_entry(this)
    class(Lexer), intent(inout) :: this

    if (this%current_flow_level > 0) then
       if (this%add_indentation(this%column())) then
          call this%processed_tokens%push_back(BlockSequenceStartToken())
       end if
    end if

    call this%forward()
    call this%processed_tokens%push_back(BlockNextEntryToken())

  end subroutine process_block_next_entry


  logical function add_indentation(this, column)
    class(Lexer), intent(inout) :: this
    integer, intent(in) :: column

    add_indentation = (this%indent < column)
    
    if (add_indentation) then
       this%indent = column
       call this%level_indentations%push_back(this%indent)
       add_indentation = .true.
    end if

  end function add_indentation


  subroutine process_value(this)
    class(Lexer), intent(inout) :: this

    call this%forward()
    call this%processed_tokens%push_back(ValueToken())

  end subroutine process_value


  logical function is_plain_scalar(this)
    class(Lexer), intent(inout) :: this

    is_plain_scalar = .true.
  end function is_plain_scalar

  subroutine process_plain_scalar(this)
    class(Lexer), intent(inout) :: this

    integer :: n
    integer :: indent
    character(1) :: ch
    character(:), allocatable :: chunks, spaces

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
             error stop 'found unexpected :'
          end if
       end if
          
       if (n == 0) exit

       chunks = chunks // spaces // this%prefix(n)
       call this%forward(offset=n)
       spaces = this%scan_plain_spaces()
       if ((len(spaces) == 0 .or. this%peek() == '#') .or. &
            & (this%current_flow_level == 0 .and. this%column() < indent)) then
          exit
       end if
    end do

    call this%processed_tokens%push_back(ScalarToken(chunks, is_plain=.true.))
    
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
       pfix = this%prefix(3)
       if ( &
            & (pfix == '---' .or. pfix == '...') .and. &
            & scan(this%peek(offset=3), WHITESPACE_CHARS) > 0) then
          chunks = ''
          return
       end if

       breaks = ''
       do while (scan(this%peek(), WHITESPACE_CHARS) > 0)
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
       else
          chunks = chunks // 'u'
       end if
       chunks = chunks // breaks
    else
       chunks = chunks // whitespaces
    end if

  end function scan_plain_spaces

  subroutine process_quoted_scalar(this, style)
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style ! "'" or '"'

    call this%processed_tokens%push_back(this%scan_flow_scalar(style))

  end subroutine process_quoted_scalar


  function scan_flow_scalar(this, style) result(token)
    class(AbstractToken), allocatable :: token
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style

    character(:), allocatable :: chunks
    integer ::n
    
    chunks = ''
    call this%forward()
    chunks = chunks // this%scan_flow_scalar_non_spaces(style)
    do while (this%peek() /= style)
       chunks = chunks // this%scan_flow_scalar_spaces(style)
       chunks = chunks // this%scan_flow_scalar_non_spaces(style)
    end do
    call this%forward()

    call this%processed_tokens%push_back(ScalarToken(chunks,is_plain=.false.,style=style))

  end function scan_flow_scalar


  function scan_flow_scalar_spaces(this, style) result(text)
    character(:), allocatable :: text
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style

    integer :: n
    character(:), allocatable :: whitespaces
    character :: ch
    character(:), allocatable :: line_break, breaks

    text = ''
    n = 0

    do while (scan(this%peek(offset=n), ' ' //TAB) > 0)
       n = n + 1
    end do

    whitespaces = this%prefix(n)
    call this%forward(offset=n)

    ch = this%peek()
    if (ch == C_NULL_CHAR) then
       ERROR STOP "End of stream while scanning a quoted scalar"
    elseif (scan(ch, CR//NL) > 0) then
       line_break = this%scan_line_break()
       breaks = this%scan_flow_scalar_breaks(style)
       if (line_break /= NL) then
          text = text // line_break
       elseif (len(breaks) > 0) then
          text = text // ' '
       end if
    else
       text = text // whitespaces
    end if
    
  end function scan_flow_scalar_spaces

  function scan_flow_scalar_breaks(this, style) result(text)
    character(:), allocatable :: text
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style

    character(:), allocatable :: pfix
    
    text = ''
    do
       pfix = this%prefix(3)
       if ((pfix == '---' .or. pfix == '...') .and. scan(this%peek(offset=3), WHITESPACE_CHARS)>0) then
          error stop "found document scanner while scanning a quoted scalar"
       end if
       do while (scan(this%peek(),' '//TAB) > 0)
          call this%forward()
       end do
       if (scan(this%peek(), CR//NL) > 0) then
          text = text // this%scan_line_break()
       else
          return
       end if
    end do

  end function scan_flow_scalar_breaks


  function scan_flow_scalar_non_spaces(this, style) result(text)
    character(:), allocatable :: text
    class(Lexer), intent(inout) :: this
    character, intent(in) :: style

    integer :: n
    character :: ch
    character(:), allocatable :: line_break

    text = ''

    do
       n = 0
       do while (scan(this%peek(offset=n), "'"//'"'//BACKSLASH//C_NULL_CHAR//TAB//CR//NL) == 0)
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
       elseif (style == '"' .or. (style == "'" .and. scan(ch,'"'//BACKSLASH) > 0)) then
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
             error stop "found unknown escape character while scanning a double quoted scalar"
          end if
       else
          return
       end if
    end do

  end function scan_flow_scalar_non_spaces

  ! Following python's example and using a strict requirement
  ! indentation == column rather than the spec (indentation >= column)
  subroutine unwind_indentation(this)
    class(Lexer), intent(inout) :: this

    ! flow context ignores indentation
    if (this%current_flow_level == 0) then ! nothing to do
       return
    end if

    ! In a block context, we need to end each block
    ! that is indented more than the current column
    do while (this%indent > this%column())
       associate (indents => this%level_indentations)
         this%indent = indents%back()
         call indents%erase(indents%end())
         call this%processed_tokens%push_back(BlockEndToken())
       end associate
    end do
       

  end subroutine unwind_indentation

  ! The following functions are simple wrappers for invoking the
  ! corresponding methods on the reader component.  These are just for
  ! convenience/clarity.
  
  function peek(this, unused, offset) result(ch)
    character(len=1) :: ch
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    integer, optional, intent(in) :: offset
    ch = this%r%peek(offset=offset)
  end function peek

  function prefix(this, n) result(buffer)
    integer, intent(in) :: n
    character(len=:), allocatable :: buffer
    class(Lexer), intent(inout) :: this
    buffer = this%r%prefix(n)
  end function prefix


  subroutine forward(this, unused, offset)
    class(Lexer), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    integer, optional, intent(in) :: offset
    call this%r%forward(offset=offset)
  end subroutine forward


  integer function column(this)
    class(Lexer), intent(in) :: this
    column = this%r%get_column()
  end function column


  integer function index(this)
    class(Lexer), intent(in) :: this
    index = this%r%get_index()
  end function index


end module fy_Lexer
