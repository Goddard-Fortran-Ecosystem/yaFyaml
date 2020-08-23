module fy_Tokens
   use fy_String
  implicit none
  private

  public :: AbstractToken
  public :: NullToken

  public :: StreamStartToken
  public :: StreamEndToken

  public :: DocumentStartToken
  public :: DocumentEndToken

  public :: FlowSequenceStartToken
  public :: FlowSequenceEndToken
  public :: FlowMappingStartToken
  public :: FlowMappingEndToken
  public :: FlowNextEntryToken

  public :: BlockMappingStartToken
  public :: BlockSequenceStartToken
  public :: BlockNextEntryToken
  public :: BlockEndToken

  public :: KeyToken
  public :: ValueToken
  public :: ScalarToken

  public :: AnchorToken
  public :: AliasToken


  type, abstract :: AbstractToken
!!$     private
     character(:), allocatable :: id
   contains
     procedure :: get_id
     procedure :: set_id
  end type AbstractToken

  type, extends(AbstractToken) :: StreamStartToken
  end type StreamStartToken

  type, extends(AbstractToken) :: StreamEndToken
  end type StreamEndToken

  interface StreamStartToken
     module procedure new_StreamStartToken
  end interface StreamStartToken

  interface StreamEndToken
     module procedure new_StreamEndToken
  end interface StreamEndToken
  
  ! Components are public to avoid tedious creation of
  ! setters/getters.  These are mostly "structs", not real classes.
  type, extends(AbstractToken) :: ScalarToken
     character(:), allocatable :: value
     logical :: is_plain
     character :: style = ''
  end type ScalarToken

  interface ScalarToken
     module procedure new_ScalarToken
  end interface ScalarToken

  type, extends(AbstractToken) :: BlockSequenceStartToken
  end type BlockSequenceStartToken

  interface BlockSequenceStartToken
     module procedure new_BlockSequenceStartToken
  end interface BlockSequenceStartToken

  type, extends(AbstractToken) :: BlockNextEntryToken
  end type BlockNextEntryToken

  interface BlockNextEntryToken
     module procedure new_BlockNextEntryToken
  end interface BlockNextEntryToken

  type, extends(AbstractToken) :: BlockEndToken
  end type BlockEndToken

  interface BlockEndToken
     module procedure new_BlockEndToken
  end interface BlockEndToken


  ! Used when a function must return a token, but an error has
  ! occurred.
  type, extends(AbstractToken) :: NullToken
  end type NullToken

  interface NullToken
     module procedure new_NullToken
  end interface NullToken

  type, abstract, extends(AbstractToken) :: DocumentBoundaryToken
  end type DocumentBoundaryToken

  type, extends(DocumentBoundaryToken) :: DocumentStartToken
  end type DocumentStartToken

  type, extends(DocumentBoundaryToken) :: DocumentEndToken
  end type DocumentEndToken

  type, extends(AbstractToken) :: FlowSequenceStartToken
  end type FlowSequenceStartToken

  type, extends(AbstractToken) :: FlowSequenceEndToken
  end type FlowSequenceEndToken

  type, extends(AbstractToken) :: FlowMappingStartToken
  end type FlowMappingStartToken

  type, extends(AbstractToken) :: FlowMappingEndToken
  end type FlowMappingEndToken

  type, extends(AbstractToken) :: FlowNextEntryToken
  end type FlowNextEntryToken

  interface DocumentStartToken
     module procedure new_DocumentStartToken
  end interface DocumentStartToken

  interface DocumentEndToken
     module procedure new_DocumentEndToken
  end interface DocumentEndToken


  interface FlowSequenceStartToken
     module procedure new_FlowSequenceStartToken
  end interface FlowSequenceStartToken

  interface FlowSequenceEndToken
     module procedure new_FlowSequenceEndToken
  end interface FlowSequenceEndToken

  interface FlowMappingStartToken
     module procedure new_FlowMappingStartToken
  end interface FlowMappingStartToken

  interface FlowMappingEndToken
     module procedure new_FlowMappingEndToken
  end interface FlowMappingEndToken

  interface FlowNextEntryToken
     module procedure new_FlowNextEntryToken
  end interface FlowNextEntryToken
  
 
  type, extends(AbstractToken) :: KeyToken
  end type KeyToken

  type, extends(AbstractToken) :: ValueToken
  end type ValueToken

  interface KeyToken
     module procedure new_KeyToken
  end interface KeyToken

  interface ValueToken
     module procedure new_ValueToken
  end interface ValueToken
  

  type, extends(AbstractToken) :: BlockMappingStartToken
  end type BlockMappingStartToken

  interface BlockMappingStartToken
     module procedure new_BlockMappingStartToken
  end interface BlockMappingStartToken

  type, extends(AbstractToken) :: AliasToken
     character(:), allocatable :: value
  end type AliasToken

  interface AliasToken
     module procedure new_AliasToken
  end interface AliasToken
  
  type, extends(AbstractToken) :: AnchorToken
     character(:), allocatable :: value
  end type AnchorToken

  interface AnchorToken
     module procedure new_AnchorToken
  end interface AnchorToken
  

!!$  type, extends(AbstractToken) :: FlowMappingStartToken
!!$  end type FlowMappingStartToken
!!$

  character(*), parameter, public :: DOCUMENT_START_INDICATOR = '-'
  character(*), parameter, public :: DOCUMENT_END_INDICATOR = '.'

  character(*), parameter, public :: VALUE_INDICATOR = ':'
  character(*), parameter, public :: ALIAS_INDICATOR = '*'
  character(*), parameter, public :: ANCHOR_INDICATOR = '&'
  character(*), parameter, public :: TAG_INDICATOR = '!'
  character(*), parameter, public :: COMMENT_INDICATOR = '#'

  character(*), parameter, public :: FLOW_SEQUENCE_START_INDICATOR = '['
  character(*), parameter, public :: FLOW_SEQUENCE_END_INDICATOR = ']'

  character(*), parameter, public :: BLOCK_NEXT_ENTRY_INDICATOR = '-'

  character(*), parameter, public :: FLOW_MAPPING_START_INDICATOR = '{'
  character(*), parameter, public :: FLOW_MAPPING_END_INDICATOR = '}'
  character(*), parameter, public :: FLOW_NEXT_ENTRY_INDICATOR = ','


  character(*), parameter, public :: KEY_INDICATOR = '?'
  character(*), parameter, public :: DIRECTIVE_INDICATOR = '%'

  character(*), parameter, public :: LITERAL_SCALAR_INDICATOR = '|'
  character(*), parameter, public :: FOLDED_SCALOR_INDICATOR = '>'
  character(*), parameter, public :: SINGLE_QUOTED_SCALAR_INDICATOR = "'"
  character(*), parameter, public :: DOUBLE_QUOTED_SCALAR_INDICATOR = '"'

contains

  function new_StreamStartToken() result(token)
    type (StreamStartToken) :: token
    call token%set_id('<stream start>')
  end function new_StreamStartToken
  
  function new_StreamEndToken() result(token)
    type (StreamEndToken) :: token
    call token%set_id('<stream end>')
  end function new_StreamEndToken

  function new_NullToken() result(token)
    type(NullToken) :: token
    call token%set_id('<null>')
  end function new_NullToken

  function new_ScalarToken(value, is_plain, style) result(token)
    type(ScalarToken) :: token
    character(*), intent(in) :: value
    logical, intent(in) :: is_plain
    character, optional, intent(in) :: style

    call token%set_id('<scalar>')
    token%value = value
    token%is_plain = is_plain
    if (present(style)) then
       token%style = style
    else
       token%style = ''
    end if

  end function new_ScalarToken


  function new_BlockSequenceStartToken() result(token)
    type(BlockSequenceStartToken) :: token
    call token%set_id('<block sequence start>')
  end function new_BlockSequenceStartToken


  function new_BlockNextEntryToken() result(token)
    type(BlockNextEntryToken) :: token
    call token%set_id(BLOCK_NEXT_ENTRY_INDICATOR)
  end function new_BlockNextEntryToken

  function new_BlockEndToken() result(token)
    type(BlockEndToken) :: token
    call token%set_id('<block end>')
  end function new_BlockEndToken


  function new_DocumentStartToken() result(token)
    type(DocumentStartToken) :: token
    call token%set_id('<document start>')
  end function new_DocumentStartToken

  function new_DocumentEndToken() result(token)
    type(DocumentEndToken) :: token
    call token%set_id('<document end>')
  end function new_DocumentEndToken


  function new_FlowSequenceStartToken() result(token)
    type(FlowSequenceStartToken) :: token
    call token%set_id(FLOW_SEQUENCE_START_INDICATOR)
  end function new_FlowSequenceStartToken

  function new_FlowSequenceEndToken() result(token)
    type(FlowSequenceEndToken) :: token
    call token%set_id(FLOW_SEQUENCE_END_INDICATOR)
  end function new_FlowSequenceEndToken

  function new_FlowMappingStartToken() result(token)
    type(FlowMappingStartToken) :: token
    call token%set_id(FLOW_MAPPING_START_INDICATOR)
  end function new_FlowMappingStartToken

  function new_FlowMappingEndToken() result(token)
    type(FlowMappingEndToken) :: token
    call token%set_id(FLOW_MAPPING_END_INDICATOR)
  end function new_FlowMappingEndToken

  function new_FlowNextEntryToken() result(token)
    type(FlowNextEntryToken) :: token
    call token%set_id(FLOW_NEXT_ENTRY_INDICATOR)

  end function new_FlowNextEntryToken

  function new_KeyToken() result(token)
    type(KeyToken) :: token
    call token%set_id(KEY_INDICATOR)
  end function new_KeyToken

  function new_ValueToken() result(token)
    type(ValueToken) :: token
    call token%set_id(VALUE_INDICATOR)
  end function new_ValueToken

  function new_BlockMappingStartToken() result(token)
    type(BlockMappingStartToken) :: token
    call token%set_id('<block mapping start>')
  end function new_BlockMappingStartToken


  function get_id(this) result(id)
    character(:), allocatable :: id
    class(AbstractToken), intent(in) :: this
    id = this%id
  end function get_id

  subroutine set_id(this, id)
    class(AbstractToken), intent(inout) :: this
    character(*), intent(in) :: id
    this%id = id
  end subroutine set_id


  function new_AliasToken(value) result(token)
     type(AliasToken) :: token
     character(*), intent(in) :: value

     call token%set_id('<alias>')
     token%value = value

  end function new_AliasToken



  function new_AnchorToken(value) result(token)
     type(AnchorToken) :: token
     character(*), intent(in) :: value

     call token%set_id('<anchor>')
     token%value = value

  end function new_AnchorToken

end module fy_Tokens
