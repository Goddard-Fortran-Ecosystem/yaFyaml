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
    class(AbstractToken), allocatable :: next_token

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

          type is (FlowSequenceEndToken)
!!$             if (expect_another) then
!!$                __ASSERT__("dangling comma in flow sequence", expect_another)
!!$             else
!!$                exit
!!$             end if
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
    integer :: i
    real :: x
    
    text = scalar%value
    print*,__FILE__,__LINE__,'interpreting: <',text,'>'
    select case (this%schema)
    case ('JSON')
       if (text == 'true') then
          value = .true.
          print*,__FILE__,__LINE__,'interpreted as logical with value .true.'
          return
       end if
       if (text == 'false') then
          value = .false.
          print*,__FILE__,__LINE__,'interpreted as logical with value .false.'
          return
       end if
       read(text,*,iostat=status) i
       if (status == 0) then
          value = i
          print*,__FILE__,__LINE__,'interpreted as integer with value',i
          return
       end if
       read(status,*,iostat=status) x
       if (status == 0) then
          value = x
          return
       end if
       value = text
    end select
    
  end function interpret


  
end module fy_Parser
