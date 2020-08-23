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
  use fy_None
  use gFTL_UnlimitedVector
  use fy_OrderedStringUnlimitedMap
  use fy_StringUnlimitedPointerMap
  use fy_AbstractSchema
  use fy_FailsafeSchema
  use fy_JSONSchema
  use fy_CoreSchema
  implicit none
  private

  public :: Parser

  type :: Parser
     private
     class(AbstractSchema), allocatable :: schema
     type(StringUnlimitedPointerMap) :: anchors
   contains
     procedure :: load
     procedure :: top
     procedure :: process_sequence
     procedure :: process_mapping
     procedure :: interpret
  end type Parser


  interface Parser
     module procedure new_Parser_default
     module procedure new_Parser_schema
     module procedure new_Parser_schema_name
  end interface Parser

  character(*), parameter :: MERGE_KEY = '<<'

contains

  function new_Parser_default() result(p)
    type(Parser) :: p

    p = Parser(CoreSchema())

  end function new_Parser_default

  function new_Parser_schema(schema) result(p)
    type(Parser) :: p
    class(AbstractSchema), intent(in) :: schema

    p%schema = schema
  end function new_Parser_schema

  function new_Parser_schema_name(schema_name) result(p)
    type(Parser) :: p
    character(*), intent(in) :: schema_name

    select case (schema_name)
    case ('json','JSON')
       p = Parser(JSONSchema())
    case ('core','Core')
       p = Parser(CoreSchema())
    case ('failsafe','Failsafe')
       p = Parser(FailsafeSchema())
    case default
       error stop "Unknown schema"
    end select

  end function new_Parser_schema_name


  function load(this, stream) result(cfg)
    type(Configuration) :: cfg
    class(Parser), intent(inout) :: this
    class(AbstractTextStream), intent(in) :: stream

    type(Lexer) :: lexr

    lexr = Lexer(Reader(stream))

    call this%top(cfg, lexr)

  end function load


  subroutine top(this, cfg, lexr)
    class(Parser), intent(inout) :: this
    type(Configuration), intent(inout) :: cfg
    type(Lexer), intent(inout) :: lexr

    class(AbstractToken), allocatable :: token
    logical :: done
    class(*), pointer :: node

    done = .false.
    do
       if (allocated(token)) deallocate(token)
       token = lexr%get_token()
       select type (token)
       type is (StreamStartToken)
       type is (StreamEndToken)
          exit
       type is (DocumentStartToken)
       type is (DocumentEndToken)
          exit
       type is (ScalarToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(this%interpret(token))
          done = .true.
       type is (FlowSequenceStartToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(scalar=UnlimitedVector())
          call cfg%get_node_at_selector(node)
!!$          call cfg%get_node(node)
          call this%process_sequence(node, lexr)
          done = .true.
       type is (BlockSequenceStartToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(scalar=UnlimitedVector())
          call cfg%get_node_at_selector(node)
!!$          call cfg%get_node(node)
          call this%process_sequence(node, lexr)
          done = .true.
       type is (BlockMappingStartToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(scalar=OrderedStringUnlimitedMap())
          call cfg%get_node_at_selector(node)
!!$          call cfg%get_node(node)
          call this%process_mapping(node, lexr)
          done = .true.
       type is (FlowMappingStartToken)
!!$          __ASSERT__("Configuration can only have one top node.", .not. done)
          cfg = Configuration(scalar=OrderedStringUnlimitedMap())
          call cfg%get_node_at_selector(node)
!!$          call cfg%get_node(node)
          call this%process_mapping(node, lexr)
          done = .true.
       class default
          error stop 'unsupported token type in top'
       end select
    end do

  end subroutine top


  recursive subroutine process_sequence(this, node, lexr)
    class(Parser), intent(inout) :: this
    class(*), pointer, intent(in) :: node ! pointer association is not modified
    type(Lexer), intent(inout) :: lexr 

    class(AbstractToken), allocatable :: token
    logical :: expect_another
    type(Configuration) :: sub
    character(:), allocatable :: anchor


    expect_another = .false.
    select type (q => node)
    type is (UnlimitedVector)
       do
          if (allocated(token)) deallocate(token)
          token = lexr%get_token()

          select type(qq => token)
          type is (AnchorToken)
             anchor = qq%value
             if (allocated(token)) deallocate(token)
             token = lexr%get_token()
          type is (AliasToken)
             print*, 'found alias unexpectedly'
             error stop
          end select

          select type (token)
          type is (ScalarToken)
             call q%push_back(this%interpret(token))

          type is (BlockSequenceStartToken)
             call q%push_back(UnlimitedVector())
             call this%process_sequence(q%back(), lexr)
          type is (FlowNextEntryToken)
             expect_another = .true.
          type is (BlockNextEntryToken)
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
             call q%push_back(UnlimitedVector())
             call this%process_sequence(q%back(), lexr)

          type is (FlowMappingStartToken)
             call q%push_back(OrderedStringUnlimitedMap())
             call this%process_mapping(q%back(), lexr)
          type is (BlockMappingStartToken)
             call q%push_back(OrderedStringUnlimitedMap())
             call this%process_mapping(q%back(), lexr)

          class default
             error stop 'illegal token encountered A'
          end select


          if (allocated(anchor)) then
             block
               class(*), pointer :: ptr
               ptr => q%back()
               call this%anchors%insert(anchor, ptr)
               deallocate(anchor)
             end block
          end if
          deallocate(token)

          
       end do

    class default
       error stop 'inconsistent state in parser:: process_sequence()'

    end select

  end subroutine process_sequence


  recursive subroutine process_mapping(this, node, lexr)
    class(Parser), intent(inout) :: this
    class(*), pointer, intent(in) :: node
    type(Lexer), intent(inout) :: lexr

    class(AbstractToken), allocatable :: token
    logical :: expect_another
    character(:), allocatable :: key
    class(AbstractToken), allocatable :: next_token
    character(:), allocatable :: anchor
    character(:), allocatable :: alias

    expect_another = .false.
    select type (q => node)
    type is (OrderedStringUnlimitedMap)
       do
          if (allocated(token)) deallocate(token)
          token = lexr%get_token()

          select type (token)
          type is (ScalarToken)
          type is (KeyToken)
             if (allocated(next_token)) deallocate(next_token)
             next_token = lexr%get_token()
             select type(next_token)
             type is (ScalarToken)
                key = next_token%value ! always a string
             class default
                error stop
             end select
             if (allocated(next_token)) deallocate(next_token)
             next_token = lexr%get_token()
             select type(next_token)
             type is (ValueToken)
                ! mandatory before value
             class default
                error stop
             end select
             if (allocated(next_token)) deallocate(next_token)
             next_token = lexr%get_token()

             ! Possible anchor or alias?
             select type(qq => next_token)
             type is (AnchorToken)
                anchor = qq%value
                if (allocated(next_token)) deallocate(next_token)
                next_token = lexr%get_token()
             type is (AliasToken)
                anchor = qq%value
                if (this%anchors%count(anchor) > 0) then

                   if (key == MERGE_KEY) then
                      call merge(q, this%anchors%at(anchor))
                      deallocate(anchor)
                      cycle
                   else
                      call q%insert(key, this%anchors%at(anchor))
                      deallocate(anchor)
                      cycle
                   end if
                else
                   error stop "no such anchor"
                end if
             end select

             select type(next_token)
             type is (ScalarToken)
                call q%insert(key, this%interpret(next_token))
             type is (FlowSequenceStartToken)
                call q%insert(key,UnlimitedVector())
                call this%process_sequence(q%at(key), lexr)
             type is (FlowMappingStartToken)
                call q%insert(key,OrderedStringUnlimitedMap())
                call this%process_mapping(q%at(key), lexr)
             type is (BlockSequenceStartToken)
                call q%insert(key,UnlimitedVector())
                call this%process_sequence(q%at(key), lexr)
             type is (BlockMappingStartToken)
                call q%insert(key,OrderedStringUnlimitedMap())
                call this%process_mapping(q%at(key), lexr)
             class default
                error stop 'illegal token encountered C'
             end select
             if (allocated(anchor)) then
                block
                  class(*), pointer :: ptr
                  ptr => q%at(key)
                  call this%anchors%insert(anchor, ptr)
                  deallocate(anchor)
                end block
             end if

          type is (FlowNextEntryToken)
             expect_another = .true.

          type is (FlowMappingEndToken)
             exit
          type is (BlockEndToken)
             exit
          class default
             error stop 'illegal token encountered B'
          end select

       end do
       
    class default
       error stop 'inconsistent state in parser:: process_mapping()'

    end select

 contains

    subroutine merge(m1, q)
       use fy_String
       use fy_OrderedStringUnlimitedMap
       use gftl_UnlimitedVector
       type(OrderedStringUnlimitedMap), intent(inout) :: m1
       class(*), intent(in) :: q

       type (OrderedStringUnlimitedMapIterator) :: iter

       character(:), pointer :: key
       class(*), pointer :: v


       select type (q)
       type is (OrderedStringUnlimitedMap)
          iter = q%begin()
          do while (iter /= q%end())
             key => iter%key()
             if (m1%count(key) == 0) then
                v => iter%value()
                ! Unfortunately, GFortran 9.3 fails with a direct
                ! insert here.  Thus we need to do a select case over
                ! each type/kind even though it results in the exact same
                ! procedure being called in each case.  Sigh.
                select type (v)
                type is (integer)
                   call m1%insert(key, v)
                type is (real)
                   call m1%insert(key, v)
                type is (logical)
                   call m1%insert(key, v)
                type is (character(*))
                   call m1%insert(key, v)
                type is (String)
                   call m1%insert(key, v)
                type is (UnlimitedVector)
                   call m1%insert(key, v)
                type is (OrderedStringUnlimitedMap)
                   call m1%insert(key, v)
                end select
             end if
             call iter%next()
          end do
       end select

    end subroutine merge
    
 end subroutine process_mapping

     

  function interpret(this, scalar) result(value)
     use fy_String
    class(*), allocatable :: value
    class(Parser), intent(in) :: this
    type(ScalarToken) :: scalar

    character(:), allocatable :: text
    integer :: status

    text = scalar%value

    if (any(scalar%style == ['"',"'"])) then
       value = String(text)
    elseif (this%schema%matches_null(text)) then
       value = None
    elseif (this%schema%matches_logical(text)) then
       value = this%schema%to_logical(text)
    elseif (this%schema%matches_logical(text)) then
       value = this%schema%to_logical(text)
    elseif (this%schema%matches_integer(text)) then
       value = this%schema%to_integer(text)
    elseif(this%schema%matches_real(text)) then
       value = this%schema%to_real(text)
    else
       ! anything else is a string (workaround for gFortran)
       value = String(text)
    end if

  end function interpret
  
end module fy_Parser
