#define HERE if(depth == 1) print*,__FILE__,__LINE__,depth


!!! The Parser imports a sequence of tokens and constructs an object
!!! that is a subclass of YAML_Node. I naively expect this to be
!!! rather simple compared to the Lexer, but reading suggests that it
!!! should be the opposite.  The difference may in part be that this
!!! package restricts keys to be simple strings.

module fy_Parser
   use fy_Lexer
   use fy_Tokens
   use fy_Reader
   use fy_AbstractTextStream

   use fy_YAML_Node
   use fy_StringNodeMap
   use fy_mappingNode
   use fy_SequenceNode
   use fy_mapping
   use fy_Sequence
   use fy_StringNode
   use fy_IntNode
   use fy_FileStream

   use fy_AbstractSchema
   use fy_FailsafeSchema
   use fy_JSONSchema
   use fy_CoreSchema
   use iso_fortran_env, only: OUTPUT_UNIT
   implicit none
   private

   public :: Parser

   type :: Parser
      private
      class(AbstractSchema), allocatable :: schema
      type (StringNodeMap) :: anchors
   contains
      procedure :: load_from_file
      procedure :: load_from_stream
      generic   :: load => load_from_file, load_from_stream
      procedure :: load_file
      procedure :: load_stream
      procedure :: top
      procedure :: top_load
      procedure :: process_sequence
      procedure :: process_mapping
      procedure :: process_map_key
      procedure :: process_value
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
      p%anchors = StringNodeMap()

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

   function load_from_stream(this, stream) result(node)
      class(YAML_Node), allocatable :: node
      class(Parser), intent(inout) :: this
      class(AbstractTextStream), intent(in) :: stream

      type(Lexer) :: lexr

      lexr = Lexer(Reader(stream))
      node = this%top(lexr)

   end function load_from_stream


   function load_from_file(this, fname) result(node)
      class(YAML_Node), allocatable :: node
      class(Parser), intent(inout) :: this
      character(len=*), intent(in) :: fname

      node = this%load(FileStream(fname))

   end function load_from_file

   subroutine load_stream(this, stream, node)
      class(YAML_Node), allocatable, intent(out) :: node
      class(Parser), intent(inout) :: this
      class(AbstractTextStream), intent(in) :: stream

      type(Lexer) :: lexr

      lexr = Lexer(Reader(stream))
      call this%top_load(lexr, node)

   end subroutine load_stream

   subroutine load_file(this, fname, node)
      class(YAML_Node), allocatable, intent(out) :: node
      class(Parser), intent(inout) :: this
      character(len=*), intent(in) :: fname

      call this%load_stream(FileStream(fname), node)

   end subroutine load_file

   function top(this, lexr) result(node)
      class(YAML_Node), target, allocatable :: node
      class(Parser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr

      call this%top_load(lexr, node)

   end function top

   subroutine top_load(this, lexr, node)
      class(YAML_Node), target, allocatable, intent(out) :: node
      class(Parser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr

      logical :: done
      class(AbstractToken), allocatable :: token
      type(mapping), pointer :: map
      type(sequence), pointer :: seq

      done = .false.

      do

         token = lexr%get_token()

         select type (token)
         type is (StreamStartToken)
            ! no-op
         type is (StreamEndToken)
            exit
         type is (DocumentStartToken)
            ! no-op
         type is (DocumentEndToken)
            exit
         type is (ScalarToken)
            node = this%interpret(token)
            done = .true.
         type is (FlowSequenceStartToken)
            node = SequenceNode()
            seq => to_sequence(node)
            call this%process_sequence(lexr, seq)
            done = .true.
         type is (BlockSequenceStartToken)
            node = SequenceNode()
            seq => to_sequence(node)
            call this%process_sequence(lexr, seq)
            done = .true.
         type is (BlockMappingStartToken)
            node = mappingNode()
            map => to_mapping(node)
            call this%process_mapping(lexr, map, node)
            done = .true.
         type is (FlowMappingStartToken)
            node = mappingNode()
            map => to_mapping(node)
            call this%process_mapping(lexr, map, node)
            done = .true. 
         class default
            error stop 'unsupported token type in top'
         end select
         deallocate(token)
      end do

      if (.not. done) error stop 'not done'

   end subroutine top_load


   recursive subroutine process_sequence(this, lexr, seq)
      class(Parser), target, intent(inout) :: this
      type(Lexer), intent(inout) :: lexr 
      type(sequence), intent(inout) :: seq

      class(AbstractToken), allocatable :: token, token_2
      logical :: expect_another
      character(:), allocatable :: anchor
      type(mapping), pointer :: map
      class(YAML_Node), pointer :: subnode
      type(Sequence), pointer :: subseq

!!$      integer :: depth = 0
!!$      depth = depth + 1

      expect_another = .false.

      do

         token = lexr%get_token()


         select type(q => token)
         type is (AnchorToken)
            anchor = q%value
            deallocate(token)
            token = lexr%get_token()
         type is (AliasToken)
            error stop 'improper AliasToken'
         end select

         select type (token)
         type is (ScalarToken)
            call seq%push_back(this%interpret(token))
         type is (BlockSequenceStartToken)
            call seq%push_back(SequenceNode())
            subnode => seq%back()
            subseq => to_sequence(subnode)
            call this%process_sequence(lexr, subseq)
         type is (FlowSequenceStartToken)
            call seq%push_back(SequenceNode())
            subnode => seq%back()
            subseq => to_sequence(subnode)
            call this%process_sequence(lexr, subseq)
         type is (FlowNextEntryToken)
            expect_another = .true.
         type is (BlockNextEntryToken)
            expect_another = .true.
         type is (FlowSequenceEndToken)
            ! TODO must match block/flow 
  !           if (expect_another) then
  !              __ASSERT__("dangling comma in flow sequence", expect_another)
  !           else
  !              exit
  !           end if
            exit
         type is (BlockEndToken)
            ! TODO must match block/flow 
            exit
         type is (FlowMappingStartToken)
            call seq%push_back(mappingNode())
            subnode => seq%back()
            map => to_mapping(subnode)
            call this%process_mapping(lexr, map)
         type is (BlockMappingStartToken)
            call seq%push_back(mappingNode())
            subnode => seq%back()
            map => to_mapping(subnode)
            call this%process_mapping(lexr, map)
         class default
            error stop 'illegal token encountered A'
         end select
         deallocate(token)

         if (allocated(anchor)) then
            call this%anchors%insert(anchor, seq%back())
            deallocate(anchor)
         end if

      end do
!!$      depth = depth - 1

   end subroutine process_sequence


   recursive subroutine process_mapping(this, lexr, map, mnode)
      type(mapping), intent(inout) :: map
      class(Parser), target, intent(inout) :: this
      type(Lexer), intent(inout) :: lexr
      class(YAML_Node), optional, intent(inout) :: mnode

!!$      integer :: depth = 0
!!$      depth = depth + 1

      map = mapping()
      do
         associate (token => lexr%get_token())
           select type (token)
           type is (ScalarToken)
              ! no-op
           type is (KeyToken)
              call this%process_map_key(lexr, map, mnode)
           type is (FlowNextEntryToken)
              ! no-op
           type is (FlowMappingEndToken)
              exit
           type is (BlockEndToken)
              exit
           class default
              error stop 'illegal token encountered B'
           end select
         end associate

      end do

!!$      depth = depth - 1
   end subroutine process_mapping

   recursive subroutine process_map_key(this, lexr, map, mnode)
      class(Parser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr
      type(mapping), intent(inout) :: map
      class(YAML_Node), optional, intent(inout) :: mnode

      character(:), allocatable :: anchor, alias
      character(:), allocatable :: key_str
      type(mapping), pointer :: anchor_mapping
      class(AbstractToken), allocatable :: token_2, token_3, token_4

      ! Wrapper is needed here as a workaround for GFortran (11.2) problem
      ! with recursions and local variables that are abstract & allocatable.
      ! At least that is the current theory.  Hard to pin down.
      type :: Wrapper
         class(YAML_Node), allocatable :: node
      end type Wrapper
      type(Wrapper) :: key_wrap
      type(Wrapper) :: value_wrap
      
!!$      integer :: depth = 0
!!$      depth = depth + 1


      associate (token => lexr%get_token())

          key_wrap%node = get_key(this, token, key_str)

          associate (value_token => lexr%get_token())
            if (value_token%get_id() /= VALUE_INDICATOR) then
               error stop 'expected ValueToken'
            end if
          end associate
          token_2 = lexr%get_token()
          
          ! Possible anchor or alias?
          select type(q => token_2)
          type is (AnchorToken)
             anchor = q%value
             token_3 = lexr%get_token()
          type is (AliasToken)
             token_3 = token_2
             alias = q%value
             if (this%anchors%count(alias) > 0) then
                if (key_str == MERGE_KEY) then
                   !TODO - should throw exception if not mapping ...
                   anchor_mapping => to_mapping(this%anchors%of(alias))
                   call merge(map, anchor_mapping)
                else
                   call map%insert(key_wrap%node, this%anchors%of(alias))
                end if
                deallocate(alias)
                return
             else
                error stop "no such anchor: <"//alias//">"
             end if
          class default
             token_3 = token_2
          end select

          call this%process_value(token_3, lexr, value_wrap%node)

          call map%insert(key_wrap%node, value_wrap%node)

          if (allocated(anchor)) then
             call this%anchors%insert(anchor, value_wrap%node)
             deallocate(anchor)
          end if

     end associate

!!$      depth = depth - 1

   contains

      recursive subroutine merge(m1, m2)
         use fy_String
         use gftl_UnlimitedVector
         type(mapping), intent(inout) :: m1
         type(mapping), intent(in) :: m2

         type (mappingIterator) :: iter

         class(YAML_Node), pointer :: key, value

         iter = m2%begin()
         do while (iter /= m2%end())
            key => iter%first()
            if (m1%count(key) == 0) then
               value => iter%second()
               call m1%insert(key, value)
            end if
            call iter%next()
         end do

      end subroutine merge

   end subroutine process_map_key

   function get_key(this, token, key_str) result(key)
      class(YAML_Node), allocatable :: key
      class(Parser), intent(in) :: this
      class(AbstractToken), intent(in) :: token
      character(:), allocatable, intent(out) :: key_str

      select type(token)
      type is (ScalarToken)
         key_str = token%value
         key = this%interpret(token)
      class default
         error stop 'expected ScalarToken'
      end select

   end function get_key


   recursive subroutine process_value(this, token, lexr, value)
      class(Parser), intent(inout) :: this
      class(AbstractToken), intent(in) :: token
      type(Lexer), intent(inout) :: lexr
      class(YAML_Node), allocatable, target, intent(out) :: value

      type(mapping), pointer :: map
      type(sequence), pointer :: seq

!!$      integer, save :: depth =0
!!$      depth = depth + 1

      select type(token)
      type is (ScalarToken)
         value = this%interpret(token)
      type is (FlowSequenceStartToken)
         value = SequenceNode()
         seq => to_sequence(value)
         call this%process_sequence(lexr, seq)
      type is (BlockSequenceStartToken)
         value = SequenceNode()
         seq => to_sequence(value)
         call this%process_sequence(lexr, seq)
      type is (FlowMappingStartToken)
         value = MappingNode()
         map => to_mapping(value)
         call this%process_mapping(lexr, map, value)
      type is (BlockMappingStartToken)
         value = MappingNode()
         map => to_mapping(value)
         call this%process_mapping(lexr, map, value)
      class default
         error stop 'illegal token encountered C'
      end select

!!$      depth = depth - 1
      return
   end subroutine process_value



   function interpret(this, scalar) result(value)
      use fy_BoolNode
      use fy_IntNode
      use fy_FloatNode

      class(YAML_Node), allocatable :: value
      class(Parser), intent(in) :: this
      type(ScalarToken) :: scalar

      character(:), allocatable :: text

      text = scalar%value

      if (any(scalar%style == ['"',"'"])) then
         value = StringNode(text)
 !   elseif (this%schema%matches_null(text)) then
 !      value = NullNode()
      elseif (this%schema%matches_logical(text)) then
         value = BoolNode(this%schema%to_logical(text))
      elseif (this%schema%matches_integer(text)) then
         value = IntNode(this%schema%to_integer(text))
      elseif(this%schema%matches_real(text)) then
         value = FloatNode(this%schema%to_real(text))
      else
         ! anything else is a string
         value = StringNode(text)
      end if

   end function interpret

end module fy_Parser
