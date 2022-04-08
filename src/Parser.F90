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
      procedure :: top
      procedure :: process_sequence
      procedure :: process_sequencen
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

   function top(this, lexr) result(node)
      class(YAML_Node), target, allocatable :: node
      class(Parser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr

      class(AbstractToken), allocatable :: token
      logical :: done
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
            call this%process_mapping(lexr, map)
            done = .true.
         type is (FlowMappingStartToken)
            node = mappingNode()
            map => to_mapping(node)
            call this%process_mapping(lexr, map)
            done = .true. 
         class default
            error stop 'unsupported token type in top'
         end select
         deallocate(token)
      end do

      if (.not. done) error stop 'not done'

   end function top


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

      integer :: depth = 0

      depth = depth + 1
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
      depth = depth - 1

   end subroutine process_sequence

   recursive subroutine process_sequencen(this, lexr, snode)
      class(Parser), target, intent(inout) :: this
      type(Lexer), intent(inout) :: lexr 
      type(SequenceNode), target, intent(inout) :: snode

      class(AbstractToken), allocatable :: token, token_2
      logical :: expect_another
      character(:), allocatable :: anchor
      type(mapping), pointer :: map
      class(YAML_Node), pointer :: subnode
      type(Sequence), pointer :: seq
      type(Sequence), pointer :: subseq

      integer :: depth = 0

      depth = depth + 1

      seq => to_sequence(snode)

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
      depth = depth - 1

   end subroutine process_sequencen



   recursive subroutine process_mapping(this, lexr, map)
      type(mapping), intent(inout) :: map
      class(Parser), target, intent(inout) :: this
      type(Lexer), intent(inout) :: lexr

      integer :: depth = 0

      depth = depth + 1

      map = mapping()
      do
         associate (token => lexr%get_token())
           select type (token)
           type is (ScalarToken)
              ! no-op
           type is (KeyToken)
              call process_map_key(this, lexr, map)
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

      depth = depth - 1
   end subroutine process_mapping

   recursive subroutine process_map_key(this, lexr, map)
      class(Parser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr
      type(mapping), intent(inout) :: map

      character(:), allocatable :: anchor, alias
      character(:), allocatable :: key_str
      type(mapping), pointer :: anchor_mapping
      class(AbstractToken), allocatable :: token_2, token_3, token_4

      type(sequence), save :: keys, values
      integer :: depth = 0
      class(YAML_Node), allocatable :: val
      class(YAML_Node), pointer :: curr_key
      class(YAML_Node), pointer :: curr_value

      depth = depth + 1

      associate (token => lexr%get_token())
        call keys%push_back(get_key(this, token, key_str))
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
                   call map%insert(keys%back(), this%anchors%of(alias))
                end if
                deallocate(alias)
                return
             else
                error stop "no such anchor: <"//alias//">"
             end if
          class default
             token_3 = token_2
          end select
          curr_key => keys%back()
          call process_value(this, keys%back(), token_3, lexr, values)
          curr_key => keys%back()
          curr_value => values%back()

            call map%insert(keys%back(), curr_value)

          curr_key => keys%back()
          if (allocated(anchor)) then
             call this%anchors%insert(anchor, map%of(curr_key))
             deallocate(anchor)
          end if

          call keys%pop_back()
          call values%pop_back()
     end associate

      depth = depth - 1

   contains

      subroutine merge(m1, m2)
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


   recursive subroutine process_value(this, key, token, lexr, values)
      class(Parser), intent(inout) :: this
      class(YAML_Node), intent(in) :: key
      class(AbstractToken), intent(in) :: token
      type(Lexer), intent(inout) :: lexr
      type(sequence), intent(inout) :: values

      type(mapping), pointer :: map
      type(sequence), pointer :: seq
      integer :: depth =0

      class(YAML_Node), pointer :: subnode

      depth = depth + 1
      select type(token)
      type is (ScalarToken)
         call values%push_back(this%interpret(token))
      type is (FlowSequenceStartToken)
         call values%push_back(SequenceNode())
         subnode => values%back()
         seq => to_sequence(subnode)
         call this%process_sequence(lexr, seq)
      type is (BlockSequenceStartToken)
         call values%push_back(SequenceNode())
         subnode => values%back()
         seq => to_sequence(subnode)
         call this%process_sequence(lexr, seq)
      type is (FlowMappingStartToken)
         call values%push_back(mappingNode())
         subnode => values%back()
         map => to_mapping(subnode)
         call this%process_mapping(lexr, map)
         subnode => values%back()
      type is (BlockMappingStartToken)
         call values%push_back(mappingNode())
         subnode => values%back()
         map => to_mapping(subnode)
         call this%process_mapping(lexr, map)
      class default
         error stop 'illegal token encountered C'
      end select

      subnode => values%back()
      depth = depth - 1
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
