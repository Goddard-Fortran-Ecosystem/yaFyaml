!!! The Parser imports a sequence of tokens and constructs a
!!! configuration object.  I naively expect this to be rather simple
!!! compared to the Lexer, but reading suggests that it should be the
!!! opposite.  The difference may in part be that this package restricts
!!! keys to be simple strings.  

module fy_newParser
   use fy_Lexer
   use fy_Tokens
   use fy_Reader
   use fy_AbstractTextStream

   use fy_AbstractNode
   use fy_StringNodeMap
   use fy_MappingNode
   use fy_SequenceNode
   use fy_Mapping
   use fy_Sequence
   use fy_StringNode
   use fy_IntNode
   use fy_NewConfiguration

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


   function load(this, stream) result(cfg)
      type(Configuration) :: cfg
      class(Parser), intent(inout) :: this
      class(AbstractTextStream), intent(in) :: stream

      type(Lexer) :: lexr
      class(AbstractNode), pointer :: node

      lexr = Lexer(Reader(stream))

      call this%top(node, lexr)
      cfg = Configuration(node)
      nullify(node) ! not deallocate !

   end function load


   subroutine top(this, node, lexr)
      class(Parser), intent(inout) :: this
      class(AbstractNode), pointer, intent(out) :: node
      type(Lexer), intent(inout) :: lexr

      class(AbstractToken), allocatable :: token
      logical :: done
      type(sequence), pointer :: seq
      type(Mapping), pointer :: map

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
            allocate(node, source=this%interpret(token))
            done = .true.
         type is (FlowSequenceStartToken)
            allocate(node, source=SequenceNode())
            seq => to_sequence(node)
            call this%process_sequence(seq, lexr)
            done = .true.
         type is (BlockSequenceStartToken)
            allocate(node,source=SequenceNode())
            seq => to_sequence(node)
            call this%process_sequence(seq, lexr)
            done = .true.
         type is (BlockMappingStartToken)
            allocate(node,source=MappingNode())
            map => to_mapping(node)
            call this%process_mapping(map, lexr)
            done = .true.
         type is (FlowMappingStartToken)
            allocate(node,source=MappingNode())
            map => to_mapping(node)
            call this%process_mapping(map, lexr)
            done = .true.
         class default
            error stop 'unsupported token type in top'
         end select
      end do

      if (.not. done) error stop

   end subroutine top


   recursive subroutine process_sequence(this, seq, lexr)
      class(Parser), target, intent(inout) :: this
      type(Sequence), intent(inout) :: seq
      type(Lexer), intent(inout) :: lexr 

      class(AbstractToken), allocatable :: token
      logical :: expect_another
      character(:), allocatable :: anchor

      type(sequence), pointer :: subsequence
      type(Mapping), pointer :: submapping

      expect_another = .false.
      do
         if (allocated(token)) deallocate(token)
         token = lexr%get_token()

         select type(q => token)
         type is (AnchorToken)
            anchor = q%value
            if (allocated(token)) deallocate(token)
            token = lexr%get_token()
         type is (AliasToken)
            error stop
         end select

         select type (token)
         type is (ScalarToken)
            call seq%push_back(this%interpret(token))
         type is (BlockSequenceStartToken)
            call seq%push_back(SequenceNode())
            subsequence => to_sequence(seq%back())
            call this%process_sequence(subsequence, lexr)
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

         type is (FlowSequenceStartToken)
            call seq%push_back(SequenceNode())
            subsequence => to_sequence(seq%back())
            call this%process_sequence(subsequence, lexr)

         type is (FlowMappingStartToken)
            call seq%push_back(MappingNode())
            submapping => to_mapping(seq%back())
            call this%process_mapping(submapping, lexr)
         type is (BlockMappingStartToken)
            call seq%push_back(MappingNode())
            submapping => to_mapping(seq%back())
            call this%process_mapping(submapping, lexr)
         class default
            error stop 'illegal token encountered A'
         end select


         if (allocated(anchor)) then
            call this%anchors%insert(anchor, seq%back())
            deallocate(anchor)
         end if
         deallocate(token)

      end do

      if (allocated(anchor)) deallocate(anchor)

   end subroutine process_sequence


   recursive subroutine process_mapping(this, map, lexr)
      class(Parser), target, intent(inout) :: this
      type(Mapping), target, intent(inout) :: map
      type(Lexer), intent(inout) :: lexr

      class(AbstractToken), allocatable :: token
      logical :: expect_another
      character(:), allocatable :: key_str
      class(AbstractNode), allocatable :: key
      class(AbstractToken), allocatable :: next_token
      character(:), allocatable :: anchor
      type(Mapping), pointer :: anchor_mapping

      type(sequence), pointer :: subsequence
      type(Mapping), pointer :: submapping

      expect_another = .false.
      do
         if (allocated(token)) deallocate(token)
         token = lexr%get_token()

         select type (token)
         type is (ScalarToken)
         type is (KeyToken)
            if (allocated(next_token)) deallocate(next_token)
            next_token = lexr%get_token()
            call get_key(next_token, key, key_str)
            if (allocated(next_token)) deallocate(next_token)
            next_token = lexr%get_token()
!!$            allocate(next_token, source=lexr%get_token())
            select type(next_token)
            type is (ValueToken)
               ! mandatory before value
            class default
               error stop
            end select
            if (allocated(next_token)) deallocate(next_token)
            next_token = lexr%get_token()

            ! Possible anchor or alias?
            select type(q => next_token)
            type is (AnchorToken)
               anchor = q%value
               if (allocated(next_token)) deallocate(next_token)
               next_token = lexr%get_token()
            type is (AliasToken)
               anchor = q%value
               if (this%anchors%count(anchor) > 0) then
                  if (key_str == MERGE_KEY) then
                     !TODO - should throw exception if not mapping ...
                     anchor_mapping => to_mapping(this%anchors%of(anchor))
                     call merge(map, anchor_mapping)
                     deallocate(anchor)
                     cycle
                  else
                     call map%insert(key, this%anchors%of(anchor))
                     deallocate(anchor)
                     cycle
                  end if
               else
                  error stop "no such anchor"
               end if
            end select

            select type(next_token)
            type is (ScalarToken)
               call map%insert(key, this%interpret(next_token))
            type is (FlowSequenceStartToken)
               call map%insert(key,SequenceNode())
               subsequence => to_sequence(map%of(key))
               call this%process_sequence(subsequence, lexr)
            type is (FlowMappingStartToken)
                 call map%insert(key,MappingNode())
               submapping => to_mapping(map%of(key))
               call this%process_mapping(submapping, lexr)
            type is (BlockSequenceStartToken)
               call map%insert(key,SequenceNode())
               subsequence => to_sequence(map%of(key))
               call this%process_sequence(subsequence, lexr)
            type is (BlockMappingStartToken)
                 call map%insert(key,MappingNode())
               submapping => to_mapping(map%of(key))
               call this%process_mapping(submapping, lexr)
            class default
               error stop 'illegal token encountered C'
            end select

            if (allocated(anchor)) then
               call this%anchors%insert(anchor, map%of(key))
               deallocate(anchor)
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

      ! Odd workaround for gfortran 10.3
      if (allocated(anchor)) deallocate(anchor)
      if (allocated(key)) deallocate(key)


   contains

      subroutine merge(m1, m2)
         use fy_String
         use gftl_UnlimitedVector
         type(Mapping), intent(inout) :: m1
         type(Mapping), intent(in) :: m2

         type (MappingIterator) :: iter

         class(AbstractNode), pointer :: key, value

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

      subroutine get_key(token, key, key_str)
         class(AbstractToken), intent(in) :: token
         class(AbstractNode), allocatable :: key
         character(:), allocatable, intent(out) :: key_str
         select type(next_token)
         type is (ScalarToken)
            key_str = next_token%value
            call interpret2(this, next_token, key)
         class default
            error stop
         end select
      end subroutine get_key

   end subroutine process_mapping



   function interpret(this, scalar) result(value)
      use fy_BoolNode
      use fy_IntNode
      use fy_FloatNode

      class(AbstractNode), allocatable :: value
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
         ! anything else is a string (workaround for gFortran)
         value = StringNode(text)
      end if

   end function interpret

   subroutine  interpret2(this, scalar, value)
      use fy_BoolNode
      use fy_IntNode
      use fy_FloatNode

      class(Parser), intent(in) :: this
      type(ScalarToken) :: scalar
      class(AbstractNode), intent(out), allocatable :: value

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
         ! anything else is a string (workaround for gFortran)
         value = StringNode(text)
      end if

   end subroutine interpret2

end module fy_newParser
