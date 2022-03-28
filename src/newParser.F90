#define HERE if(depth == 1) print*,__FILE__,__LINE__,depth


!!! The Parser imports a sequence of tokens and constructs an object
!!! that is a subclass of AbstractNode. I naively expect this to be
!!! rather simple compared to the Lexer, but reading suggests that it
!!! should be the opposite.  The difference may in part be that this
!!! package restricts keys to be simple strings.

module fy_newParser
   use fy_Lexer
   use fy_Tokens
   use fy_Reader
   use fy_AbstractTextStream

   use fy_AbstractNode
   use fy_StringNodeMap
   use fy_newMappingNode
   use fy_SequenceNode
   use fy_newMapping
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

   public :: newParser

   type :: newParser
      private
      class(AbstractSchema), allocatable :: schema
      type (StringNodeMap) :: anchors
      
   contains
      procedure :: load_from_file
      procedure :: load_from_stream
      generic   :: load => load_from_file, load_from_stream
      procedure :: top
      procedure :: process_sequence
      procedure :: process_mapping
      procedure :: interpret
   end type newParser


   interface newParser
      module procedure new_Parser_default
      module procedure new_Parser_schema
      module procedure new_Parser_schema_name
   end interface newParser

   character(*), parameter :: MERGE_KEY = '<<'

contains

   function new_Parser_default() result(p)
      type(newParser) :: p

      p = newParser(CoreSchema())

   end function new_Parser_default

   function new_Parser_schema(schema) result(p)
      type(newParser) :: p
      class(AbstractSchema), intent(in) :: schema

      p%schema = schema
      p%anchors = StringNodeMap()

   end function new_Parser_schema

   function new_Parser_schema_name(schema_name) result(p)
      type(newParser) :: p
      character(*), intent(in) :: schema_name

      select case (schema_name)
      case ('json','JSON')
         p = newParser(JSONSchema())
      case ('core','Core')
         p = newParser(CoreSchema())
      case ('failsafe','Failsafe')
         p = newParser(FailsafeSchema())
      case default
         error stop "Unknown schema"
      end select

   end function new_Parser_schema_name

   function load_from_stream(this, stream) result(node)
      class(AbstractNode), allocatable :: node
      class(newParser), intent(inout) :: this
      class(AbstractTextStream), intent(in) :: stream

      type(Lexer) :: lexr

      lexr = Lexer(Reader(stream))
      node = this%top(lexr)

   end function load_from_stream


   function load_from_file(this, fname) result(node)
      class(AbstractNode), allocatable :: node
      class(newParser), intent(inout) :: this
      character(len=*), intent(in) :: fname

      node = this%load(FileStream(fname))

   end function load_from_file

   function top(this, lexr) result(node)
      class(AbstractNode), target, allocatable :: node
      class(newParser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr

      class(AbstractToken), allocatable :: token
      logical :: done
      type(newMapping), pointer :: map
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
            print*,__FILE__,__LINE__,'top block sequence'
            node = SequenceNode()
            seq => to_sequence(node)
            call this%process_sequence(lexr, seq)
            print*,__FILE__,__LINE__,'top node', node
            done = .true.
         type is (BlockMappingStartToken)
            node = newMappingNode()
            map => to_newmapping(node)
            call this%process_mapping(lexr, map)
            done = .true.
         type is (FlowMappingStartToken)
            node = newMappingNode()
            map => to_newmapping(node)
            call this%process_mapping(lexr, map)
            done = .true. 
         class default
            error stop 'unsupported token type in top'
         end select
      end do

      if (.not. done) error stop 'not done'

   end function top


   recursive subroutine process_sequence(this, lexr, seq)
      class(newParser), target, intent(inout) :: this
      type(Lexer), intent(inout) :: lexr 
      type(sequence), intent(inout) :: seq

      class(AbstractToken), allocatable :: token
      logical :: expect_another
      character(:), allocatable :: anchor
      type(newMapping), pointer :: map
      class(AbstractNode), pointer :: subnode
      type(Sequence), pointer :: subseq

      integer :: depth = 0

      depth = depth + 1
      expect_another = .false.

      do
         token = lexr%get_token()

         select type(q => token)
         type is (AnchorToken)
            anchor = q%value
            token = lexr%get_token()
         type is (AliasToken)
            error stop 'improper AliasToken'
         end select

         select type (token)
         type is (ScalarToken)
            call seq%push_back(this%interpret(token))
         type is (BlockSequenceStartToken)
            HERE
            call seq%push_back(SequenceNode())
            subnode => seq%back()
            HERE
            subseq => to_sequence(subnode)
            call this%process_sequence(lexr, subseq)
            HERE
            HERE, 'subseq: ', subnode
         type is (FlowSequenceStartToken)
            HERE
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
!!$            call seq%push_back(MappingNode(this%process_mapping(lexr)))
            call seq%push_back(newMappingNode())
            subnode => seq%back()
            map => to_newmapping(subnode)
            call this%process_mapping(lexr, map)
         type is (BlockMappingStartToken)
            call seq%push_back(newMappingNode())
            subnode => seq%back()
            map => to_newmapping(subnode)
            call this%process_mapping(lexr, map)
!!$            call seq%push_back(newMappingNode(this%process_mapping(lexr)))
         class default
            error stop 'illegal token encountered A'
         end select

         if (allocated(anchor)) then
            call this%anchors%insert(anchor, seq%back())
            deallocate(anchor)
         end if

      end do
      HERE,'end of process_sequence()', SequenceNode(seq)
      depth = depth - 1

   end subroutine process_sequence


!!$   recursive function process_mapping(this, lexr) result(map)
!!$      type(newMapping) :: map

   recursive subroutine process_mapping(this, lexr, map)
      type(newMapping), intent(inout) :: map
      class(newParser), target, intent(inout) :: this
      type(Lexer), intent(inout) :: lexr

      integer :: depth = 0

      depth = depth + 1

      map = newMapping()
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
!!$   end function process_mapping
   end subroutine process_mapping

   recursive subroutine process_map_key(this, lexr, map)
      class(newParser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr
      type(newMapping), intent(inout) :: map

      character(:), allocatable :: anchor, alias
      character(:), allocatable :: key_str
      type(newMapping), pointer :: anchor_mapping
      class(AbstractToken), allocatable :: token_2, token_3, token_4

      type(sequence), save :: keys, values
      integer :: depth = 0
      class(AbstractNode), allocatable :: val
      class(AbstractNode), pointer :: curr_key
      class(AbstractNode), pointer :: curr_value

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
                   anchor_mapping => to_newmapping(this%anchors%of(alias))
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
          end if

          call keys%pop_back()
          call values%pop_back()
     end associate

      depth = depth - 1

   contains

      subroutine merge(m1, m2)
         use fy_String
         use gftl_UnlimitedVector
         type(newMapping), intent(inout) :: m1
         type(newMapping), intent(in) :: m2

         type (newMappingIterator) :: iter

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

   end subroutine process_map_key

   function get_key(this, token, key_str) result(key)
      class(AbstractNode), allocatable :: key
      class(Newparser), intent(in) :: this
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
      class(NewParser), intent(inout) :: this
      class(AbstractNode), intent(in) :: key
      class(AbstractToken), intent(in) :: token
      type(Lexer), intent(inout) :: lexr
      type(sequence), intent(inout) :: values

      type(newMapping), pointer :: map
      type(sequence), pointer :: seq
      integer :: depth =0

      class(AbstractNode), pointer :: subnode

      depth = depth + 1
      HERE
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
         call values%push_back(newMappingNode())
         subnode => values%back()
         map => to_newmapping(subnode)
         call this%process_mapping(lexr, map)
         subnode => values%back()
      type is (BlockMappingStartToken)
         call values%push_back(newMappingNode())
         subnode => values%back()
         map => to_newmapping(subnode)
         call this%process_mapping(lexr, map)
      class default
         error stop 'illegal token encountered C'
      end select

      subnode => values%back()
      HERE, 'subnode: ', subnode
      depth = depth - 1
      return
   end subroutine process_value



   function interpret(this, scalar) result(value)
      use fy_BoolNode
      use fy_IntNode
      use fy_FloatNode

      class(AbstractNode), allocatable :: value
      class(newParser), intent(in) :: this
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

end module fy_newParser
