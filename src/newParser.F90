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
   use fy_MappingNode
   use fy_SequenceNode
   use fy_Mapping
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
      class(AbstractNode), target, allocatable :: node
      class(newParser), intent(inout) :: this
      class(AbstractTextStream), intent(in) :: stream

      type(Lexer) :: lexr

      lexr = Lexer(Reader(stream))
      call this%top(node, lexr)

   end function load_from_stream

   function load_from_file(this, fname) result(node)
      class(AbstractNode), target, allocatable :: node
      class(newParser), intent(inout) :: this
      character(len=*), intent(in) :: fname

      node = this%load(FileStream(fname))

   end function load_from_file

   subroutine top(this, node, lexr)
      class(newParser), intent(inout) :: this
      class(AbstractNode), allocatable, target, intent(out) :: node
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
            print*,__FILE__,__LINE__,' is block sequence'
            allocate(node,source=SequenceNode())
            seq => to_sequence(node)
            call this%process_sequence(seq, lexr)
            done = .true.
            block
              class(AbstractNode), pointer :: ptr
              ptr => seq%front()
              print*,__FILE__,__LINE__, 'subsequence: ', ptr
              ptr => seq%back()
              print*,__FILE__,__LINE__, 'subsequence: ', ptr
            end block
            print*,__FILE__,__LINE__, node
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

      if (.not. done) error stop 'not done'

   end subroutine top


   recursive subroutine process_sequence(this, seq, lexr)
      class(newParser), target, intent(inout) :: this
      type(Sequence), intent(inout) :: seq
      type(Lexer), intent(inout) :: lexr 

      class(AbstractToken), allocatable :: token
      logical :: expect_another
      character(:), allocatable :: anchor

      type(sequence), pointer :: subsequence
      type(Mapping), pointer :: submapping
      type(SequenceNode) :: ppp

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
            error stop 'improper AliasToken'
         end select

         select type (token)
         type is (ScalarToken)
            call seq%push_back(this%interpret(token))
         type is (BlockSequenceStartToken)
!!$            call seq%push_back(SequenceNode())
            call seq%push_back(ppp)
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

      print*,__FILE__,__LINE__, 'result sequence is size: ', seq%size()

   end subroutine process_sequence


   recursive subroutine process_mapping(this, map, lexr)
      class(newParser), target, intent(inout) :: this
      type(Mapping), target, intent(inout) :: map
      type(Lexer), intent(inout) :: lexr

      class(AbstractToken), allocatable :: token
      logical :: expect_another
      character(:), allocatable :: key_str
      class(AbstractNode), allocatable :: key
      class(AbstractToken), allocatable :: next_token
      character(:), allocatable :: anchor, alias
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
               error stop 'expected ValueToken'
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
               alias = q%value
               if (this%anchors%count(alias) > 0) then
                  if (key_str == MERGE_KEY) then
                     !TODO - should throw exception if not mapping ...
                     anchor_mapping => to_mapping(this%anchors%of(alias))
                     call merge(map, anchor_mapping)
                  else
                     call map%insert(key, this%anchors%of(alias))
                  end if
                  deallocate(alias)
                  cycle
               else
                  error stop "no such anchor: <"//alias//">"
               end if
            end select

            call process_value(map, key, next_token)

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

!!$      ! Odd workaround for gfortran 10.3
!!$      if (allocated(anchor)) deallocate(anchor)
!!$      if (allocated(key)) deallocate(key)


   contains

      recursive subroutine process_value(map, key, token)
         type(Mapping), intent(inout) :: map
         class(AbstractNode), intent(in) :: key
         class(AbstractToken), intent(in) :: token

         select type(token)
         type is (ScalarToken)
            call map%insert(key, this%interpret(token))
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
         return
      end subroutine process_value

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
         class(AbstractNode), intent(out), allocatable :: key
         character(:), allocatable, intent(out) :: key_str
         select type(next_token)
         type is (ScalarToken)
            key_str = next_token%value
            allocate(key, source=this%interpret(next_token))
         class default
            error stop 'expected ScalarToken'
         end select
      end subroutine get_key

   end subroutine process_mapping



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
         ! anything else is a string (workaround for gFortran)
         value = StringNode(text)
      end if

   end function interpret

end module fy_newParser
