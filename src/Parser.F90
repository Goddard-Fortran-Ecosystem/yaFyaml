#define HERE if(depth == 1) print*,__FILE__,__LINE__,depth
#ifdef __GFORTRAN__
#  define _KEY key_wrap%node
#  define _VALUE value_wrap%node
#else
#  define _KEY key
#  define _VALUE value
#endif
#include "error_handling.h"

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
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_KeywordEnforcer

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
      procedure :: top
      procedure :: top_load
      procedure :: process_sequence
      procedure :: process_mapping
      procedure :: process_map_entry
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
      type(CoreSchema) :: c
      c = CoreSchema()
      p = Parser(c)

   end function new_Parser_default

   function new_Parser_schema(schema) result(p)
      type(Parser) :: p
      class(AbstractSchema), intent(in) :: schema

      p%schema = schema
      p%anchors = StringNodeMap()

   end function new_Parser_schema

   function new_Parser_schema_name(schema_name,unusable,rc) result(p)
      type(Parser) :: p
      character(*), intent(in) :: schema_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      print*,__FILE__,__LINE__
      select case (schema_name)
      case ('json','JSON')
      print*,__FILE__,__LINE__
         p = Parser(JSONSchema())
      case ('core','Core')
      print*,__FILE__,__LINE__
         p = Parser(CoreSchema())
      case ('failsafe','Failsafe')
      print*,__FILE__,__LINE__
         p = Parser(FailsafeSchema())
      case default
         __FAIL__(YAFYAML_PARSER_ERROR)
      end select
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

   end function new_Parser_schema_name

   function load_from_stream(this, stream, unusable, rc) result(node)
      class(YAML_Node), allocatable :: node
      class(Parser), intent(inout) :: this
      class(AbstractTextStream), intent(in) :: stream
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(Lexer) :: lexr
      integer :: status

      lexr = Lexer(Reader(stream))
      node = this%top(lexr,rc=status)
      __VERIFY__(status)
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

   end function load_from_stream

   function load_from_file(this, fname, unusable, rc) result(node)
      class(YAML_Node), allocatable :: node
      class(Parser), intent(inout) :: this
      character(len=*), intent(in) :: fname
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      node = this%load(FileStream(fname),rc=status)
      __VERIFY__(status)
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

   end function load_from_file

   function top(this, lexr, unusable, rc) result(node)
      class(YAML_Node), target, allocatable :: node
      class(Parser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
  
      integer :: status

      call this%top_load(lexr, node, rc=status)
      __VERIFY__(status)
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)     

   end function top

   subroutine top_load(this, lexr, node, unusable, rc)
      class(YAML_Node), target, allocatable, intent(out) :: node
      class(Parser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      logical :: done
      class(AbstractToken), allocatable :: token
      type(mapping), pointer :: map
      type(sequence), pointer :: seq
      integer :: status

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
         class is (SequenceStartToken)
            node = SequenceNode()
            seq => to_sequence(node)
            call this%process_sequence(lexr, seq, rc=status)
            __VERIFY__(status)
            done = .true.
         class is (MappingStartToken)
            allocate(node, source=MappingNode())
            map => to_mapping(node)
            call this%process_mapping(lexr, map, rc=status)
            __VERIFY__(status)
            done = .true.
         class default
            __FAIL__(YAFYAML_PARSER_ERROR)
         end select
         deallocate(token)
      end do

      if (.not. done) then
            __FAIL__(YAFYAML_PARSER_ERROR)
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

   end subroutine top_load


   recursive subroutine process_sequence(this, lexr, seq, unusable, rc)
      class(Parser), target, intent(inout) :: this
      type(Lexer), intent(inout) :: lexr 
      type(sequence), intent(inout) :: seq
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class(AbstractToken), allocatable :: token, token_2
      logical :: expect_another
      character(:), allocatable :: anchor, alias
      type(mapping), pointer :: map
      class(YAML_Node), pointer :: subnode
      type(Sequence), pointer :: subseq
      type(Mapping), pointer :: submap

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
            alias = q%value
            if (this%anchors%count(alias) > 0) then
!!$              if (key_str == MERGE_KEY) then
!!$                 !TODO - should throw exception if not mapping ...
!!$                 anchor_mapping => to_mapping(this%anchors%of(alias))
!!$                 call merge(map, anchor_mapping)
!!$              else ! scalar
               call seq%push_back(this%anchors%of(alias))
           end if
           deallocate(alias)
           cycle
         end select

         select type (token)
         type is (ScalarToken)
            call seq%push_back(this%interpret(token))
         class is (SequenceStartToken)
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
         class is (MappingStartToken)
            call seq%push_back(MappingNode())
            subnode => seq%back()
            submap => to_mapping(subnode)
            call this%process_mapping(lexr, submap)
         class default
            __FAIL__(YAFYAML_PARSER_ERROR)
         end select
         deallocate(token)

         if (allocated(anchor)) then
            call this%anchors%insert(anchor, seq%back())
            deallocate(anchor)
         end if

      end do
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
!!$      depth = depth - 1

   end subroutine process_sequence


   recursive subroutine process_mapping(this, lexr, map, unusable, rc)
      class(Parser), target, intent(inout) :: this
      type(Lexer), intent(inout) :: lexr
      type(Mapping), target, intent(inout):: map
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

!!$      integer :: depth = 0
!!$      depth = depth + 1

      do
         associate (token => lexr%get_token())
           select type (token)
           type is (ScalarToken)
              ! no-op
           type is (KeyToken)
              call this%process_map_entry(lexr, map)
           type is (FlowNextEntryToken)
              ! no-op
           type is (FlowMappingEndToken)
              exit
           type is (BlockEndToken)
              exit
           class default
               __FAIL__(YAFYAML_PARSER_ERROR)
           end select
         end associate

      end do
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

!!$      depth = depth - 1
   end subroutine process_mapping

   recursive subroutine process_map_entry(this, lexr, map, unusable, rc)
      class(Parser), intent(inout) :: this
      type(Lexer), intent(inout) :: lexr
      type(Mapping), target, intent(inout) :: map
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(:), allocatable :: anchor, alias
      character(:), allocatable :: key_str
      type(mapping), pointer :: anchor_mapping
      class(AbstractToken), allocatable :: token_2, token_3

      ! Wrapper is needed here as a workaround for GFortran (11.2) problem
      ! with recursions and local variables that are abstract & allocatable.
      ! At least that is the current theory.  Hard to pin down.
      type :: Wrapper
         class(YAML_Node), allocatable :: node
      end type Wrapper

      class(YAML_Node), pointer :: tmp
      type(Sequence), pointer :: subseq
      type(Mapping), pointer :: submap

#ifdef __GFORTRAN__
      type(Wrapper) :: key_wrap
      type(Wrapper) :: value_wrap
#else
      class(YAML_Node), allocatable :: key
      class(YAML_Node), allocatable :: value
#endif
      
!!$      integer :: depth = 0
!!$      depth = depth + 1


      associate (token => lexr%get_token())

        _KEY = get_key(this, token, key_str,rc=rc)
        __VERIFY__(YAFYAML_SUCCESS)

        associate (value_token => lexr%get_token())
          if (value_token%get_id() /= VALUE_INDICATOR) then
             __FAIL__(YAFYAML_PARSER_ERROR)
          end if
        end associate

        ! Usually, next token indicates the value type of key-value
        ! pair.
        token_2 = lexr%get_token()  
          
        ! Possible anchor or alias?
        select type(q => token_2)
        type is (AnchorToken)
           anchor = q%value
           token_3 = lexr%get_token()
        type is (AliasToken)
           alias = q%value
           if (this%anchors%count(alias) > 0) then
              if (key_str == MERGE_KEY) then
                 !TODO - should throw exception if not mapping ...
                 anchor_mapping => to_mapping(this%anchors%of(alias))
                 call merge(map, anchor_mapping)
              else ! scalar
                 call map%insert(_KEY, this%anchors%of(alias))
              end if
              deallocate(alias)
              __RETURN__(YAFYAML_SUCCESS)
           else
              __FAIL__(YAFYAML_PARSER_ERROR)
           end if
        class default
           token_3 = token_2
        end select

        select type (q => token_3)
        type is (ScalarToken)
           call map%insert(_KEY, this%interpret(q))

        class is (SequenceStartToken)
           call map%insert(_KEY, SequenceNode())
           tmp => map%at(_KEY)
           subseq => to_sequence(tmp)
           call this%process_sequence(lexr, subseq)

        class is (MappingStartToken)
#ifndef _NAG
           call map%insert(_KEY, MappingNode())
#else
           block
             class(YAML_Node), allocatable :: tnode
             allocate(tnode, source=MappingNode())
             call map%insert(key, tnode)
           end block
#endif
           tmp => map%at(_KEY)
           submap => to_mapping(tmp)
           call this%process_mapping(lexr, submap)
           
        class default
           __FAIL__(YAFYAML_PARSER_ERROR)
        end select
        
        if (allocated(anchor)) then
           call this%anchors%insert(anchor, map%of(_KEY))
           deallocate(anchor)
        end if
        
      end associate
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

!!$      depth = depth - 1
      
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

   end subroutine process_map_entry

   function get_key(this, token, key_str, rc) result(key)
      class(YAML_Node), allocatable :: key
      class(Parser), intent(in) :: this
      class(AbstractToken), intent(in) :: token
      character(:), allocatable, intent(out) :: key_str
      integer, optional, intent(out) :: rc

      select type(token)
      type is (ScalarToken)
         key_str = token%value
         key = this%interpret(token)
      class default
         __FAIL__(YAFYAML_PARSER_ERROR)
      end select
      __RETURN__(YAFYAML_SUCCESS)

   end function get_key


   ! 
   recursive subroutine process_value(this, token, lexr, value, unusable, rc)
      class(Parser), intent(inout) :: this
      class(AbstractToken), intent(in) :: token
      type(Lexer), intent(inout) :: lexr
      class(YAML_Node), allocatable, target, intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(mapping), pointer :: map
      type(sequence), pointer :: seq

!!$      integer, save :: depth =0
!!$      depth = depth + 1

      select type(token)
      type is (ScalarToken)
         value = this%interpret(token)
      class is (SequenceStartToken)
         value = SequenceNode()
         seq => to_sequence(value)
         call this%process_sequence(lexr, seq)
      class is (MappingStartToken)
         value = MappingNode()
         map => to_mapping(value)
         call this%process_mapping(lexr, map)
      class default
         __FAIL__(YAFYAML_PARSER_ERROR)
      end select
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

!!$      depth = depth - 1
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
#undef _KEY
#undef _VALUE
