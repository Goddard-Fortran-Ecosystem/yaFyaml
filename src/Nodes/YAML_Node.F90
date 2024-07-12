#include "string_handling.h"
module fy_YAML_Node
   use fy_KeywordEnforcer
   implicit none
   private

   public :: YAML_Node
   public :: NodeIterator

   type, abstract :: YAML_Node
   contains
      procedure(i_size), deferred :: size

      ! accessors to sub-nodes
      procedure(I_at_multi_selector), deferred :: at_multi_selector
      procedure(I_of_multi_selector), deferred :: of_multi_selector
      procedure(I_has),               deferred :: has
      generic :: at => at_multi_selector
      generic :: of => of_multi_selector

      procedure(I_get_logical),      deferred :: get_logical
      procedure(I_get_logical_1d),   deferred :: get_logical_1d
      procedure(I_get_string),       deferred :: get_string
      procedure(I_get_integer32),    deferred :: get_integer32
      procedure(I_get_integer32_1d), deferred :: get_integer32_1d
      procedure(I_get_integer64),    deferred :: get_integer64
      procedure(I_get_integer64_1d), deferred :: get_integer64_1d
      procedure(I_get_real32),       deferred :: get_real32
      procedure(I_get_real32_1d),    deferred :: get_real32_1d
      procedure(I_get_real64),       deferred :: get_real64
      procedure(I_get_real64_1d),    deferred :: get_real64_1d
      generic :: get => get_string
      generic :: get => get_logical,   get_logical_1d
      generic :: get => get_integer32, get_integer32_1d
      generic :: get => get_integer64, get_integer64_1d
      generic :: get => get_real32,    get_real32_1d
      generic :: get => get_real64,    get_real64_1d

      procedure(I_set_logical), deferred :: set_logical
      procedure(I_set_string), deferred :: set_string
      procedure(I_set_integer32), deferred :: set_integer32
      procedure(I_set_integer64), deferred :: set_integer64
      procedure(I_set_real32), deferred :: set_real32
      procedure(I_set_real64), deferred :: set_real64
      procedure(I_set_logical_1d), deferred :: set_logical_1d
      procedure(I_set_integer32_1d), deferred :: set_integer32_1d
      procedure(I_set_integer64_1d), deferred :: set_integer64_1d
      procedure(I_set_real32_1d), deferred :: set_real32_1d
      procedure(I_set_real64_1d), deferred :: set_real64_1d
      procedure(I_set_node), deferred :: set_node

      generic :: set => set_string
      generic :: set => set_logical,   set_logical_1d
      generic :: set => set_integer32, set_integer32_1d
      generic :: set => set_integer64, set_integer64_1d
      generic :: set => set_real32,    set_real32_1d
      generic :: set => set_real64,    set_real64_1d
      generic :: set => set_node

      ! using YAML terminology
      procedure(I_is), deferred, nopass :: is_sequence
      procedure(I_is), deferred, nopass :: is_mapping
      procedure(I_is), deferred, nopass :: is_scalar
      procedure(I_is), deferred, nopass :: is_bool
      procedure(I_is), deferred, nopass :: is_string
      procedure(I_is), deferred, nopass :: is_int
      procedure(I_is), deferred, nopass :: is_float

      procedure(I_write_formatted),      deferred :: write_formatted
      procedure(I_write_node_formatted), deferred :: write_node_formatted
      generic :: write(formatted) => write_formatted

      ! "<" opertor.
      ! Necessary to support map container with *Node keys.
      procedure(I_less_than), deferred :: less_than
      generic :: operator(<) => less_than

      procedure(I_clear), deferred :: clear

      procedure(I_begin), deferred :: begin
      procedure(I_begin), deferred :: end

      procedure (I_verify), deferred :: verify
      procedure (I_clone), deferred :: clone
   end type YAML_Node


   type, abstract :: NodeIterator
      private
   contains
      procedure(I_is_iter), deferred :: is_valid
      procedure(I_is_iter), deferred :: is_sequence_iterator
      procedure(I_is_iter), deferred :: is_mapping_iterator

      procedure(I_next), deferred :: next

      procedure(I_equal), deferred :: equal_to
      generic :: operator(==) => equal_to
      procedure(I_equal), deferred :: not_equal_to
      generic :: operator(/=) => not_equal_to


      ! Sequence
      procedure(I_at_iter), deferred :: at

      ! Mapping
      procedure(I_at_iter), deferred :: first
      procedure(I_at_iter), deferred :: second

   end type NodeIterator


#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s2, s3, s4, s5, s6, s7, s8, s9
   abstract interface

      integer function i_size(this)
         import YAML_Node
         class(YAML_Node), intent(in) :: this
      end function i_size
      
      function I_of_multi_selector(this, SELECTORS) result(node_ptr)
         import YAML_Node
         implicit none
         class(YAML_Node), pointer :: node_ptr
         class(YAML_Node), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
      end function I_of_multi_selector

      logical function I_has(this, SELECTORS) result(has)
         import YAML_Node
         implicit none
         class(YAML_Node), intent(in) :: this
         class(*), intent(in) :: s1 ! at least one selector required here
         class(*), optional, intent(in) :: OPT_SELECTORS
      end function I_has


      function I_at_multi_selector(this, SELECTORS, &
           & unusable, found, err_msg, rc) result(node_ptr)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), pointer :: node_ptr
         class(YAML_Node), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function I_at_multi_selector


      subroutine I_get_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         logical, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_logical

      subroutine I_get_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         logical, allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_logical_1d


      subroutine I_get_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         character(:), allocatable, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_string

      subroutine I_get_string_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         character(:), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_string_1d


      subroutine I_get_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         integer(kind=INT32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_integer32

      subroutine I_get_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         integer(kind=INT32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_integer32_1d


      subroutine I_get_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         integer(kind=INT64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_integer64

      subroutine I_get_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         integer(kind=INT64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_integer64_1d


      subroutine I_get_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         real(kind=REAL32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_real32

      subroutine I_get_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         real(kind=REAL32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_real32_1d


      subroutine I_get_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         real(kind=REAL64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_real64

      subroutine I_get_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(in) :: this
         real(kind=REAL64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_real64_1d

      subroutine I_set_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         logical, intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_logical

      subroutine I_set_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         character(*), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine  I_set_string

      subroutine I_set_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         integer(kind=INT32), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_integer32

      subroutine I_set_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         integer(kind=INT64), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_integer64


      subroutine I_set_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         real(kind=REAL32), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_real32

      subroutine I_set_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         real(kind=REAL64), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_real64

      subroutine I_set_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         logical, intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_logical_1d

      subroutine I_set_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         integer(kind=INT32), intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_integer32_1d

      subroutine I_set_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         integer(kind=INT64), intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_integer64_1d

      subroutine I_set_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         real(kind=REAL32), intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_real32_1d

      subroutine I_set_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         real(kind=REAL64), intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_real64_1d

      subroutine I_set_node(this, node, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         implicit none
         class(YAML_Node), target, intent(inout) :: this
         class(YAML_Node), intent(in) :: node
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_set_node


      subroutine I_assign_to_logical(flag, this)
         import YAML_Node
         implicit none
         logical, intent(inout) :: flag
         class(YAML_Node), intent(in) :: this
      end subroutine I_assign_to_logical


      subroutine I_assign_to_string(string, this)
         import YAML_Node
         implicit none
         character(:), allocatable, intent(inout) :: string
         class(YAML_Node), intent(in) :: this
      end subroutine I_assign_to_string

      subroutine I_assign_to_integer32(i32, this)
         use, intrinsic :: iso_fortran_env, only: INT32
         import YAML_Node
         implicit none
         integer(kind=INT32), intent(inout) :: i32
         class(YAML_Node), intent(in) :: this
      end subroutine I_assign_to_integer32

      subroutine I_assign_to_integer64(i64, this)
         use, intrinsic :: iso_fortran_env, only: INT64
         import YAML_Node
         implicit none
         integer(kind=INT64), intent(inout) :: i64
         class(YAML_Node), intent(in) :: this
      end subroutine I_assign_to_integer64

      subroutine I_assign_to_real32(r32, this)
         use, intrinsic :: iso_fortran_env, only: REAL32
         import YAML_Node
         implicit none
         real(kind=REAL32), intent(inout) :: r32
         class(YAML_Node), intent(in) :: this
      end subroutine I_assign_to_real32

      subroutine I_assign_to_real64(r64, this)
         use, intrinsic :: iso_fortran_env, only: REAL64
         import YAML_Node
         implicit none
         real(kind=REAL64), intent(inout) :: r64
         class(YAML_Node), intent(in) :: this
      end subroutine I_assign_to_real64

      pure logical function I_is() result(is)
         import YAML_Node
      end function I_is

      logical function I_less_than(a, b)
         import YAML_Node
         implicit none
         class(YAML_Node), intent(in) :: a
         class(YAML_Node), intent(in) :: b
      end function I_less_than

      subroutine i_write_formatted(this, unit, iotype, v_list, iostat, iomsg)
         import YAML_Node
         class(YAML_Node), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine i_write_formatted

      subroutine i_write_node_formatted(this, unit, iotype, v_list, iostat, iomsg)
         import YAML_Node
         class(YAML_Node), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine i_write_node_formatted

      subroutine I_clear(this)
         import YAML_Node
         class(YAML_Node), intent(inout) :: this
      end subroutine I_clear

      ! Throws exception if node is scalar.
      function I_begin(this, unusable, rc) result(iter)
         use fy_KeywordEnforcer
         import YAML_Node
         import NodeIterator
         class(NodeIterator), allocatable :: iter
         class(YAML_Node), target, intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end function I_begin

   end interface

   abstract interface

      logical function I_is_iter(this)
         import NodeIterator
         class(NodeIterator), intent(in) :: this
      end function I_is_iter

      subroutine I_next(this)
         import NodeIterator
         class(NodeIterator), intent(inout) :: this
      end subroutine I_next

      function I_at_iter(this, unusable, err_msg, rc) result(ptr)
         use fy_KeywordEnforcer
         import YAML_Node
         import NodeIterator
         class(YAML_Node), pointer :: ptr
         class(NodeIterator), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function I_at_iter

      logical function I_equal(a, b)
         import NodeIterator
         class(NodeIterator), intent(in) :: a
         class(NodeIterator), intent(in) :: b
      end function I_equal

      logical function I_verify(this) result(verify)
         import YAML_Node
         class(YAML_Node), target, intent(in) :: this
      end function I_verify

      subroutine I_clone(to, from, unusable, rc)
         use fy_KeywordEnforcer
         import YAML_Node
         class(YAML_Node), intent(inout) :: to
         class(YAML_Node), intent(in) :: from
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine I_clone

   end interface

contains


   subroutine next(this)
      class(NodeIterator), intent(inout) :: this
   end subroutine next


end module fy_YAML_Node
