#include "error_handling.h"
#include "string_handling.h"
#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s2, s3, s4, s5, s6, s7, s8, s9


! This class exists largely to shield Fortran developers from the
! inherent polymorphism that underpins the representation of different
! yaml types: seqence, mapping, and scalars (int, float, bool,
! string).

! Unfortunately that still requires a great deal of trivial wrappers
! ...

   ! Users that wish to grow a YAML_Node by inserting new
   ! elements, as opposed to just using the parser to construct a
   ! configuration, will need to use the lower level types.


module fy_YAML_Node
   use fy_AbstractNode
   use fy_Mapping
   use fy_MappingNode
   use fy_Sequence
   use fy_SequenceNode
   use fy_ErrorCodes
   use fy_ErrorHandling
   implicit none
   private

   public :: YAML_Node
   public :: YAML_NodeIterator
   public :: operator(==)
   public :: operator(/=)


   type :: YAML_Node
!!$      private
      class(AbstractNode), pointer :: node => null()
   contains
      procedure :: initialize
      procedure :: at_multi_selector
      generic :: at => at_multi_selector
      procedure :: of_multi_selector
      generic :: of => of_multi_selector
      procedure :: has_selector
      generic :: has => has_selector

      procedure :: get_logical
      procedure :: get_string
      procedure :: get_integer32
      procedure :: get_integer64
      procedure :: get_real32
      procedure :: get_real64
      procedure :: get_logical_1d
      procedure :: get_integer32_1d
      procedure :: get_integer64_1d
      procedure :: get_real32_1d
      procedure :: get_real64_1d
      procedure :: get_subconfig
      generic :: get => get_logical, get_logical_1d
      generic :: get => get_string
      generic :: get => get_integer32, get_integer32_1d
      generic :: get => get_integer64, get_integer64_1d
      generic :: get => get_real32, get_real32_1d
      generic :: get => get_real64, get_real64_1d
      generic :: get => get_subconfig

      procedure :: set_logical
      procedure :: set_string
      procedure :: set_integer32
      procedure :: set_integer64
      procedure :: set_real32
      procedure :: set_real64
!!$      procedure :: set_logical_1d
!!$      procedure :: set_integer32_1d
!!$      procedure :: set_integer64_1d
!!$      procedure :: set_real32_1d
!!$      procedure :: set_real64_1d
!!$      procedure :: set_subconfig
      generic :: set => set_logical!, set_logical_1d
      generic :: set => set_string
      generic :: set => set_integer32!, set_integer32_1db
      generic :: set => set_integer64!, set_integer64_1d
      generic :: set => set_real32!, set_real32_1d
      generic :: set => set_real64!, set_real64_1d
!!$      generic :: set => set_subconfig
!!$
!!$      procedure :: insert_logical
!!$      procedure :: insert_string
!!$      procedure :: insert_integer32
!!$      procedure :: insert_integer64
!!$      procedure :: insert_real32
!!$      procedure :: insert_real64
!!$      procedure :: insert_logical_1d
!!$      procedure :: insert_integer32_1d
!!$      procedure :: insert_integer64_1d
!!$      procedure :: insert_real32_1d
!!$      procedure :: insert_real64_1d
!!$      procedure :: insert_subconfig
!!$      generic :: insert => insert_logical, insert_logical_1d
!!$      generic :: insert => insert_string
!!$      generic :: insert => insert_integer32, insert_integer32_1d
!!$      generic :: insert => insert_integer64, insert_integer64_1d
!!$      generic :: insert => insert_real32, insert_real32_1d
!!$      generic :: insert => insert_real64, insert_real64_1d
!!$      generic :: insert => insert_subconfig
!!$
      procedure, pass(this) :: assign_to_logical
      procedure, pass(this) :: assign_to_string
      procedure, pass(this) :: assign_to_integer32
      procedure, pass(this) :: assign_to_integer64
      procedure, pass(this) :: assign_to_real32
      procedure, pass(this) :: assign_to_real64
      generic :: assignment(=) => assign_to_logical
      generic :: assignment(=) => assign_to_string
      generic :: assignment(=) => assign_to_integer32
      generic :: assignment(=) => assign_to_integer64
      generic :: assignment(=) => assign_to_real32
      generic :: assignment(=) => assign_to_real64

      ! using YAML terminology
      procedure :: is_sequence
      procedure :: is_mapping
      procedure :: is_scalar
      procedure :: is_bool
      procedure :: is_string
      procedure :: is_int
      procedure :: is_float

      procedure :: size => size_config
      procedure :: clear

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

      procedure :: begin => begin_cfg
      procedure :: end => end_cfg

   end type YAML_Node

   type YAML_NodeIterator
      private
      type(MappingIterator) :: mapping_iterator
   contains
      procedure :: next => next_iter

      procedure :: get_key_logical
      procedure :: get_key_string
      procedure :: get_key_integer32
      procedure :: get_key_integer64
      procedure :: get_key_real32
      procedure :: get_key_real64
      procedure :: get_key_logical_1d
      procedure :: get_key_integer32_1d
      procedure :: get_key_integer64_1d
      procedure :: get_key_real32_1d
      procedure :: get_key_real64_1d
      procedure :: get_key_subconfig
      generic :: get_key => get_key_logical, get_key_logical_1d
      generic :: get_key => get_key_string
      generic :: get_key => get_key_integer32, get_key_integer32_1d
      generic :: get_key => get_key_integer64, get_key_integer64_1d
      generic :: get_key => get_key_real32, get_key_real32_1d
      generic :: get_key => get_key_real64, get_key_real64_1d
      generic :: get_key => get_key_subconfig

!!$      procedure :: get_value_logical
!!$      procedure :: get_value_string
!!$      procedure :: get_value_integer32
!!$      procedure :: get_value_integer64
!!$      procedure :: get_value_real32
!!$      procedure :: get_value_real64
!!$      procedure :: get_value_logical_1d
!!$      procedure :: get_value_integer32_1d
!!$      procedure :: get_value_integer64_1d
!!$      procedure :: get_value_real32_1d
!!$      procedure :: get_value_real64_1d
!!$      procedure :: get_value_subconfig
!!$      generic :: get_value => get_value_logical, get_value_logical_1d
!!$      generic :: get_value => get_value_string
!!$      generic :: get_value => get_value_integer32, get_value_integer32_1d
!!$      generic :: get_value => get_value_integer64, get_value_integer64_1d
!!$      generic :: get_value => get_value_real32, get_value_real32_1d
!!$      generic :: get_value => get_value_real64, get_key_real64_1d
!!$      generic :: get_value => get_value_subconfig
!!$
!!$      procedure :: set_value_logical
!!$      procedure :: set_value_string
!!$      procedure :: set_value_integer32
!!$      procedure :: set_value_integer64
!!$      procedure :: set_value_real32
!!$      procedure :: set_value_real64
!!$      procedure :: set_value_logical_1d
!!$      procedure :: set_value_integer32_1d
!!$      procedure :: set_value_integer64_1d
!!$      procedure :: set_value_real32_1d
!!$      procedure :: set_value_real64_1d
!!$      procedure :: set_value_subconfig
!!$      generic :: set_value => set_value_logical, set_value_logical_1d
!!$      generic :: set_value => set_value_string
!!$      generic :: set_value => set_value_integer32, set_value_integer32_1d
!!$      generic :: set_value => set_value_integer64, set_value_integer64_1d
!!$      generic :: set_value => set_value_real32, set_value_real32_1d
!!$      generic :: set_value => set_value_real64, set_key_real64_1d
!!$      generic :: set_value => set_value_subconfig
!!$
!!$      procedure :: insert_value_logical
!!$      procedure :: insert_value_string
!!$      procedure :: insert_value_integer32
!!$      procedure :: insert_value_integer64
!!$      procedure :: insert_value_real32
!!$      procedure :: insert_value_real64
!!$      procedure :: insert_value_logical_1d
!!$      procedure :: insert_value_integer32_1d
!!$      procedure :: insert_value_integer64_1d
!!$      procedure :: insert_value_real32_1d
!!$      procedure :: insert_value_real64_1d
!!$      procedure :: insert_value_subconfig
!!$      generic :: insert_value => insert_value_logical, insert_value_logical_1d
!!$      generic :: insert_value => insert_value_string
!!$      generic :: insert_value => insert_value_integer32, insert_value_integer32_1d
!!$      generic :: insert_value => insert_value_integer64, insert_value_integer64_1d
!!$      generic :: insert_value => insert_value_real32, insert_value_real32_1d
!!$      generic :: insert_value => insert_value_real64, insert_key_real64_1d
!!$      generic :: insert_value => insert_value_subconfig

   end type YAML_NodeIterator

   interface YAML_Node
      module procedure new_YAML_Node
   end interface YAML_Node

   ! YAML_Node type-bound procedures
   interface

      module function size_config(this) result(size)
         use, intrinsic :: iso_fortran_env, only: INT64
         integer(kind=INT64) :: size
         class(YAML_Node), intent(in) :: this
      end function size_config

      ! With error handling
      module function at_multi_selector(this, SELECTORS, unusable, found, err_msg, rc) result(subcfg)
         use fy_KeywordEnforcer
         type(YAML_Node) :: subcfg
         class(YAML_Node), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function at_multi_selector

      ! No error handling
      module function of_multi_selector(this, SELECTORS) result(subcfg)
         type(YAML_Node) :: subcfg
         class(YAML_Node), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
      end function of_multi_selector

      module function has_selector(this, s1, OPT_SELECTORS) result(has)
         logical :: has
         class(YAML_Node), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
      end function has_selector


      module subroutine get_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_Node), target, intent(in) :: this
         logical, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_logical


      module subroutine get_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_Node), target, intent(in) :: this
         logical, allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_logical_1d
   

      module subroutine get_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_Node), target, intent(in) :: this
         character(:), allocatable, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_string


      module subroutine get_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         class(YAML_Node), target, intent(in) :: this
         integer(kind=INT32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer32

      module subroutine get_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         class(YAML_Node), target, intent(in) :: this
         integer(kind=INT32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer32_1d

      module subroutine get_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         class(YAML_Node), target, intent(in) :: this
         integer(kind=INT64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer64

      module subroutine get_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         class(YAML_Node), target, intent(in) :: this
         integer(kind=INT64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer64_1d


      module subroutine get_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         class(YAML_Node), target, intent(in) :: this
         real(kind=REAL32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real32

      module subroutine get_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         class(YAML_Node), target, intent(in) :: this
         real(kind=REAL32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real32_1d

      module subroutine get_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         class(YAML_Node), target, intent(in) :: this
         real(kind=REAL64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real64

      module subroutine get_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         class(YAML_Node), target, intent(in) :: this
         real(kind=REAL64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real64_1d

      module subroutine set_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_Node), target, intent(inout) :: this
         logical, intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_logical

      module subroutine set_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_Node), target, intent(inout) :: this
         character(*), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine  set_string

      module subroutine set_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         class(YAML_Node), target, intent(inout) :: this
         integer(kind=INT32), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_integer32

      module subroutine set_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         class(YAML_Node), target, intent(inout) :: this
         integer(kind=INT64), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_integer64


      module subroutine set_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         class(YAML_Node), target, intent(inout) :: this
         real(kind=REAL32), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_real32

      module subroutine set_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         class(YAML_Node), target, intent(inout) :: this
         real(kind=REAL64), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_real64


      module subroutine get_subconfig(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_Node), target, intent(in) :: this
         type(YAML_Node), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_subconfig


      module subroutine assign_to_logical(flag, this)
         logical, intent(out) :: flag
         class(YAML_Node), intent(in) :: this
      end subroutine assign_to_logical

      module subroutine assign_to_string(string, this)
         character(:), allocatable, intent(out) :: string
         class(YAML_Node), intent(in) :: this
      end subroutine assign_to_string

      module subroutine assign_to_integer32(i32, this)
         use, intrinsic :: iso_fortran_env, only: INT32
         integer(kind=INT32), intent(out) :: i32
         class(YAML_Node), intent(in) :: this
      end subroutine assign_to_integer32

      module subroutine assign_to_integer64(i64, this)
         use, intrinsic :: iso_fortran_env, only: INT64
         integer(kind=INT64), intent(out) :: i64
         class(YAML_Node), intent(in) :: this
      end subroutine assign_to_integer64

      module subroutine assign_to_real32(r32, this)
         use, intrinsic :: iso_fortran_env, only: REAL32
         real(kind=REAL32), intent(out) :: r32
         class(YAML_Node), intent(in) :: this
      end subroutine assign_to_real32

      module subroutine assign_to_real64(r64, this)
         use, intrinsic :: iso_fortran_env, only: REAL64
         real(kind=REAL64), intent(out) :: r64
         class(YAML_Node), intent(in) :: this
      end subroutine assign_to_real64



      module logical function is_sequence(this)
         class(YAML_Node), intent(in) :: this
      end function is_sequence

      module logical function is_mapping(this)
         class(YAML_Node), intent(in) :: this
      end function is_mapping

      module logical function is_scalar(this)
         class(YAML_Node), intent(in) :: this
      end function is_scalar

      module logical function is_bool(this)
         class(YAML_Node), intent(in) :: this
      end function is_bool

      module logical function is_string(this)
         class(YAML_Node), intent(in) :: this
      end function is_string

      module logical function is_int(this)
         class(YAML_Node), intent(in) :: this
      end function is_int

      module logical function is_float(this)
         class(YAML_Node), intent(in) :: this
      end function is_float


      ! Factory methods to create an iterator
      module function begin_cfg(this, unusable, err_msg, rc) result(iter)
         use fy_KeywordEnforcer
         type(YAML_NodeIterator) :: iter
         class(YAML_Node), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(out) :: err_msg
         integer, optional, intent(out) :: rc
      end function begin_cfg

      module function end_cfg(this, unusable, err_msg, rc) result(iter)
         use fy_KeywordEnforcer
         type(YAML_NodeIterator) :: iter
         class(YAML_Node), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(out) :: err_msg
         integer, optional, intent(out) :: rc
      end function end_cfg

      module subroutine clear(this)
         class(YAML_Node), intent(inout) :: this
      end subroutine clear

      module subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
         class(YAML_Node), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine write_formatted

   end interface

   ! YAML_NodeIterator type-bound procedures
   interface

      module subroutine next_iter(this)
         class(YAML_NodeIterator), intent(inout) :: this
      end subroutine next_iter

      module subroutine get_key_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_NodeIterator), target, intent(in) :: this
         logical, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_logical

      module subroutine get_key_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_NodeIterator), target, intent(in) :: this
         character(:), allocatable, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_string

      module subroutine get_key_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         class(YAML_NodeIterator), target, intent(in) :: this
         integer(kind=INT32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_integer32

      module subroutine get_key_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         class(YAML_NodeIterator), target, intent(in) :: this
         integer(kind=INT64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_integer64

      module subroutine get_key_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         class(YAML_NodeIterator), target, intent(in) :: this
         real(kind=REAL32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_real32

      module subroutine get_key_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         class(YAML_NodeIterator), target, intent(in) :: this
         real(kind=REAL64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_real64

      module subroutine get_key_subconfig(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_NodeIterator), target, intent(in) :: this
         type(YAML_Node), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_subconfig

      module subroutine get_key_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_NodeIterator), target, intent(in) :: this
         logical, allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_logical_1d

      module subroutine get_key_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         class(YAML_NodeIterator), target, intent(in) :: this
         integer(kind=INT32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_integer32_1d

      module subroutine get_key_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         class(YAML_NodeIterator), target, intent(in) :: this
         integer(kind=INT64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_integer64_1d

      module subroutine get_key_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         class(YAML_NodeIterator), target, intent(in) :: this
         real(kind=REAL32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_real32_1d

      module subroutine get_key_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         class(YAML_NodeIterator), target, intent(in) :: this
         real(kind=REAL64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_key_real64_1d


      module subroutine get_value_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_NodeIterator), target, intent(in) :: this
         logical, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_logical

      module subroutine get_value_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_NodeIterator), target, intent(in) :: this
         character(:), allocatable, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_string

      module subroutine get_value_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         class(YAML_NodeIterator), target, intent(in) :: this
         integer(kind=INT32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_integer32

      module subroutine get_value_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         class(YAML_NodeIterator), target, intent(in) :: this
         integer(kind=INT64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_integer64

      module subroutine get_value_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         class(YAML_NodeIterator), target, intent(in) :: this
         real(kind=REAL32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_real32

      module subroutine get_value_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         class(YAML_NodeIterator), target, intent(in) :: this
         real(kind=REAL64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_real64

      module subroutine get_value_subconfig(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_NodeIterator), target, intent(in) :: this
         type(YAML_Node), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_subconfig

      module subroutine get_value_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(YAML_NodeIterator), target, intent(in) :: this
         logical, allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_logical_1d

      module subroutine get_value_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         class(YAML_NodeIterator), target, intent(in) :: this
         integer(kind=INT32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_integer32_1d

      module subroutine get_value_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         class(YAML_NodeIterator), target, intent(in) :: this
         integer(kind=INT64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_integer64_1d

      module subroutine get_value_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         class(YAML_NodeIterator), target, intent(in) :: this
         real(kind=REAL32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_real32_1d

      module subroutine get_value_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         class(YAML_NodeIterator), target, intent(in) :: this
         real(kind=REAL64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_value_real64_1d

   end interface

   ! Relational operators
   interface operator(==)
      module function equal_iter(a, b) result(equal)
         logical :: equal
         type(YAML_NodeIterator), intent(in) :: a, b
      end function equal_iter
   end interface operator(==)

   interface operator(/=)
      module function not_equal_iter(a, b) result(not_equal)
         logical :: not_equal
         type(YAML_NodeIterator), intent(in) :: a, b
      end function not_equal_iter
   end interface operator(/=)

      
contains

   ! Constructor
   function new_YAML_Node(node) result(this)
      type(YAML_Node) :: this
      class(AbstractNode), intent(in) :: node

      allocate(this%node, source=node)
!!$      select type(q => node)
!!$      type is (MappingNode)
!!$         allocate(this%node, source=MappingNode())
!!$         select type (qq => this%node)
!!$         type is (MappingNode) ! guaranteed
!!$            call clone(q, qq)
!!$         end select
!!$      type is (SequenceNode)
!!$         allocate(this%node, source=SequenceNode())
!!$         select type (qq => this%node)
!!$         type is (SequenceNode) ! guaranteed
!!$            call clone(q, qq)
!!$         end select
!!$      class default
!!$         allocate(this%node, source=node)
!!$      end select

   end function new_YAML_Node

   ! Set the top node
   subroutine initialize(this, node)
      class(YAML_Node), intent(inout) :: this
      class(AbstractNode), pointer, intent(in) :: node

      this%node => node

   end subroutine initialize

end module fy_YAML_Node
