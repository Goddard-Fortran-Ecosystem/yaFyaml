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

   ! Users that wish to grow a Configuration by inserting new
   ! elements, as opposed to just using the parser to construct a
   ! configuration, will need to use the lower level types.


module fy_Configuration
   use fy_AbstractNode
   use fy_Mapping
   use fy_MappingNode
   use fy_Sequence
   use fy_SequenceNode
   use fy_ErrorCodes
   use fy_ErrorHandling
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   implicit none
   private

   public :: Configuration

   type :: Configuration
      private
      class(AbstractNode), pointer :: node => null()
   contains
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

   end type Configuration

   interface Configuration
      module procedure new_Configuration
   end interface Configuration

   interface

      module function size_config(this) result(size)
         integer(kind=INT64) :: size
         class(Configuration), intent(in) :: this
      end function size_config

      ! With error handling
      module function at_multi_selector(this, SELECTORS, unusable, found, err_msg, rc) result(subcfg)
         use fy_KeywordEnforcer
         type(Configuration) :: subcfg
         class(Configuration), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function at_multi_selector

      ! No error handling
      module function of_multi_selector(this, SELECTORS) result(subcfg)
         type(Configuration) :: subcfg
         class(Configuration), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
      end function of_multi_selector

      module function has_selector(this, s1, OPT_SELECTORS) result(has)
         logical :: has
         class(Configuration), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
      end function has_selector


      module subroutine get_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         logical, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_logical


      module subroutine get_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         logical, allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_logical_1d
   

      module subroutine get_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         character(:), allocatable, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_string


      module subroutine get_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         integer(kind=INT32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer32

      module subroutine get_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         integer(kind=INT32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer32_1d

      module subroutine get_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         integer(kind=INT64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer64

      module subroutine get_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         integer(kind=INT64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer64_1d


      module subroutine get_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         real(kind=REAL32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real32

      module subroutine get_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         real(kind=REAL32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real32_1d

      module subroutine get_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         real(kind=REAL64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real64

      module subroutine get_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         real(kind=REAL64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real64_1d

      module subroutine get_subconfig(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(Configuration), target, intent(in) :: this
         type(Configuration), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_subconfig


      module subroutine assign_to_logical(flag, this)
         logical, intent(out) :: flag
         class(Configuration), intent(in) :: this
      end subroutine assign_to_logical

      module subroutine assign_to_string(string, this)
         character(:), allocatable, intent(out) :: string
         class(Configuration), intent(in) :: this
      end subroutine assign_to_string

      module subroutine assign_to_integer32(i32, this)
         integer(kind=INT32), intent(out) :: i32
         class(Configuration), intent(in) :: this
      end subroutine assign_to_integer32

      module subroutine assign_to_integer64(i64, this)
         integer(kind=INT64), intent(out) :: i64
         class(Configuration), intent(in) :: this
      end subroutine assign_to_integer64

      module subroutine assign_to_real32(r32, this)
         real(kind=REAL32), intent(out) :: r32
         class(Configuration), intent(in) :: this
      end subroutine assign_to_real32

      module subroutine assign_to_real64(r64, this)
         real(kind=REAL64), intent(out) :: r64
         class(Configuration), intent(in) :: this
      end subroutine assign_to_real64




      module logical function is_sequence(this)
         class(Configuration), intent(in) :: this
      end function is_sequence

      module logical function is_mapping(this)
         class(Configuration), intent(in) :: this
      end function is_mapping

      module logical function is_scalar(this)
         class(Configuration), intent(in) :: this
      end function is_scalar

      module logical function is_bool(this)
         class(Configuration), intent(in) :: this
      end function is_bool

      module logical function is_string(this)
         class(Configuration), intent(in) :: this
      end function is_string

      module logical function is_int(this)
         class(Configuration), intent(in) :: this
      end function is_int

      module logical function is_float(this)
         class(Configuration), intent(in) :: this
      end function is_float


      ! Factory methods to create an iterator
      module function begin_cfg(this, unusable, err_msg, rc) result(iter)
         use fy_KeywordEnforcer
         type(MappingIterator) :: iter
         class(Configuration), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(out) :: err_msg
         integer, optional, intent(out) :: rc
      end function begin_cfg

      module function end_cfg(this, unusable, err_msg, rc) result(iter)
         use fy_KeywordEnforcer
         type(MappingIterator) :: iter
         class(Configuration), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(out) :: err_msg
         integer, optional, intent(out) :: rc
      end function end_cfg

      module subroutine clear(this)
         class(Configuration), intent(inout) :: this
      end subroutine clear

      module subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
         class(Configuration), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine write_formatted

   end interface

contains

   function new_Configuration(node) result(config)
      type(Configuration) :: config
      class(AbstractNode), target, intent(in) :: node

      config%node => node

   end function new_Configuration


end module fy_Configuration
