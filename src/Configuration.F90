! Three types of accessor procedures are provided.

!   I. Subroutines that return pointers, and allow for errors.  These
!      are arguably less elegant to use, but have the fewest compiler
!      issues and are likely necessary in complex applications that
!      must propagate error status to higher levels.
!
!      Forms: overloaded for logical, integer, real, string; scalar,
!              1D, and gFTL Vector & gFTL Map
!
!            CALL  cfg%at(ptr, key=<string>, is_present=is_present, rc=status)
!            CALL  cfg%at(ptr, index=<integer>, is_present=is_present, rc=status)
!
!      Unsuccessful calls set ptr to NULL().

!      If calls fail because key/index do not exist, but is_present is PRESENT, the
!      rc is zero.  I.e. it is not an error condition.
!      (Later we may support integer keys as well.)
!
!  II. Subroutines that return values and allow for default values and
!      errors.  These are very similar to the above but can require
!      copying complex data structures for nested components, and
!      therefore are more likely to encounter compiler bugs.  In
!      particular, during development, gfortran 9.2, has difficulty
!      with these, but ifort 19.0.5 and nag 6.2 (6247) were fine.
!
!      Forms: overloaded for logical, integer, real, string; scalar,
!              1D, and gFTL Vector & gFTL Map
!
!            CALL  cfg%get(value, key=<string>, default=<default>, is_present, rc=status)
!            CALL  cfg%get(value, index=<integer>, default=<default>, is_present, rc=status)
!
!      If calls fail because key/index do not exist, but is_present is PRESENT, the
!      rc is zero.  I.e. it is not an error condition.
!
! III. For those that like to live dangerously, a simplified operator
!      is provided to drill into the data structure.  If any errors
!      are encountered a (hopefully-informative) message is printed
!      and execution is terminated (ERROR STOP).  This is
!      esp. dangerous in a parallel application where one process may
!      terminate.    Users can set their own error handler which may
!      help
!
!      Forms: overloaded for logical, integer, real, string; scalar,
!
!
!      q = cfg .at. <key> .at. <index> .at. <key2>
!


#include 'error_handling.h'
module fy_Configuration
  use fy_ArrayWrapper
  use fy_KeywordEnforcer
  use fy_None
  use gFTL_UnlimitedVector
  use fy_OrderedStringUnlimitedMap
  use fy_String
  use fy_ErrorCodes
  use fy_ErrorHandling
  implicit none
  private

  public :: Configuration

  type, abstract :: BaseNode
   contains
     procedure(get_node_interface), deferred :: get_node
  end type BaseNode

  type, extends(BaseNode) :: AllocatableNode
     private
     class(*), allocatable :: node
   contains
     procedure :: get_node => get_node_AllocatableNode
  end type AllocatableNode

  abstract interface
     function get_node_interface(this) result(node)
       import BaseNode
       class(*), pointer :: node
       class(BaseNode), target, intent(in) :: this
     end function get_node_interface
  end interface

  type, extends(BaseNode) :: PointerNode
     private
     class(*), pointer :: node
   contains
     procedure :: get_node => get_node_PointerNode
  end type PointerNode
     

  type :: Configuration
     private
     class(BaseNode), allocatable :: node
     class(*), pointer :: node_reference => null()
   contains

     ! Return Config reference at selector(s)
     procedure :: at
     procedure :: at_index
     procedure :: at_key
     generic :: operator(.at.) => at_index
     generic :: operator(.at.) => at_key

     ! Get pointer to substructure at selector
     procedure :: get_node_at_selector
     procedure :: get_config_at_selector
     generic :: get => get_config_at_selector


     ! Cast to containers
     procedure, pass(this) :: to_map
     procedure, pass(this) :: to_unlimited_vector
     generic :: assignment(=) => to_map
     generic :: assignment(=) => to_unlimited_vector
     
     ! Cast to direct scalars (simple document)
     procedure, pass(this) :: to_logical
     procedure, pass(this) :: to_integer
     procedure, pass(this) :: to_real
     procedure, pass(this) :: to_string
     generic :: assignment(=) => to_logical
     generic :: assignment(=) => to_integer
     generic :: assignment(=) => to_real
     generic :: assignment(=) => to_string

     ! Cast to arrays
     procedure, pass(this) :: to_logical_array
     procedure, pass(this) :: to_integer_array
     procedure, pass(this) :: to_real_array
     procedure, pass(this) :: to_string_array
     generic :: assignment(=) => to_logical_array
     generic :: assignment(=) => to_integer_array
     generic :: assignment(=) => to_real_array
     generic :: assignment(=) => to_string_array ! fixed length

!!$     ! Cast to vectors
!!$     procedure, pass(this) :: to_logical_vector
!!$     procedure, pass(this) :: to_integer_vector
!!$     procedure, pass(this) :: to_real_vector
     procedure, pass(this) :: to_string_vector
!!$     generic :: assignment(=) => to_logical_vector
!!$     generic :: assignment(=) => to_integer_vector
!!$     generic :: assignment(=) => to_real_vector
     generic :: assignment(=) => to_string_vector
     
     procedure :: write_formatted
     generic :: write(formatted) => write_formatted

     
!!$
!!$     ! Maybe not these?  Compilers make them problematic, and Fortran
!!$     ! requirements mean that they cannot be references.
!!$     ! Access to map
!!$     generic :: assignment(=) => to_map
!!$     ! Access to vector
!!$     generic :: assignment(=) => to_sequence
!!$

     procedure :: is_none

     procedure :: get_integer_at_key
     procedure :: get_string_at_key
     procedure :: get_real_at_key
     procedure :: get_logical_at_key

     generic :: get => get_integer_at_key
     generic :: get => get_string_at_key
     generic :: get => get_logical_at_key
     generic :: get => get_real_at_key
  end type Configuration


  interface Configuration
     module procedure new_Configuration_scalar ! including map and 
     module procedure new_Configuration_array
  end interface Configuration


contains


  function new_Configuration_scalar(scalar) result(config)
    type (Configuration) :: config
    class(*), intent(in) :: scalar

    allocate(AllocatableNode :: config%node)

    select type (q => config%node)
    type is (AllocatableNode)
       allocate(q%node, source=scalar)
    class is (BaseNode)
    end select

  end function new_Configuration_scalar


  function new_Configuration_array(array) result(config)
    type (Configuration) :: config
    class(*), intent(in) :: array(:)

    type(ArrayWrapper) :: w

    allocate(w%elements, source=array)

    allocate(AllocatableNode :: config%node)
    select type (q => config%node)
    type is (AllocatableNode)
       allocate(q%node, source=w)
    end select

  end function new_Configuration_array

  function get_node_AllocatableNode(this) result(node)
    class(*), pointer :: node
    class(AllocatableNode), target, intent(in) :: this
    node => this%node
  end function get_node_AllocatableNode

  function get_node_PointerNode(this) result(node)
    class(*), pointer :: node
    class(PointerNode), target, intent(in) :: this
    node => this%node
  end function get_node_PointerNode



#define ARG_LIST arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9


  subroutine get_node_at_selector(this, q, ARG_LIST, unused, is_present, rc)
    class(Configuration), target, intent(in) :: this
    class(*), pointer :: q
    class(*), optional, intent(in) :: arg1
    class(*), optional, intent(in) :: arg2
    class(*), optional, intent(in) :: arg3
    class(*), optional, intent(in) :: arg4
    class(*), optional, intent(in) :: arg5
    class(*), optional, intent(in) :: arg6
    class(*), optional, intent(in) :: arg7
    class(*), optional, intent(in) :: arg8
    class(*), optional, intent(in) :: arg9
    class(KeywordEnforcer), optional, intent(in) :: unused
    logical, optional, intent(out) :: is_present
    integer, optional, intent(out) :: rc

    type (UnlimitedVector) :: v
    type (UnlimitedVectorIterator) :: iter
    class(*), pointer :: arg
    class(*), pointer :: node, next_node

    call save_args(v)

    node => this%node%get_node()
    iter = v%begin()
    do while (iter /= v%end())
       arg => iter%get()

       select type (node)
       type is (UnlimitedVector)
          select type (index => arg)
          type is (integer)
             if (index >=1 .and. index <= node%size()) then
                next_node => node%at(index)
             else
                next_node => null()
             end if
          class default
             next_node => null()
          end select
       type is (OrderedStringUnlimitedMap)
          select type (key => arg)
          type is (character(*))
             next_node => node%at(key)
          type is (String)
             next_node => node%at(key%s)
          class default
             next_node => null()
          end select
       class default !  cannot dive down into anything else
          next_node => null()
       end select

       node => next_node
       if (.not. associated(node)) then
          if (present(is_present)) then
             is_present = .false.
          end if
       end if
          
       call iter%next()

    end do

    q => node

    if (present(is_present)) is_present = .true.
    __RETURN__(SUCCESS)
    
  contains

    subroutine save_args(v)
      type (UnlimitedVector), intent(out) :: v

      if (present(arg1)) call v%push_back(arg1)
      if (present(arg2)) call v%push_back(arg2)
      if (present(arg3)) call v%push_back(arg3)
      if (present(arg4)) call v%push_back(arg4)
      if (present(arg5)) call v%push_back(arg5)
      if (present(arg6)) call v%push_back(arg6)
      if (present(arg7)) call v%push_back(arg7)
      if (present(arg8)) call v%push_back(arg8)
      if (present(arg9)) call v%push_back(arg9)
    end subroutine save_args
    
  end subroutine get_node_at_selector
    
  subroutine get_config_at_selector(this, config, ARG_LIST, unused, default, rc)
    class(Configuration), target, intent(in) :: this
    type(Configuration), intent(out) :: config
    class(*), intent(in) :: arg1
    class(*), optional, intent(in) :: arg2
    class(*), optional, intent(in) :: arg3
    class(*), optional, intent(in) :: arg4
    class(*), optional, intent(in) :: arg5
    class(*), optional, intent(in) :: arg6
    class(*), optional, intent(in) :: arg7
    class(*), optional, intent(in) :: arg8
    class(*), optional, intent(in) :: arg9
    class(KeywordEnforcer), optional, intent(in) :: unused
    class(*), optional, intent(in) :: default
    integer, optional, intent(out) :: rc

    integer :: status
    class(*), pointer :: node

    
    
    call this%get_node_at_selector(node, ARG_LIST, rc=status)

    if (.not. associated(node) .and. .not.present(default)) then
       node => None
    end if

    if(associated(node)) then
       allocate(PointerNode :: config%node)
       select type (q => config%node)
       type is (PointerNode)
          q%node => node
       end select
    else ! default must be present
       allocate(AllocatableNode :: config%node)
       select type (q => config%node)
       type is (AllocatableNode)
          allocate(q%node, source=default)
       end select
    end if
       
!!$    VERIFY(status)

  end subroutine get_config_at_selector



  ! i = cfg%at(key1, index1, key2, default=default, rc=status)
  function at(this, ARG_LIST, unused, default, rc) result(q)
    type(Configuration) :: q
    class(Configuration), target, intent(in) :: this
    class(*), intent(in) :: arg1
    class(*), optional, intent(in) :: arg2
    class(*), optional, intent(in) :: arg3
    class(*), optional, intent(in) :: arg4
    class(*), optional, intent(in) :: arg5
    class(*), optional, intent(in) :: arg6
    class(*), optional, intent(in) :: arg7
    class(*), optional, intent(in) :: arg8
    class(*), optional, intent(in) :: arg9
    class(KeywordEnforcer), optional, intent(in) :: unused
    class(*), optional, intent(in) :: default
    integer, optional, intent(out) :: rc

    class(*), pointer :: p
    integer :: status

    allocate(PointerNode :: q%node)
    call this%get_config_at_selector(q, ARG_LIST, default=default, rc=status)
  end function at


  function at_index(this, index) result(sub)
    type(Configuration) :: sub
    class(Configuration), target, intent(in) :: this
    integer, intent(in) :: index

    sub = at(this, index)

  end function at_index

  function at_key(this, key) result(sub)
    type(Configuration) :: sub
    class(Configuration), target, intent(in) :: this
    character(*), intent(in) :: key

    sub = at(this, key)
    
  end function at_key


  subroutine to_map(values, this)
    use gFTL_StringVector
    type (OrderedStringUnlimitedMap), intent(out) :: values
    class(Configuration), intent(in) :: this

    class(*), pointer :: node

    node => this%node%get_node()

    select type(q => node)
    type is (OrderedStringUnlimitedMap)
       values = q
    class default
       values = OrderedStringUnlimitedMap() ! empty map
    end select

  end subroutine to_map

  subroutine to_unlimited_vector(values, this)
    use gFTL_StringVector
    type (UnlimitedVector), intent(out) :: values
    class(Configuration), intent(in) :: this

    class(*), pointer :: node

    node => this%node%get_node()

    select type(q => node)
    type is (UnlimitedVector)
       values = q
    class default
       values = UnlimitedVector() ! empty vector
    end select

  end subroutine to_unlimited_vector



  subroutine to_logical(value, this)
    logical, intent(out) :: value
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    
    node => this%node%get_node()

    select type(q => node)
    type is (logical)
       value = q
    class default
       value = default_logical()
    end select

  end subroutine to_logical

  subroutine to_integer(value, this)
    integer, intent(out) :: value
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    node => this%node%get_node()

    select type(q => node)
    type is (integer) ! default
       value = q
    class default
       value = default_integer()
    end select

  end subroutine to_integer

  subroutine to_real(value, this)
    real, intent(out) :: value
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    node => this%node%get_node()

    select type(q => node)
    type is (real) ! default
       value = q
    class default
       value = default_real()
    end select

  end subroutine to_real
  
  subroutine to_string(value, this)
    character(:), allocatable, intent(out) :: value
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    node => this%node%get_node()

    select type(q => node)
    type is (character(*))
       value = q
    type is (String)
       value = q%s
    class default
       value = default_string()
    end select

  end subroutine to_string
  

  subroutine to_logical_array(values, this)
    logical, allocatable, intent(out) :: values(:)
    class(Configuration), intent(in) :: this

    integer :: i, n
    class(*), pointer :: node

    node => this%node%get_node()

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (logical)
          allocate(values, source=qq)
!!$          values = qq
       class default ! type mismatch
          values = [logical :: ] ! empty array
       end select
    type is (UnlimitedVector)
       ! Check if all elements are logical
       n = q%size()
       do i = 1, n
          select type (qq => q%at(i))
          type is (logical)
          class default
             values = [logical :: ]
             return
          end select
       end do
       allocate(values(n))
       do i = 1, n
          select type (qq => q%at(i))
          type is (logical)
             values(i) = qq
          end select
       end do
    class default ! category mismatch - not an array
       values = [logical :: ] ! empty array
    end select

  end subroutine to_logical_array


  subroutine to_integer_array(values, this)
    integer, allocatable, intent(out) :: values(:)
    class(Configuration), target, intent(in) :: this

    integer :: i, n

    class(*), pointer :: node

    node => this%node%get_node()

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (integer)
          values = qq
       class default ! type mismatch
          values = [integer :: ] ! empty array
       end select
    type is (UnlimitedVector)
       ! Check if all elements are default integer
       n = q%size()
       do i = 1, n
          select type (qq => q%at(i))
          type is (integer)
          class default
             values = [integer :: ]
             return
          end select
       end do
       allocate(values(n))
       do i = 1, n
          select type (qq => q%at(i))
          type is (integer)
             values(i) = qq
          end select
       end do
    class default ! category mismatch - not an array
       values = [integer :: ] ! empty array
    end select

  end subroutine to_integer_array


  subroutine to_real_array(values, this)
    real, allocatable, intent(out) :: values(:)
    class(Configuration), intent(in) :: this

    integer :: i, n

    class(*), pointer :: node
    node => this%node%get_node()

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (real)
          values = qq
       class default ! type mismatch
          values = [real :: ] ! empty array
       end select
    type is (UnlimitedVector)
       ! Check if all elements are default real
       n = q%size()
       do i = 1, n
          select type (qq => q%at(i))
          type is (real)
          class default
             values = [real :: ]
             return
          end select
       end do
       allocate(values(n))
       do i = 1, n
          select type (qq => q%at(i))
          type is (real)
             values(i) = qq
          end select
       end do
    class default ! category mismatch - not an array
       values = [real :: ] ! empty array
    end select

  end subroutine to_real_array


  subroutine to_string_array(values, this)
    character(:), allocatable, intent(out) :: values(:)
    class(Configuration), intent(in) :: this

    integer :: i, n
    integer :: maxlen

    class(*), pointer :: node
    node => this%node%get_node()

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (character(*))
          values = qq
       class default ! type mismatch
          values = [character(0) :: ] ! empty array
       end select
    type is (UnlimitedVector)
       ! Check if all elements are REAL32
       n = q%size()
       maxlen = 0
       do i = 1, n
          select type (qq => q%at(i))
          type is (character(*))
             maxlen = max(maxlen, len(qq))
          type is (String)
             maxlen = max(maxlen, len(qq%s))
          class default
             values = [character(0) :: ] ! empty array
             return
          end select
       end do
       allocate(character(maxlen) :: values(n))
       do i = 1, n
          select type (qq => q%at(i))
          type is (character(*))
             values(i) = qq
          type is (String)
             values(i) = qq%s
          end select
       end do
    class default ! category mismatch - not an array
       values = [character(0) :: ] ! empty array
    end select

  end subroutine to_string_array

  subroutine to_string_vector(values, this)
    use gFTL_StringVector
    type (StringVector), intent(out) :: values
    class(Configuration), intent(in) :: this

    integer :: i, n

    class(*), pointer :: node


    node => this%node%get_node()

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (character(*))
          do i = 1, size(qq)
             call values%push_back(qq(i))
          end do
       type is (String)
          do i = 1, size(qq)
             call values%push_back(qq(i)%s)
          end do
       class default ! type mismatch
          values = StringVector()
       end select
    type is (UnlimitedVector)
       ! Check if all elements are character(*)
       n = q%size()
       do i = 1, n
          select type (qq => q%at(i))
          type is (character(*))
          type is (String)
          class default
             values = StringVector()
             return
          end select
       end do
       do i = 1, n
          select type (qq => q%at(i))
          type is (character(*))
             call values%push_back(qq)
          type is (String)
             call values%push_back(qq%s)
          end select
       end do
    class default ! category mismatch - not an array
       values = StringVector()
    end select

  end subroutine to_string_vector


  logical function default_logical(default)
    logical, optional, intent(in) :: default

    if (present(default)) then
       default_logical = default
    else
       default_logical = .false.
    end if

  end function default_logical

  integer function default_integer()
    default_integer = -HUGE(1)
  end function default_integer
  
  real function default_real()
    use, intrinsic :: ieee_arithmetic
    default_real = ieee_value(1.,  IEEE_QUIET_NAN)
  end function default_real

  function default_string() result(s)
    character(len=:), allocatable :: s
    s = ''
  end function default_string


  subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
    class(Configuration), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    class(*), pointer :: node

    node => this%node%get_node()
    call write_one(unit, node)

  contains

    recursive subroutine write_one(unit, node)
      integer, intent(in) :: unit
      class(*), intent(in) :: node

      type(OrderedStringUnlimitedMapIterator) :: iter
      integer :: i
      
      iostat = 0
      select type (q => node)
      type is (logical)
         if (q) then
            write(unit,'(a4)',iostat=iostat) 'true'
         else
            write(unit,'(a5)',iostat=iostat) 'false'
         end if
      type is (integer)
         write(unit,'(i0)',iostat=iostat) q
      type is (real)
         write(unit,'(g0)',iostat=iostat) q
      type is (string)
         write(unit,'(a1,a,a1)',iostat=iostat)"'",q%s,"'"
      type is (character(*))
         write(unit,'(a1,a,a1)',iostat=iostat) "'",q,"'"
      type is (UnlimitedVector)
         write(unit,'(a1)') "["
         do i = 1, q%size()
            call write_one(unit,q%at(i))
            if (i < q%size()) then
               write(unit,'(a1)') ","
            end if
         end do
         write(unit,'(a1)') "]"

      type is (OrderedStringUnlimitedMap)
         write(unit,'(a1)')"{"
         iter = q%begin()
         if (iter /= q%end()) then
            call write_one(unit,iter%key())
            write(unit,'(a2)') ": "
            call write_one(unit,iter%value())
            call iter%next()
         end if
         do while (iter /= q%end())
            write(unit,'(a1)') ","
            call write_one(unit,iter%key())
            write(unit,'(a2)') ": "
            call write_one(unit,iter%value())
            call iter%next
         end do
         write(unit,'(a1)') "}"

      class default
         iostat = -1
      end select

    end subroutine write_one
    
  end subroutine write_formatted

  logical function is_none(this)
    class(Configuration), target, intent(in) :: this

    class(*), pointer :: node

    node => this%node%get_node()
    is_none = same_type_as(node, None)

  end function is_none

#define TYPE_NAME integer
#include 'get_value.inc'
#undef TYPE_NAME

#define TYPE_NAME string
#define STRING
#include 'get_value.inc'
#undef STRING
#undef TYPE_NAME

#define TYPE_NAME real
#include 'get_value.inc'
#undef TYPE_NAME

#define TYPE_NAME logical
#include 'get_value.inc'
#undef TYPE_NAME

end module fy_Configuration
