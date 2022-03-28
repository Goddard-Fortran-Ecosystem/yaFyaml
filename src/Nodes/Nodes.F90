module fy_Nodes
   use fy_AbstractNode
   use fy_BaseNode


   use fy_BoolNode
   use fy_StringNode
   use fy_IntNode
   use fy_FloatNode
   
   use fy_SequenceNode
   use fy_MappingNode

   use fy_Sequence
   use fy_Mapping
   implicit none
   public
   
   interface assignment(=)
      module procedure assign_to_logical
      module procedure assign_to_string
      module procedure assign_to_integer32
      module procedure assign_to_integer64
      module procedure assign_to_real32
      module procedure assign_to_real64
   end interface assignment(=)

contains

   subroutine assign_to_logical(flag, this)
      logical, intent(out) :: flag
      class(AbstractNode), intent(in) :: this

      flag = to_bool(this)

   end subroutine assign_to_logical

   
   subroutine assign_to_String(str, this)
      character(:), allocatable, intent(out) :: str
      class(AbstractNode), intent(in) :: this

      str = to_string(this)

   end subroutine assign_to_String


   subroutine assign_to_integer32(i32, this)
      use, intrinsic :: iso_fortran_env, only: INT32, INT64
      integer(kind=INT32), intent(out) :: i32
      class(AbstractNode), intent(in) :: this
      integer(kind=INT64) :: i64

      i64 = to_int(this)
      print*,__FILE__,__LINE__, i64, huge(i32)
      if (abs(i64) <= huge(i32)) then
         i32 = i64
      else
         i32 = -huge(i32)
      end if

   end subroutine assign_to_integer32
      
   subroutine assign_to_integer64(i64, this)
      use, intrinsic :: iso_fortran_env, only: INT64
      integer(kind=INT64), intent(out) :: i64
      class(AbstractNode), intent(in) :: this

      i64 = to_int(this)

   end subroutine assign_to_integer64
      
   
   subroutine assign_to_real32(r32, this)
      use, intrinsic :: iso_fortran_env, only: REAL32
      real(kind=REAL32), intent(out) :: r32
      class(AbstractNode), intent(in) :: this

      call this%get_real32(r32)

   end subroutine assign_to_real32
      
   subroutine assign_to_real64(r64, this)
      use, intrinsic :: iso_fortran_env, only: REAL64
      real(kind=REAL64), intent(out) :: r64
      class(AbstractNode), intent(in) :: this

      r64 = to_float(this)

   end subroutine assign_to_real64
      
end module fy_Nodes
