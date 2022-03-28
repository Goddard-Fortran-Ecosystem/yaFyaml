module fy_newMapping
   use fy_Sequence
   use fy_AbstractNode
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private

   public :: newMapping
   public :: newMappingIterator
   public :: operator(==)
   public :: operator(/=)
   
   type :: newMapping
      private
      type(sequence) :: keys
      type(sequence) :: values
   contains
      procedure :: at
      procedure :: of => of
      procedure :: insert
      procedure :: set
      procedure :: size => size_mapping
      procedure :: count
      procedure :: begin => begin_mapping
      procedure :: end => end_mapping
      procedure :: clear => clear_mapping
   end type newMapping


   type :: newMappingIterator
!      private
      type(newMapping), pointer :: reference
      integer :: i
   contains
      procedure :: first
      procedure :: second
      procedure :: next => next_iter
   end type newMappingIterator

   interface operator(==)
      module procedure :: equals
   end interface operator(==)

   interface operator(/=)
      module procedure :: not_equals
   end interface operator(/=)

   interface newMapping
      module procedure :: new_newMapping
   end interface newMapping

contains

   function new_newMapping() result(m)
      type(newMapping) :: m
   end function new_newMapping


   function at(this, key, rc) result(p_value)
      class(AbstractNode), pointer :: p_value
      class(newMapping), target, intent(in) :: this
      class(AbstractNode), intent(in) :: key
      integer, optional, intent(out) :: rc 

      integer :: i

      if (present(rc)) rc = 0
      p_value => null()
      do i = 1, this%keys%size()
          if (.not. (key < this%keys%of(i) .or. this%keys%of(i) < key)) then
            p_value => this%values%of(i)
            return
         end if
      end do

      if (present(rc)) rc = 1

   end function at

   function of(this, key) result(p_value)
      class(AbstractNode), pointer :: p_value
      class(newMapping), target, intent(in) :: this
      class(AbstractNode), intent(in) :: key

      p_value => this%at(key)
   end function of


   subroutine insert(this, key, value)
      class(newMapping), intent(inout) :: this
      class(AbstractNode), intent(in) :: key
      class(AbstractNode), intent(in) :: value

      call this%keys%push_back(key)
      call this%values%push_back(value)

   end subroutine insert

   subroutine set(this, key, value)
      class(newMapping), intent(inout) :: this
      class(AbstractNode), intent(in) :: key
      class(AbstractNode), intent(in) :: value

      integer :: i

      do i = 1, this%keys%size()
         if (.not. (key < this%keys%of(i) .or. this%keys%of(i) < key)) then
            call this%values%set(i, value)
            return
         end if
      end do

      ! new key
      call this%insert(key, value)

   end subroutine set

   integer(kind=INT64) function size_mapping(this)
      class(newMapping), intent(in) :: this
      size_mapping = this%keys%size()
   end function size_mapping
   

   integer function count(this, key)
      class(newMapping), intent(in) :: this
      class(AbstractNode), intent(in) :: key
      
      integer :: i

      count = 0
      do i = 1, this%keys%size()
         if (.not. (key < this%keys%of(i) .or. this%keys%of(i) < key)) then
             count = 1
            return
         end if
      end do
   end function count

   function begin_mapping(this) result(iter)
      type(newMappingIterator) :: iter
      class(newMapping), target, intent(in) :: this

      iter%reference => this
      iter%i = 1
   end function begin_mapping

   function end_mapping(this) result(iter)
      type(newMappingIterator) :: iter
      class(newMapping), target, intent(in) :: this

      iter%reference => this
      iter%i = this%size() + 1
   end function end_mapping

   subroutine clear_mapping(this)
      class(newMapping), intent(inout) :: this
      call this%keys%clear()
      call this%values%clear()
   end subroutine clear_mapping

   !*****************
   ! Iterator methods
   !*****************

   function first(this)
      class(AbstractNode), pointer :: first
      class(newMappingIterator) :: this

      associate (ref => this%reference)
        first => ref%keys%of(this%i)
      end associate
   end function first

   function second(this)
      class(AbstractNode), pointer :: second
      class(newMappingIterator) :: this

      associate (ref => this%reference)
        second => ref%values%of(this%i)
      end associate

   end function second

   subroutine next_iter(this)
      class(newMappingIterator), intent(inout) :: this
      this%i = this%i + 1
   end subroutine next_iter

   logical function equals(this, other)
      type(newMappingIterator), intent(in) :: this
      type(newMappingIterator), intent(in) :: other

      equals = this%i == other%i
   end function equals

   logical function not_equals(this, other)
      type(newMappingIterator), intent(in) :: this
      type(newMappingIterator), intent(in) :: other

      not_equals = .not. (this%i == other%i)
   end function not_equals
end module fy_newMapping
