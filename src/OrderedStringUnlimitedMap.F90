module fy_OrderedStringUnlimitedMap
  use gFTL_StringIntegerMap
  use gFTL_StringVector
  use gFTL_UnlimitedVector
  use gFTL_StringUnlimitedMap, only: StringUnlimitedPair
  implicit none
  private

  public :: OrderedStringUnlimitedMap
  public :: OrderedStringUnlimitedMapIterator
  public :: StringUnlimitedPair
  type :: OrderedStringUnlimitedMap
     private
     type(StringIntegerMap) :: map
     type(StringVector) :: keys
     type(UnlimitedVector) :: values
   contains
     procedure :: empty
     procedure :: size
     procedure, nopass :: max_size
     
     procedure :: insert_key_value
     procedure :: insert_pair
     generic :: insert => insert_key_value
     generic :: insert => insert_pair
     
     procedure :: of ! [] operator
     procedure :: at
     procedure :: erase_one
     generic :: erase => erase_one
     procedure :: clear
     procedure :: get
     procedure :: m_set
     
     procedure :: begin
     procedure :: end
     procedure :: find
     
     procedure :: count
     procedure :: deepCopy
     
  end type OrderedStringUnlimitedMap

  type :: OrderedStringUnlimitedMapIterator
     private
     class(OrderedStringUnlimitedMap), pointer :: reference
     type(StringVectorIterator) :: k_iter
     type(UnlimitedVectorIterator) :: v_iter
     integer :: current = 0
   contains
     procedure :: value
     procedure :: key
     procedure :: next
     procedure :: previous
     procedure :: iter_equal
     generic :: operator(==) => iter_equal
     procedure :: iter_not_equal
     generic :: operator(/=) => iter_not_equal
  end type OrderedStringUnlimitedMapIterator

  interface OrderedStringUnlimitedMap
     module procedure new_OrderedStringUnlimitedMap
  end interface OrderedStringUnlimitedMap

  integer, parameter :: SIZE_KIND =                                        &
       &           max(kind(1),selected_int_kind(18))

contains

  function new_OrderedStringUnlimitedMap() result(m)
    type(OrderedStringUnlimitedMap) :: m
    m%map = StringIntegerMap()
    m%keys = StringVector()
    m%values = UnlimitedVector()
  end function new_OrderedStringUnlimitedMap

  logical function empty(this)
    class (OrderedStringUnlimitedMap), intent(in) :: this
    empty = this%map%empty()
  end function empty
  
  function size(this)
    integer(kind=SIZE_kind) :: size
    class (OrderedStringUnlimitedMap), intent(in) :: this
    size = this%map%size()
  end function size


  subroutine insert_key_value(this, key, value)
    class(OrderedStringUnlimitedMap), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value

    integer :: n

    n = this%keys%size()
    call this%map%insert(key,n+1)
    call this%keys%push_back(key)
    call this%values%push_back(value)

  end subroutine insert_key_value

  subroutine insert_pair(this, p)
    class(OrderedStringUnlimitedMap), intent(inout) :: this
    type(StringUnlimitedPair), intent(in) :: p

    call this%insert(p%key, p%value)

  end subroutine insert_pair


  function get(this, key, value) result(res)
    logical :: res
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    character(len=*)  :: key
    class(*) , pointer, intent(out) :: value

    integer, pointer :: n

    res = this%map%get(key, n)
    value => this%values%at(n)

    return

  end function get

  ! Note: does not change insertion order.
  subroutine m_set(this, key, value) 
    class(OrderedStringUnlimitedMap), intent(inout) :: this
    character(len=*) , intent(in) :: key
    class(*) , intent(in) :: value

    integer, pointer :: n
    logical :: res

    res = this%map%get(key,n)
    if (res) then
       call this%values%set(n,value)
    else
       call this%insert(key,value)
    end if

  end subroutine m_set

  function of(this, key) result(res)
    class(OrderedStringUnlimitedMap), target, intent(inout) :: this
    character(len=*) , intent(in) :: key
    class(*) , pointer :: res

    integer :: n

    n = this%map%of(key)
    res => this%values%at(n)

  end function of

  function at(this, key) result(res)
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    character(len=*) , intent(in) :: key
    class(*), pointer :: res

    integer, pointer :: n

    n => this%map%at(key)
    if (associated(n)) then
       res => this%values%at(n)
    else
       res => null()
    end if

  end function at


  subroutine erase_one(this, iter)
    class(OrderedStringUnlimitedMap), intent(inout) :: this
    type(OrderedStringUnlimitedMapIterator), intent(inout) :: iter 

    character(:), pointer :: key
    type(StringIntegerMapIterator) :: map_iter
    type(StringVectorIterator) :: k_iter
    type(UnlimitedVectorIterator) :: v_iter

    integer :: n
    integer, pointer :: nn

    n = iter%current
    key => this%keys%at(n)

    map_iter = this%map%begin()
    do while (map_iter /= this%map%end())
       nn => map_iter%value()
       if (nn > n) then
          nn = nn - 1
       else
          call this%map%erase(map_iter)
       end if
       call map_iter%next()
    end do

    call this%keys%erase(iter%k_iter)
    call this%values%erase(iter%v_iter)

  end subroutine erase_one

  subroutine clear(this)
    class(OrderedStringUnlimitedMap), intent(inout) :: this

    call this%map%clear()
    call this%keys%clear()
    call this%values%clear()

  end subroutine clear


  function begin(this) result(iter)
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    type (OrderedStringUnlimitedMapIterator) :: iter
    
    iter%reference => this
    iter%current = 1
    iter%k_iter = this%keys%begin()
    iter%v_iter = this%values%begin()

  end function begin


! =======================
!  end
! =======================
  function end(this) result(iter)
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    type (OrderedStringUnlimitedMapIterator) :: iter

    iter%reference => this
    iter%current = this%keys%size() + 1
    iter%k_iter = this%keys%end()
    iter%v_iter = this%values%end()

  end function end


  ! slow search - consider using map instead
  function find(this, key) result(iter)
    type (OrderedStringUnlimitedMapIterator) :: iter
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    character(len=*) , intent(in) :: key

    type(StringIntegerMapIterator) :: m_iter

    m_iter = this%map%find(key)

    iter%reference => this
    iter%current = m_iter%value()

    iter%k_iter = this%keys%begin()
    iter%k_iter = iter%k_iter + (iter%current-1)
    iter%v_iter = this%values%begin()
    iter%v_iter = iter%v_iter + (iter%current-1)
    
  end function find


  function count(this, key)
    integer(kind=SIZE_KIND) :: count
    class(OrderedStringUnlimitedMap), intent(in) :: this
    character(len=*) , intent(in) :: key

    count = this%map%count(key)

  end function count


  subroutine deepCopy(this, original)
    class(OrderedStringUnlimitedMap), intent(out) :: this
    class(OrderedStringUnlimitedMap), intent(in) :: original

    call this%map%deepCopy(original%map)
    this%keys = original%keys
    this%values = original%values
    
  end subroutine deepCopy
  
! =======================
!  value
! =======================
      function value(this) result(res)
        class(OrderedStringUnlimitedMapIterator), target, intent(in) :: this
        class(*) , pointer :: res

        character(:), pointer :: key

        res => this%v_iter%get()

      end function value

! =======================
!  key
! =======================
      function key(this) result(res)
        class(OrderedStringUnlimitedMapIterator), target, intent(in) :: this
        character(len=:), pointer :: res

        res => this%k_iter%get()

      end function key


! =======================
!  operator(==)
! =======================
      logical function iter_equal(this, other) result(equal)
        class(OrderedStringUnlimitedMapIterator), intent(in) :: this
        type(OrderedStringUnlimitedMapIterator), intent(in) :: other

        equal = (this%current == other%current)
        ! Other components must agree (at least in theory ...)

      end function iter_equal


! =======================
!  operator(/=)
! =======================
      logical function iter_not_equal(this, other)                          &
     &   result(not_equal)
         class(OrderedStringUnlimitedMapIterator), intent(in) :: this
         type(OrderedStringUnlimitedMapIterator), intent(in) :: other

         not_equal = .not. (this == other)
       end function iter_not_equal


! =======================
!  next
! =======================
      subroutine next(this)
         class(OrderedStringUnlimitedMapIterator), intent(inout) :: this

         this%current = this%current + 1
         call this%k_iter%next()
         call this%v_iter%next()

       end subroutine next


! =======================
!  previous
! =======================
      subroutine previous(this)
         class(OrderedStringUnlimitedMapIterator), intent(inout) :: this

         this%current = this%current - 1
         call this%k_iter%previous()
         call this%v_iter%previous()

       end subroutine previous



! =======================
!  max_size
! =======================
!  limited by 32 bit integer in terms of result
      function max_size()
         integer(kind=SIZE_KIND) :: max_size

         max_size = huge(1_SIZE_KIND)

      end function max_size

  
end module fy_OrderedStringUnlimitedMap
