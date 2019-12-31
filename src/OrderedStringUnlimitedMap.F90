module fy_OrderedStringUnlimitedMap
  use gFTL_StringUnlimitedMap
  use gFTL_StringVector
  implicit none
  private

  public :: OrderedStringUnlimitedMap
  public :: OrderedStringUnlimitedMapIterator

  type :: OrderedStringUnlimitedMap
     private
     type(StringUnlimitedMap) :: map
     type(StringVector) :: insertions
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
     type(StringVectorIterator) :: insertion_iter
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
    m%map = StringUnlimitedMap()
    m%insertions = StringVector()
  end function new_OrderedStringUnlimitedMap

  logical function empty(this)
    class (OrderedStringUnlimitedMap), intent(in) :: this
    empty = this%insertions%empty()
  end function empty
  
  function size(this)
    integer(kind=SIZE_kind) :: size
    class (OrderedStringUnlimitedMap), intent(in) :: this
    size = this%insertions%size()
  end function size


  subroutine insert_key_value(this, key, value)
    class(OrderedStringUnlimitedMap), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value

    call this%map%insert(key, value)
    call this%insertionS%push_back(key)

  end subroutine insert_key_value

  subroutine insert_pair(this, p)
    class(OrderedStringUnlimitedMap), intent(inout) :: this
    type(pair), intent(in) :: p

    call this%map%insert(p)
    call this%insertions%push_back(p%key)

  end subroutine insert_pair


  function get(this, key, value) result(res)
    logical :: res
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    character(len=*)  :: key
    class(*) , pointer, intent(out) :: value

    res = this%map%get(key, value)

    return
  end function get

  ! Note: does not change insertion order.
  subroutine m_set(this, key, value) 
    class(OrderedStringUnlimitedMap), intent(inout) :: this
    character(len=*) , intent(in) :: key
    class(*) , intent(in) :: value

    call this%map%set(key,value)
  end subroutine m_set

  function of(this, key) result(res)
    class(OrderedStringUnlimitedMap), target, intent(inout) :: this
    character(len=*) , intent(in) :: key
    class(*) , pointer :: res

    res => this%map%of(key)
  end function of

  function at(this, key) result(res)
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    character(len=*) , intent(in) :: key
    class(*) , pointer :: res


    res => this%map%at(key)

  end function at


  subroutine erase_one(this, iter)
    class(OrderedStringUnlimitedMap), intent(inout) :: this
    type(OrderedStringUnlimitedMapIterator), intent(inout) :: iter 

    character(:), pointer :: key
    type(StringUnlimitedMapIterator) :: map_iter

    key => iter%key()
    map_iter = this%map%find(key)
    
    call this%map%erase(map_iter)
    call this%insertions%erase(iter%insertion_iter)

  end subroutine erase_one

  subroutine clear(this)
    class(OrderedStringUnlimitedMap), intent(inout) :: this

    call this%map%clear()
    call this%insertions%clear()

  end subroutine clear


  function begin(this) result(iter)
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    type (OrderedStringUnlimitedMapIterator) :: iter
    
    iter%reference => this
    iter%insertion_iter = this%insertions%begin()

  end function begin


! =======================
!  end
! =======================
  function end(this) result(iter)
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    type (OrderedStringUnlimitedMapIterator) :: iter

    iter%reference => this
    iter%insertion_iter = this%insertions%end()

  end function end


  ! slow search - consider using map instead
  function find(this, key) result(iter)
    type (OrderedStringUnlimitedMapIterator) :: iter
    class(OrderedStringUnlimitedMap), target, intent(in) :: this
    character(len=*) , intent(in) :: key

    iter = this%begin()
    do while (iter /= this%end())
       if (key == iter%insertion_iter%get()) exit
       call iter%next()
    end do
    
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
    this%insertions = original%insertions
    
  end subroutine deepCopy
  
! =======================
!  value
! =======================
      function value(this) result(res)
        class(OrderedStringUnlimitedMapIterator), target, intent(in) :: this
        class(*) , pointer :: res

        character(:), pointer :: key

        key => this%insertion_iter%get()
        res => this%reference%map%at(key)

      end function value

! =======================
!  key
! =======================
      function key(this) result(res)
        class(OrderedStringUnlimitedMapIterator), target, intent(in) :: this
        character(len=:), pointer :: res

        res => this%insertion_iter%get()

      end function key


! =======================
!  operator(==)
! =======================
      logical function iter_equal(this, other) result(equal)
        class(OrderedStringUnlimitedMapIterator), intent(in) :: this
        type(OrderedStringUnlimitedMapIterator), intent(in) :: other

        equal = (this%insertion_iter == other%insertion_iter)

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

         call this%insertion_iter%next()

       end subroutine next


! =======================
!  previous
! =======================
      subroutine previous(this)
         class(OrderedStringUnlimitedMapIterator), intent(inout) :: this

         call this%insertion_iter%previous()

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
