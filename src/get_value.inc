! Must set TYPE_NAME

#define IDENTITY(x) x
#define NAME_(t) IDENTITY(get_)IDENTITY(t)IDENTITY(_at_key)


subroutine NAME_(TYPE_NAME)(this, value, ARG_LIST, unusable, default, is_present, rc)
   class(Configuration), target, intent(in) :: this
#  ifdef STRING
   character(:), allocatable, intent(out) :: value
#  else
   type(TYPE_NAME), intent(out) :: value
#  endif
   class(*), optional, intent(in) :: arg1
   class(*), optional, intent(in) :: arg2
   class(*), optional, intent(in) :: arg3
   class(*), optional, intent(in) :: arg4
   class(*), optional, intent(in) :: arg5
   class(*), optional, intent(in) :: arg6
   class(*), optional, intent(in) :: arg7
   class(*), optional, intent(in) :: arg8
   class(*), optional, intent(in) :: arg9

   class(KeywordEnforcer), optional, intent(in) :: unusable
#  ifdef STRING
   character(*), optional, intent(in) :: default
#  else
   type(TYPE_NAME), optional, intent(in) :: default
#  endif
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
      if (.not. associated(node)) exit

      call iter%next()

   end do

   if (.not. associated(node)) then
      if (.not. (present(is_present) .or. present(default))) then
         __FAIL__(SELECTOR_NOT_FOUND)
      else
         if (present(is_present)) is_present = .false.
         if (present(default)) value = default
         __RETURN__(SUCCESS)
      end if
   else

      select type (q => node)
#     ifdef STRING
      type is (character(*))
#     else
      type is (TYPE_NAME)
#     endif
         value = q
         if (present(is_present)) is_present = .true.
      class default
         if (present(is_present)) is_present = .true.
         if (present(default)) value = default
#ifdef _INTEGER_ 
         print*,'integer'
#endif
#ifdef _REAL_ 
         print*,'real'
#endif
#ifdef _LOGICAL_ 
         print*,'logical'
#endif
#ifdef _STRING_VECTOR_ 
         print*,'string vector'
#endif
#ifdef STRING
         print*,'string'
#endif
         __FAIL__(INCONSISTENT_TYPE)
      end select
   end if

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

end subroutine NAME_(TYPE_NAME)






#undef NAME_
#undef IDENTITY
