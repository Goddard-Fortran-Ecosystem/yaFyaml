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
         __FAIL__(YAFYAML_INVALID_SEQUENCE_INDEX)
      else
         if (present(is_present)) is_present = .false.
         if (present(default)) value = default
         __RETURN__(YAFYAML_SUCCESS)
      end if
   else

      select type (q => node)
#     ifdef STRING
      type is (character(*))
         value = q
         if (present(is_present)) is_present = .true.
      type is (String)
         value = q%s
         if (present(is_present)) is_present = .true.
#     else
      type is (TYPE_NAME)
         value = q
         if (present(is_present)) is_present = .true.
#     endif
      class default
         if (present(is_present)) is_present = .true.
         if (present(default)) value = default
         __FAIL__(YAFYAML_TYPE_MISMATCH)
      end select
   end if

   __RETURN__(YAFYAML_SUCCESS)

contains

    subroutine save_args(v)
      type (UnlimitedVector), intent(out) :: v

      if (present(arg1)) call save_one(v, arg1)
      if (present(arg2)) call save_one(v, arg2)
      if (present(arg3)) call save_one(v, arg3)
      if (present(arg4)) call save_one(v, arg4)
      if (present(arg5)) call save_one(v, arg5)
      if (present(arg6)) call save_one(v, arg6)
      if (present(arg7)) call save_one(v, arg7)
      if (present(arg8)) call save_one(v, arg8)
      if (present(arg9)) call save_one(v, arg9)

   end subroutine save_args

   subroutine save_one(v, arg)
      type(UnlimitedVector), intent(inout) :: v
      class(*), intent(in) :: arg
      select type (arg)
      type is (character(*))
         call v%push_back(String(arg))
      class default
         call v%push_back(arg)
      end select
   end subroutine save_one
    
end subroutine NAME_(TYPE_NAME)






#undef NAME_
#undef IDENTITY
