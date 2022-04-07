#include "error_handling.h"
#include "string_handling.h"

submodule (fy_MappingNode) MappingNode_implementation
   use fy_YAML_Node
   use fy_ErrorCodes
   implicit none


contains
   ! ordering:
   ! (bool, int, string, float, sequence, mapping)

   ! The innards of this algorithm should eventually migrate to gFTL algorithms.
   module function less_than(a, b)
      logical :: less_than
      class(MappingNode), intent(in) :: a
      class(YAML_Node), intent(in) :: b

      type(MappingIterator) :: iter_a, iter_b

      select type (b)
      type is (MappingNode)
         ! We must determine which dictionary to treat as "first" to avoid
         ! paradoxical cases.   Find which is less ignoring order:

         associate (a_begin => a%value%begin(), a_end => a%value%end())
           associate (b_begin => b%value%begin() , b_end => b%value%end())
            ! compare first key
             iter_a = a_begin
             iter_b = b_begin
             do
                if (iter_a == a_end) then
                   if (iter_b == b_end) then ! are equal
                      less_than = .false.
                      return
                   else ! b still has more members
                      less_than = .true.
                      return
                   end if
                else if (iter_b == b_end) then ! a still has more members
                   less_than = .false.
                   return
                else ! both have members so continue

                   ! compare keys
                   if (iter_a%first() < iter_b%first()) then
                      less_than = ordered_compare(a,b,swap=.false.)
                      return
                   else if (iter_b%first() < iter_a%first()) then
                      less_than = ordered_compare(b,a,swap=.true.)
                      return
                   else ! compare values
                      if (iter_a%second() < iter_b%second()) then
                         less_than = ordered_compare(a,b,swap=.false.)
                         return
                      else if (iter_b%second() < iter_a%second()) then
                         less_than = ordered_compare(b,a,swap=.true.)
                         return
                      end if
                   end if
                end if
                ! keeep going
                call iter_a%next()
                call iter_b%next()
             end do

           end associate
         end associate

      class default ! everything else is less than a mapping
         less_than = .false.
      end select

   contains

      logical function ordered_compare(first, second, swap) result(less_than)
         class(MappingNode), target, intent(in) :: first
         class(MappingNode), target, intent(in) :: second
         logical, intent(in) :: swap
         
         class(YAML_Node), pointer :: key_1, value_2
         type(MappingIterator) :: iter_1
         integer :: n1, n2, i
         integer :: status
         
         n1 = first%value%size()
         n2 = second%value%size()

         associate (b1 => first%value%begin(), e1 => first%value%end())
           associate (b2 => second%value%begin(), e2 => second%value%end())
             
             iter_1 = b1
             do i = 1, min(n1, n2)
                
                key_1 => iter_1%first()
                ! Does second have the same key?
                if (second%value%count(key_1) > 0) then
                   ! compare values
                   value_2 => second%value%at(key_1,rc=status)
                   if (status/= YAFYAML_SUCCESS) error stop "not possible"
                   
                   if (iter_1%second() < value_2) then
                      if (swap) then
                         less_than = .false.
                      else
                         less_than = .true.
                      end if
                      return
                   else if (value_2 < iter_1%second()) then
                      if (swap) then
                         less_than = .true.
                      else
                         less_than = .false.
                      end if
                      return
                   end if
                   
                else ! b does not have key and is therefore "less".
                   if (swap) then
                      less_than = .true.
                   else
                      less_than = .false.
                   end if
                   return
                end if
                
                call iter_1%next()
             end do
           end associate
         end associate

         ! all elements equal so far, but does b have more elements still?
         if (swap) then
            less_than = (n2 < n1)
         else
            less_than = (n1 < n2)
         end if

      end function ordered_compare

   end function less_than

   recursive module subroutine clone_mapping_node(from, to)
      use fy_SequenceNode
      use fy_Sequence
      type(MappingNode), target, intent(in) :: from
      class(YAML_Node), target, intent(out) :: to
      
      type(Mapping), pointer :: m_a, m_b
      type(MappingIterator) :: iter
      class(YAML_Node), pointer :: key, val
      class(YAML_Node), pointer :: subobject

      m_a => to_mapping(from)
      select type (to)
      type is (MappingNode)
         m_b => to_mapping(to)
         call clone(m_a, m_b)
      class default
         error stop "Should not be possible."
      end select

   end subroutine clone_mapping_node

   recursive module subroutine clone_mapping(from, to)
      use fy_SequenceNode
      use fy_Sequence
      type(Mapping), target, intent(in) :: from
      type(Mapping), target, intent(out) :: to

      type(MappingIterator) :: iter
      class(YAML_Node), pointer :: key, val
      class(YAML_Node), pointer :: subobject

      associate (beg => from%begin(), e => from%end())                                                                                      
        iter = beg                                                                                                                    
        do while (iter /= e)                                                                                                          
           key => iter%first()                                                                                                        
           val => iter%second()                                                                                                       
           select type (q => val)                                                                                                     
           type is (SequenceNode)                                                                                                     
              call to%insert(key, SequenceNode())                                                                                      
              subobject => to%of(key)                                                                                                  
              select type (qq => subobject)                                                                                           
              type is (SequenceNode) ! guaranteed                                                                                     
                 call clone(q, qq)                                                                                                    
              end select                                                                                                              
           type is (MappingNode)
              call to%insert(key, MappingNode())
              subobject => to%of(key)
              select type (qq => subobject)
              type is (MappingNode) ! guaranteed
                 call clone(q, qq)              
              end select
           class default ! scalar
              call to%insert(key, val)
           end select
           call iter%next()
        end do
      end associate
   end subroutine clone_mapping

   module function first(this, unusable, err_msg, rc) result(ptr)
      use fy_keywordenforcer, only: KE => KeywordEnforcer
      class(YAML_Node), pointer :: ptr
      class(MappingNodeIterator), intent(in) :: this
      class(KE), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      integer :: status

      ptr => this%map_iter%first()

      __RETURN__(YAFYAML_SUCCESS)
   end function first

   module function second(this, unusable, err_msg, rc) result(ptr)
      use fy_keywordenforcer, only: KE => KeywordEnforcer
      class(YAML_Node), pointer :: ptr
      class(MappingNodeIterator), intent(in) :: this
      class(KE), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      integer :: status

      ptr => this%map_iter%second()

      __RETURN__(YAFYAML_SUCCESS)
   end function second



end submodule MappingNode_implementation
   
