#include "error_handling.h"
#include "string_handling.h"
submodule (fy_SequenceNode) SequenceNode_implementation
   use fy_AbstractNode
   use fy_MappingNode
   implicit none


contains
   ! ordering:
   ! (bool, int, string, float, sequence, mapping)

   ! The innards of this algorithm should eventually migrate to gFTL algorithms.
   module function less_than(a, b)
      logical :: less_than
      class(SequenceNode), intent(in) :: a
      class(AbstractNode), intent(in) :: b

      integer :: i, na, nb

      select type (b)
      type is (MappingNode)
         less_than = .true.
      type is (SequenceNode)

         na = a%value%size()
         nb = b%value%size()
         do i = 1, min(na, nb)
            if (a%value%of(i) < b%value%of(i)) then
               less_than = .true.
               return
            else if (b%value%of(i) < a%value%of(i)) then
               less_than = .false.
               return
            end if
         end do

         less_than = (na < nb)
      class default
         less_than = .false.
      end select

   end function less_than

   recursive module subroutine clone_sequence_node(from, to)
      type(SequenceNode), target, intent(in) :: from
      class(AbstractNode), target, intent(out) :: to

      type(sequence), pointer :: s_a, s_b

      s_a => to_sequence(from)
      select type(to)
      type is (SequenceNode)
         s_b => to_sequence(to)
         call clone(s_a, s_b)
      class default
         error stop "Should not happen."
      end select

   end subroutine clone_sequence_node


   recursive module subroutine clone_sequence(from, to)
      type(Sequence), target, intent(in) :: from
      type(Sequence), target, intent(out) :: to

      type(SequenceIterator) :: iter
      class(AbstractNode), pointer :: item
      class(AbstractNode), pointer :: subobject

      associate (beg => from%begin(), e => from%end())                                                                                      
        iter = beg                                                                                                                    
        do while (iter /= e)                                                                                                          
           item => iter%of()                                                                                                          
           select type (q => item)                                                                                                    
           type is (SequenceNode)                                                                                                     
              call to%push_back(SequenceNode())                                                                                        
              subobject => to%back()                                                                                                   
              select type (qq => subobject)                                                                                           
              type is (SequenceNode) ! guaranteed                                                                                     
                 call clone(q, qq)                                                                                                    
              end select                                                                                                              
           type is (MappingNode)                                                                                                      
              call to%push_back(MappingNode())                                                                                         
              subobject => to%back()                                                                                                   
              select type (qq => subobject)                                                                                           
              type is (MappingNode) ! guaranteed                                                                                      
                 call clone(q, qq)                                                                                                    
              end select                                                                                                              
           class default ! scalar                                                                                                     
              call to%push_back(item)                                                                                                  
           end select                                                                                                                 
           call iter%next()                                                                                                           
        end do                                                                                                                        
      end associate                                                                                                                   
   end subroutine clone_sequence

   module function as_bool(this, bool, unusable, err_msg, rc) result(ptr)
      use fy_BoolNode
      logical, pointer :: ptr
      class(SequenceNodeIterator), intent(in) :: this
      type(bool_t), intent(in) :: bool
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node_ptr
      integer :: status


      select type (node_ptr)
      type is (BoolNode)
         ptr => to_bool(this%seq_iter%of(), err_msg=err_msg, rc=rc)
         __VERIFY2__(err_msg, status)
      class default
         ptr => null()
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function as_bool

   logical function false(this)
      class(SequenceNodeIterator), intent(in) :: this
      false = .false.
      __UNUSED_DUMMY__(this)
   end function false

   logical function true(this)
      class(SequenceNodeIterator), intent(in) :: this
      true = .true.
      __UNUSED_DUMMY__(this)
   end function true



end submodule SequenceNode_implementation
   
