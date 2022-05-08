module fy_Sequence
   use fy_YAML_Node

#define T YAML_Node
#define T_polymorphic
!#define T_COPY(x,y) x=y
#define Vector Sequence
#define VectorIterator SequenceIterator

#include "vector/template.inc"

#undef Vector
#undef VectorIterator
#undef T_polymorphic
#undef T
end module fy_Sequence
