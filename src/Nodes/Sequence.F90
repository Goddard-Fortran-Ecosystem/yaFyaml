module fy_Sequence
  use fy_YAML_Node

#define T YAML_Node
#define T_polymorphic
#define T_Free(x) call x%clear()
#define T_Copy(x,y) call y%clone(x)
#define Vector Sequence
#define VectorIterator SequenceIterator

#include "vector/template.inc"

#undef Vector
#undef VectorIterator
#undef T_polymorphic
#undef T
end module fy_Sequence
