module fy_Sequence
  use fy_AbstractNode

#define T AbstractNode
#define T_polymorphic
#define Vector Sequence
#define VectorIterator SequenceIterator

#include "vector/template.inc"

#undef Vector
#undef VectorIterator
#undef T_polymorphic
#undef T
end module fy_Sequence
