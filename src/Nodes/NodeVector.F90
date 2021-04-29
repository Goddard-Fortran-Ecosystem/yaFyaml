module fy_NodeVector
  use fy_AbstractNode

#define T AbstractNode
#define T_polymorphic
#define Vector NodeVector
#define VectorIterator NodeVectorIterator

#include "vector/template.inc"

#undef Vector
#undef VectorIterator
#undef T_polymorphic
#undef T
end module fy_NodeVector
