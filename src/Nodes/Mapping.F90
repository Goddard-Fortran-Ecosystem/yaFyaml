module fy_Mapping
   use fy_AbstractNode

#define Key AbstractNode
#define Key_polymorphic
#define Key_LT(lhs,rhs) lhs < rhs
#define Key_EQ(lhs,rhs) .not.(lhs<rhs .or. rhs<lhs)
#define T AbstractNode
#define T_polymorphic

#define OrderedMap Mapping
#define OrderedMapIterator MappingIterator
#define Pair MappingPair

#define Map Mapping
#define MapIterator MappingIterator

#include "ordered_map/template.inc"
!!$#include "map/template.inc"

#undef Pair
#undef OrderedMapIterator
#undef OrderedMap
#undef T_polymorphic
#undef T
#undef Key_EQ
#undef Key_LT
#undef Key_polymorphic
#undef Key

end module fy_Mapping
