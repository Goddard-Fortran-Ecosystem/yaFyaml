module fy_Mapping
   use fy_YAML_Node

#define DEBUG_INSERT

#define Key YAML_Node
#define Key_polymorphic
#define Key_LT(lhs,rhs) lhs < rhs
#define Key_EQ(lhs,rhs) .not.(lhs<rhs .or. rhs<lhs)
#define T YAML_Node
#define T_polymorphic


#define OrderedMap Mapping
#define OrderedMapIterator MappingIterator
#define Pair MappingPair

#define Map Mapping
#define MapIterator MappingIterator
!#define __PAIR_COPY(a,b) Key_COPY(a%first,b%first); T_COPY(a%second,b%second)

! Toggle below to use ordinary gFTL map instead of ordered_map container.
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
