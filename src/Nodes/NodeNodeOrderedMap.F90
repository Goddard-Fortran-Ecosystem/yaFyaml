module fy_NodeNodeOrderedMap
   use fy_AbstractNode

#define Key AbstractNode
#define Key_polymorphic
#define Key_LT(lhs,rhs) lhs < rhs
#define Key_EQ(lhs,rhs) .not.(lhs<rhs .or. rhs<lhs)
#define T AbstractNode
#define T_polymorphic

#define OrderedMap NodeNodeOrderedMap
#define OrderedMapIterator NodeNodeOrderedOrderedMapIterator
#define Pair NodeNodePair

#include "ordered_map/template.inc"

#undef Pair
#undef OrderedMapIterator
#undef OrderedMap
#undef T_polymorphic
#undef T
#undef Key_EQ
#undef Key_LT
#undef Key_polymorphic
#undef Key

end module fy_NodeNodeOrderedMap
