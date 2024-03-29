module fy_StringNodeMap
   use fy_YAML_Node

#define Key __CHARACTER_DEFERRED
#define T YAML_Node
#define T_polymorphic

#define Map StringNodeMap
#define MapIterator StringNodeMapIterator
#define Pair StringNodePair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module fy_StringNodeMap
