module fy_IntegerSimpleKeyMap
  use fy_SimpleKey

#define Key __INTEGER
#define T SimpleKey
#define Map IntegerSimpleKeyMap
#define MapIterator IntegerSimpleKeyMapIterator
#define Pair IntegerSimpleKeyPair

#include "map/template.inc"

end module fy_IntegerSimpleKeyMap
