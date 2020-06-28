module fy_IntegerSimpleKeyMap
  use fy_SimpleKey
  
#define _key integer
#define _value type(SimpleKey)
#define _map IntegerSimpleKeyMap
#define _pair IntegerSimpleKeyPair
#define _alt
#define _iterator IntegerSimpleKeyMapIterator
#include "templates/map.inc"
#undef _key
#undef _value

end module fy_IntegerSimpleKeyMap
