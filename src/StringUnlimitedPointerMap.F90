module fy_StringUnlimitedPointerMap
  use fy_SimpleKey
  
#include "types/key_deferredLengthString.inc"
#define _value class(*)
#define _map StringUnlimitedPointerMap
#define _pair StringUnlimitedPointerPair
#define _value_pointer
#define _alt
#define _iterator StringUnlimitedPointerMapIterator
#include "templates/map.inc"
#undef _key
#undef _value
  
end module fy_StringUnlimitedPointerMap
