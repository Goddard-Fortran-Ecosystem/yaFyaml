module fy_TokenVector
  use fy_Tokens
#define _type class(AbstractToken)
#define _allocatable
#define _vector TokenVector
#define _iterator TokenVectorIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _allocatable
#undef _type
  
end module fy_TokenVector
