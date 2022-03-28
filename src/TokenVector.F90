module fy_TokenVector
   use fy_Tokens

#define T AbstractToken
#define T_polymorphic
#define Vector TokenVector
#define VectorIterator TokenVectorIterator

#include "vector/template.inc"

end module fy_TokenVector
