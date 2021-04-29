!#ifndef __GFORTRAN__
!#   define STRING_DUMMY character(:), allocatable
!#else
#   define STRING_DUMMY character(*)
!#endif
