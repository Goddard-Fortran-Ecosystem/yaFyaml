! Adopted from style in GEOS

#define __return__ return

#define ___rc___(rc) ,rc
#define __rc__(rc,status) rc=status); __VERIFY__(status
#define __RC__ __rc__(rc,status)

! Assumes status is passed back in dummy called "rc"
#define __RETURN__(a)     call return_rc(a,__FILE__,__LINE__ ___rc___(rc)); __return__
#define __VERIFY__(a)     if(verify(a,__FILE__,__LINE__ ___rc___(rc))) __return__

#define __ASSERT_CODE_AND_LOC_AND_RC__(cond,code,file,line,rc)  if(assert(cond,code,file,line ___rc___(rc))) __return__
#define __ASSERT_CODE_AND_LOC__(cond,code,file,line) __ASSERT_CODE_AND_LOC_AND_RC__(cond,code,file,line,rc)
#define __ASSERT__(cond,code) __ASSERT_CODE_AND_LOC__(cond,code,__FILE__,__LINE__)
#define __FAIL__(code) __ASSERT__(.false.,code)

! The following macro is useful in suppressing unused dummy variable warnings.
! Unused dummys are not unusual in OO code, and the warnings are a distraction.
! Use wisely ...
#define __UNUSED_DUMMY__(x) if (.false.) print*,shape(x)
