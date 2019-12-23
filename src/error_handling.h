! Adopted from style in GEOS

#define __return__ return

#define ___rc___(rc) ,rc
#define __rc__(rc,status) rc=status); __VERIFY__(status
#define __RC__ __rc__(rc,status)

! Assumes status is passed back in dummy called "rc"
#define __RETURN__(a)     call return_rc(a,__FILE__,__LINE__ ___rc___(rc)); __return__
#define __VERIFY__(a)     if(verify(a,__FILE__,__LINE__ ___rc___(rc))) __return__

#define __ASSERT_MSG_AND_LOC_AND_RC__(msg,cond,file,line,rc)  if(assert(msg,cond,file,line ___rc___(rc))) __return__
#define __ASSERT_MSG_AND_LOC__(msg,cond,file,line) __ASSERT_MSG_AND_LOC_AND_RC__(msg,cond,file,line,rc)
#define __ASSERT__(msg,cond) __ASSERT_MSG_AND_LOC__(msg,cond,__FILE__,__LINE__)
#define __ASSERT_NOMSG__(cond) __ASSERT__('needs informative message',cond)
#define __FAIL__(msg) __ASSERT__(msg,.false.)

! The following macro is useful in suppressing unused dummy variable warnings.
! Unused dummys are not unusual in OO code, and the warnings are a distraction.
! Use wisely ...
#    define _UNUSED_DUMMY(x) if (.false.) print*,shape(x)
