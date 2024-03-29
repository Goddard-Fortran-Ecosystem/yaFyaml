!---------
! This module contains interfaces to manage tracebacks durig
! unexpected conditions.  Users can override the behavior by invoking
! set_throw_method() with a procedure of their own.
!---------

#include "error_handling.h"
#include "string_handling.h"
module fy_ErrorHandling
   use fy_KeywordEnforcer
   use fy_ErrorCodes
   implicit none
   private

   public :: throw
   public :: assert
   public :: internal_verify
   public :: return_rc
   public :: set_throw_method


   abstract interface
      subroutine throw_interface(filename, line_number, message)
         character(len=*), intent(in) :: filename
         integer, intent(in) :: line_number
         character(len=*), intent(in) :: message
      end subroutine throw_interface
   end interface

   procedure(throw_interface), pointer :: throw_method => null()
   logical, save :: initialized = .false.

   interface assert
      module procedure assert_message
      module procedure assert_code
   end interface assert

   ! GFortran 9.2 complains that "throw" cannot be generic
   ! when attempting to use this module in conjunction with pFUnit.
   ! Might be right, but NAG is not complaining.  
   interface throw
      module procedure throw_
   end interface throw

contains


   subroutine throw_(filename, line_number, message)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: line_number
      character(len=*), intent(in) :: message

      if (.not. initialized) call initialize()
      call throw_method(filename, line_number, message)

   end subroutine throw_


   logical function assert_message(condition, message, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      character(*), intent(in) :: message
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN

      fail = .not. condition

      if (fail) then
         call throw(filename, line, message)
         if (present(rc)) rc = -1
      end if

   end function assert_message


   logical function assert_code(condition, code, filename, line, unusable, err_msg, rc) result(fail)
      logical, intent(in) :: condition
      integer, intent(in) :: code
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc ! Not present in MAIN

      fail = (.not. condition)

      if (fail) then
         call throw(filename, line, error_message(code))
         if (present(err_msg)) err_msg = error_message(code)
         if (present(rc)) rc = code
      end if

      __UNUSED_DUMMY__(UNUSABLE)

   end function assert_code


   logical function internal_verify(status, filename, line, unusable, err_msg, rc) result(fail)
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc ! Not present in MAIN

      logical :: condition
      character(:), allocatable :: message
      character(16) :: status_string

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         write(status_string,'(i0)') status
         message = 'status=' // status_string
         call throw(filename, line, message)
         if (present(err_msg)) err_msg = error_message(status)
         if (present(rc)) rc = status
      end if

      __UNUSED_DUMMY__(unusable)
   end function internal_verify


   subroutine return_rc(status, filename, line, rc) 
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, intent(out), optional :: rc

      logical :: condition, fail
      character(:), allocatable :: message
      character(8) :: status_string

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         write(status_string,'(i0)') status
         message = 'status=' // status_string
         call throw(filename, line, message)
      end if
      ! Regardless of error:
      if (present(rc)) rc = status 

   end subroutine return_rc



   ! Ensure that there is a default throw method
   subroutine initialize()
      throw_method => default_throw_method
      initialized = .true.
   end subroutine initialize


   subroutine default_throw_method(filename, line_number, message)
      use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
      character(len=*), intent(in) :: filename
      integer, intent(in) :: line_number
      character(len=*), intent(in) :: message

      integer, parameter :: FIELD_WIDTH=40
      character(FIELD_WIDTH) :: use_name
      character(3) :: prefix
      character(:), allocatable :: base_name

      !-----------
      ! Force file names to be <= FIELD_WIDTH for legibility
      !-----------
      base_name = get_base_name(filename)
      if (len(base_name) > FIELD_WIDTH) then
         prefix = '...'
         use_name = base_name(2:)
      else
         prefix = '   '
         use_name = base_name
      end if

      write(ERROR_UNIT,'(a,i5.5,1x,a3,a40,1x,a)') &
           & 'FAIL at line=', line_number, prefix, use_name, &
           & '<'//adjustl(trim(message))//'>'
   end subroutine default_throw_method


   subroutine set_throw_method(method)
      procedure(throw_) :: method

      throw_method => method
      initialized = .true.

   end subroutine set_throw_method

   !-----------
   ! Some build systems provide the full path, which makes file name
   ! a potentialy long and difficult to read string.
   ! If a client really wants the full path, it can pass its
   ! own error handler to throw.
   !-----------
   function get_base_name(filename) result(base_name)
      character(:), allocatable :: base_name
      character(*), intent(in) :: filename

      integer :: idx

      idx = scan(filename, '/', back=.true.)
      base_name = filename(idx+1:)

   end function get_base_name


end module fy_ErrorHandling
