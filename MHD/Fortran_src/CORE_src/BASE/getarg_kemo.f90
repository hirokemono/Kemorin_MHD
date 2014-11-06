!>@file   getarg_kemo.f90
!!@brief  module getarg_kemo
!!
!!@author H. Matsui
!!@date Programmed in July, 2010
!
!>@brief  Subroutines to get text from command line
!!
!!@verbatim
!!      subroutine getarg_k(i, argc)
!!      integer function iargc_kemo() result(oresult)
!!@endverbatim
!
      module getarg_kemo
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine getarg_k(i, argc)
!
      integer(kind = 4), intent(in) :: i
      character(len=*), intent(out) :: argc
!
      call getarg(0, argc)
      if(argc == "") then
        call getarg(i+1, argc)
      else
        call getarg(i, argc)
      end if
      end subroutine getarg_k
!
!   --------------------------------------------------------------------
!
      integer function iargc_kemo() result(oresult)
!
      integer(kind = kint) :: iargc
      character(len=8) :: argc
      oresult = iargc()
      call getarg(0, argc)
      if(argc == "") then
        oresult = oresult - 1
      end if
      end function iargc_kemo
!
!   --------------------------------------------------------------------
!
      end module getarg_kemo
