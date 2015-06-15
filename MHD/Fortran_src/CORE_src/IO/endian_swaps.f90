!>@file  endian_swaps.f90
!!       module endian_swaps
!!
!!@author H. Matsui
!!@date   Programmed in June, 2015
!
!> @brief Swap endian
!!
!!@verbatim
!!      subroutine endian_swap_real(num_real, real_data)
!!      subroutine endian_swap_int(num_int, int_data)
!!      subroutine endian_swap_int_gl(num_int, int_data)
!!
!!      subroutine endian_swap_real4(num_real4, real4_data)
!!      subroutine endian_swap_real8(num_real8, real8_data)
!!      subroutine endian_swap_int4(num_int4, int4_data)
!!      subroutine endian_swap_int8(num_int8, int8_data)
!!@endverbatim
!
      module endian_swaps
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine endian_swap_real(num_real, real_data)
!
      integer(kind = kint), intent(inout) :: num_real
      real(kind = 8), intent(inout) :: real_data(num_real)
!
!
!      if(kreal .eq. 8) then
        call endian_swap_real8(num_real, real_data)
!      else if(kreal .eq. 4) then
!        call endian_swap_real4(num_real, real_data)
!      end if
!
      end subroutine endian_swap_real
!
!  ---------------------------------------------------------------------
!
      subroutine endian_swap_int(num_int, int_data)
!
      integer(kind = kint), intent(inout) :: num_int
      integer(kind = kint), intent(inout) :: int_data(num_int)
!
!
!      if(kint .eq. 8) then
!        call endian_swap_int8(num_int, int_data)
!      else if(kint .eq. 4) then
        call endian_swap_int4(num_int, int_data)
!      end if
!
      end subroutine endian_swap_int
!
!  ---------------------------------------------------------------------
!
      subroutine endian_swap_int_gl(num_int, int_data)
!
      integer(kind = kint), intent(inout) :: num_int
      integer(kind = kint_gl), intent(inout) :: int_data(num_int)
!
!
!      if(kint_gl .eq. 8) then
        call endian_swap_int8(num_int, int_data)
!      else if(kint_gl .eq. 4) then
!        call endian_swap_int4(num_int, int_data)
!      end if
!
      end subroutine endian_swap_int_gl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine endian_swap_real4(num_real4, real4_data)
!
      integer(kind = kint), intent(inout) :: num_real4
      real(kind = 4), intent(inout) :: real4_data(num_real4)
!
      integer(kind = kint)  :: i
      character(len=1) :: tmpchara1,tmpchara2
      character(len=4) :: real4chara
      real(kind = 4) ::r4tmp
      equivalence(r4tmp,real4chara)
!
!
!$omp parallel do private(i,r4tmp,real4chara,tmpchara1,tmpchara2)
      do i = 1, num_real4
        r4tmp = real4_data(i)
        tmpchara1 = real4chara(1:1)
        tmpchara2 = real4chara(2:2)
        real4chara(1:1) = real4chara(4:4)
        real4chara(2:2) = real4chara(3:3)
        real4chara(3:3) = tmpchara2
        real4chara(4:4) = tmpchara1
        real4_data(i) = r4tmp
      end do
!$omp end parallel do
!
      end subroutine endian_swap_real4
!
! -----------------------------------------------------------------------
!
      subroutine endian_swap_real8(num_real8, real8_data)
!
      integer(kind = kint), intent(inout) :: num_real8
      real(kind = 8), intent(inout) :: real8_data(num_real8)
!
      integer(kind = kint)  :: i
      character(len=1) :: tmpchara1,tmpchara2
      character(len=8) :: real8chara
      real(kind = 8) ::r8tmp
      equivalence(r8tmp,real8chara)
!
!
!$omp parallel do private(i,r8tmp,real8chara,tmpchara1,tmpchara2)
      do i = 1, 2*num_real8
        r8tmp = real8_data(i)
        tmpchara1 = real8chara(1:1)
        tmpchara2 = real8chara(2:2)
        real8chara(1:1) = real8chara(4:4)
        real8chara(2:2) = real8chara(3:3)
        real8chara(3:3) = tmpchara2
        real8chara(4:4) = tmpchara1
        tmpchara1 = real8chara(5:5)
        tmpchara2 = real8chara(6:6)
        real8chara(5:5) = real8chara(8:8)
        real8chara(6:6) = real8chara(7:7)
        real8chara(7:7) = tmpchara2
        real8chara(8:8) = tmpchara1
        real8_data(i) = r8tmp
      end do
!$omp end parallel do
!
      end subroutine endian_swap_real8
!
! -----------------------------------------------------------------------
!
      subroutine endian_swap_int4(num_int4, int4_data)
!
      integer(kind = kint), intent(inout) :: num_int4
      integer(kind = 4), intent(inout) :: int4_data(num_int4)
!
      integer(kind = kint)  :: i
      character(len=1) :: tmpchara1,tmpchara2
      character(len=4) :: int4chara
      integer(kind = 4) :: i4tmp
      equivalence(i4tmp,int4chara)
!
!
!$omp parallel do private(i,i4tmp,int4chara,tmpchara1,tmpchara2)
      do i = 1, num_int4
        i4tmp = int4_data(i)
        tmpchara1 = int4chara(1:1)
        tmpchara2 = int4chara(2:2)
        int4chara(1:1) = int4chara(4:4)
        int4chara(2:2) = int4chara(3:3)
        int4chara(3:3) = tmpchara2
        int4chara(4:4) = tmpchara1
        int4_data(i) = i4tmp
      end do
!$omp end parallel do
!
      end subroutine endian_swap_int4
!
! -----------------------------------------------------------------------
!
      subroutine endian_swap_int8(num_int8, int8_data)
!
      integer(kind = kint), intent(inout) :: num_int8
      integer(kind = 8), intent(inout) :: int8_data(num_int8)
!
      integer(kind = kint)  :: i
      character(len=1) :: tmpchara1,tmpchara2
      character(len=8) :: int8chara
      integer(kind = 8) :: i8tmp
      equivalence(i8tmp,int8chara)
!
!
!$omp parallel do private(i,i8tmp,int8chara,tmpchara1,tmpchara2)
      do i = 1, 2*num_int8
        i8tmp = int8_data(i)
        tmpchara1 = int8chara(1:1)
        tmpchara2 = int8chara(2:2)
        int8chara(1:1) = int8chara(4:4)
        int8chara(2:2) = int8chara(3:3)
        int8chara(3:3) = tmpchara2
        int8chara(4:4) = tmpchara1
        tmpchara1 = int8chara(5:5)
        tmpchara2 = int8chara(6:6)
        int8chara(5:5) = int8chara(8:8)
        int8chara(6:6) = int8chara(7:7)
        int8chara(7:7) = tmpchara2
        int8chara(8:8) = tmpchara1
        int8_data(i) = i8tmp
      end do
!$omp end parallel do
!
      end subroutine endian_swap_int8
!
! -----------------------------------------------------------------------
!
      end module endian_swaps
       