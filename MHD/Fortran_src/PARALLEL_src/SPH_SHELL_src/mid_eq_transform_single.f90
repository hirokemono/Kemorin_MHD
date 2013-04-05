!>@file   mid_eq_transform_single.f90
!!@brief  module mid_eq_transform_single
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  spherical transform at equatorial plane for single process
!!
!!@n   subroutine deallocate_mid_eq_transform
!!@n   subroutine initialize_mid_eq_transform(ltr, mphi_eq, r_mid)
!
!!@n   subroutine equator_transfer_vector(jmax, mphi_eq,                &
!!@n  &          d_rj_mid_eq, v_rtp_eq_mid)
!!@n   subroutine equator_transfer_scalar(jmax, mphi_eq,                &
!!@n  &          d_rj_mid_eq, v_rtp_eq_mid)
!
      module mid_eq_transform_single
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_FFT_selector
!
      implicit none
!
!>      Truncation level for spherical transform at equator
      integer(kind = kint) :: ltr_eq
!>      Number of modes for spherical transform at equator
      integer(kind = kint) :: jmax_eq
!>      end address of SMP parallelization for scalar Fourier transform
      integer(kind = kint), allocatable :: istack_eqfft_smp(:)
!
!>      @f$ 1/ r @f$ at mid-depth of shell
      real(kind = kreal) :: ar_mid
!>      @f$ 1/ r^{2} @f$ at mid-depth of shell
      real(kind = kreal) :: ar_mid2
!
!>      associated Lagender polynomial at equator
      real(kind = kreal), allocatable :: P_eq(:)
!>       @f$ dP_{l}^{m}/ d\theta @f$ at equator
      real(kind = kreal), allocatable :: dPdt_eq(:)
!
!>      spectr data for Fourier transform at mid-depth equator
      real(kind = kreal), allocatable :: veq_rtm(:,:)
!>      Work area for Fourier transform at mid-depth equator
      real(kind = kreal), allocatable :: veq_fft(:)
!
!>      Working structure for Fourier transform at mid-depth equator
!!@n      (Save attribute is necessary for Hitachi compiler for SR16000)
      type(working_FFTs), save :: WK_mid_eq_fft
!
      private :: ltr_eq, jmax_eq
      private :: istack_eqfft_smp
      private :: ar_mid, ar_mid2
      private :: P_eq, dPdt_eq, veq_fft
      private :: allocate_mid_eq_transform
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_mid_eq_transform(ltr, mphi_eq)
!
      integer(kind = kint), intent(in) :: ltr, mphi_eq
!
!
      ltr_eq = ltr
      jmax_eq = ltr_eq*(ltr_eq+2)
!
      allocate(P_eq(0:jmax_eq))
      allocate(dPdt_eq(0:jmax_eq))
      P_eq = zero
      dPdt_eq = zero
!
      allocate( veq_rtm(3,-ltr_eq:ltr_eq) )
      allocate( veq_fft(3*mphi_eq) )
      veq_rtm = zero
      veq_fft = zero
!
      allocate(istack_eqfft_smp(0:np_smp))
      istack_eqfft_smp(1:np_smp) = 1
      istack_eqfft_smp(0) =        0
!
      end subroutine allocate_mid_eq_transform
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_mid_eq_transform
!
!
      deallocate(P_eq, dPdt_eq)
      deallocate(veq_rtm, veq_fft)
      deallocate(istack_eqfft_smp)
!
      end subroutine deallocate_mid_eq_transform
!
! ----------------------------------------------------------------------
!
      subroutine initialize_mid_eq_transform(ltr, mphi_eq, r_mid)
!
      use m_parallel_var_dof
      use m_schmidt_polynomial
!
      integer(kind = kint), intent(in) :: ltr, mphi_eq
      real(kind = kreal), intent(in) :: r_mid
!
      integer(kind = kint) :: l, m, mm, j
!
!
      call allocate_mid_eq_transform(ltr, mphi_eq)
!
      ar_mid = one / r_mid
      ar_mid2 = ar_mid*ar_mid
!
      nth = ltr_eq
      call allocate_schmidt_polynomial
!
      dth = two * atan(one)
      call dschmidt
!
      do l = 1, ltr_eq
        do m = -l, l
          j = l*(l+1) + m
          mm = abs(m)
          P_eq(j) =     p(mm,l)
          dPdt_eq(j) = dp(mm,l)
        end do
      end do
!
      call deallocate_schmidt_polynomial
!
      if(my_rank .gt. 0) return
!
      call initialize_FFT_sel_t(np_smp, istack_eqfft_smp, mphi_eq,      &
     &    WK_mid_eq_fft)
!
      end subroutine initialize_mid_eq_transform
!
! ----------------------------------------------------------------------
!
      subroutine equator_transfer_vector(jmax, mphi_eq,                 &
     &          d_rj_mid_eq, v_rtp_eq_mid)
!
      use FFT_selector
!
      integer(kind = kint), intent(in) :: jmax, mphi_eq
      real(kind = kreal), intent(in) :: d_rj_mid_eq(0:jmax,3)
      real(kind = kreal), intent(inout) :: v_rtp_eq_mid(mphi_eq,3)
!
      integer(kind = kint) :: l, m, j
!
!
      veq_rtm = 0.0d0
      do l = 1, ltr_eq
        do m = -l, l
          j = l*(l+1) + m
          veq_rtm(1,m) = veq_rtm(1,m) + d_rj_mid_eq(j,1) * P_eq(j)      &
     &                               * dble(l)*dble(l+1)
          veq_rtm(2,m) = veq_rtm(2,m) + d_rj_mid_eq(j,2) * dPdt_eq(j)
          veq_rtm(3,m) = veq_rtm(3,m) - d_rj_mid_eq(j,3) * dPdt_eq(j)
!
!
          veq_rtm(2,-m) = veq_rtm(2,-m) + d_rj_mid_eq(j,3) * P_eq(j)    &
     &                  * dble(-m)
!
          veq_rtm(3,-m) = veq_rtm(3,-m) + d_rj_mid_eq(j,2) * P_eq(j)    &
     &                  * dble(-m)
        end do
      end do
!
      veq_fft = 0.0d0
      veq_fft(1          ) = veq_rtm(1,0)
      veq_fft(1+  mphi_eq) = veq_rtm(2,0)
      veq_fft(1+2*mphi_eq) = veq_rtm(3,0)
      do m = 1, ltr_eq-1
        veq_fft(2*m+1          ) = veq_rtm(1 ,m)
        veq_fft(2*m+2          ) = veq_rtm(1,-m)
        veq_fft(2*m+1+  mphi_eq) = veq_rtm(2 ,m)
        veq_fft(2*m+2+  mphi_eq) = veq_rtm(2,-m)
        veq_fft(2*m+1+2*mphi_eq) = veq_rtm(3, m)
        veq_fft(2*m+2+2*mphi_eq) = veq_rtm(3,-m)
      end do
      if(ltr_eq .eq. (mphi_eq/2)) then
        veq_fft(2          ) = veq_rtm(1,ltr_eq)
        veq_fft(2+  mphi_eq) = veq_rtm(2,ltr_eq)
        veq_fft(2+2*mphi_eq) = veq_rtm(3,ltr_eq)
      else
        veq_fft(2*ltr_eq+1          ) = veq_rtm(1, ltr_eq)
        veq_fft(2*ltr_eq+2          ) = veq_rtm(1,-ltr_eq)
        veq_fft(2*ltr_eq+1+  mphi_eq) = veq_rtm(2, ltr_eq)
        veq_fft(2*ltr_eq+2+  mphi_eq) = veq_rtm(2,-ltr_eq)
        veq_fft(2*ltr_eq+1+2*mphi_eq) = veq_rtm(3, ltr_eq)
        veq_fft(2*ltr_eq+2+2*mphi_eq) = veq_rtm(3,-ltr_eq)
      end if
!
      call backward_FFT_sel_t(np_smp, istack_eqfft_smp, ione,           &
     &    mphi_eq, veq_fft(1          ), WK_mid_eq_fft)
      call backward_FFT_sel_t(np_smp, istack_eqfft_smp, ione,           &
     &    mphi_eq, veq_fft(1+  mphi_eq), WK_mid_eq_fft)
      call backward_FFT_sel_t(np_smp, istack_eqfft_smp, ione,           &
     &    mphi_eq, veq_fft(1+2*mphi_eq), WK_mid_eq_fft)
!
      do m = 1, mphi_eq
        v_rtp_eq_mid(m,1) = veq_fft(m          ) * ar_mid2
        v_rtp_eq_mid(m,2) = veq_fft(m+  mphi_eq) * ar_mid
        v_rtp_eq_mid(m,3) = veq_fft(m+2*mphi_eq) * ar_mid
      end do
!
      end subroutine equator_transfer_vector
!
! ----------------------------------------------------------------------
!
      subroutine equator_transfer_scalar(jmax, mphi_eq,                 &
     &          d_rj_mid_eq, v_rtp_eq_mid)
!
      use FFT_selector
!
      integer(kind = kint), intent(in) :: jmax, mphi_eq
      real(kind = kreal), intent(in) :: d_rj_mid_eq(0:jmax)
      real(kind = kreal), intent(inout) :: v_rtp_eq_mid(mphi_eq)
!
      integer(kind = kint) :: l, m, j
!
!
      veq_rtm = 0.0d0
      veq_rtm(1,0) = d_rj_mid_eq(0)
      do l = 1, ltr_eq
        do m = -l, l
          j = l*(l+1) + m
          veq_rtm(1,m) = veq_rtm(1,m) + d_rj_mid_eq(j) * P_eq(j)
        end do
      end do
!
      veq_fft = 0.0d0
      veq_fft(1) = veq_rtm(1,0)
      do m = 1, ltr_eq-1
        veq_fft(2*m+1) = veq_rtm(1,m)
        veq_fft(2*m+2) = veq_rtm(1,-m)
      end do
      if(ltr_eq .eq. (mphi_eq/2)) then
        veq_fft(2) = veq_rtm(1,ltr_eq)
      else
        veq_fft(2*ltr_eq+1) = veq_rtm(1,ltr_eq)
        veq_fft(2*ltr_eq+2) = veq_rtm(1,-ltr_eq)
      end if
!
      call backward_FFT_sel_t(np_smp, istack_eqfft_smp, ione,           &
     &    mphi_eq, veq_fft, WK_mid_eq_fft)
!
      do m = 1, mphi_eq
        v_rtp_eq_mid(m) = veq_fft(m)
      end do
!
      end subroutine equator_transfer_scalar
!
! ----------------------------------------------------------------------
!
     end module mid_eq_transform_single
