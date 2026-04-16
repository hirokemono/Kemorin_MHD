!>@file   trim_sph_field_to_FFT.f90
!!@brief  module trim_sph_field_to_FFT
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief Dara trasfer between commucation buffer and complex FFT array
!!
!!@verbatim
!!      subroutine prt_field_to_pin_FFT(ncomp_fwd, nnod_rt,             &
!!     &                                mphi, X_prt, Nfft_r, X_fft)
!!        integer(kind = kint), intent(in) :: ncomp_fwd, nnod_rt
!!        integer(kind = kint), intent(in) :: mphi, Nfft_r
!!        real(kind = kreal), intent(in)                                &
!!     &                 :: X_prt(mphi,nnod_rt,ncomp_fwd)
!!        real(kind = kreal), intent(inout)                             &
!!     &                 :: X_fft(Nfft_r,nnod_rt,ncomp_fwd)
!!      subroutine prt_field_from_pin_FFT                               &
!!     &         (ncomp_bwd, nnod_rt, Nfft_r, X_fft, mphi, X_prt)
!!        real(kind=kreal), intent(inout)                               &
!!        integer(kind = kint), intent(in) :: ncomp_bwd, nnod_rt
!!        integer(kind = kint), intent(in) :: mphi, Nfft_r
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: X_fft(Nfft_r,nnod_rt,ncomp_bwd)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_prt(mphi,nnod_rt,ncomp_bwd)
!!
!!      subroutine rtp_field_to_pout_FFT(ncomp_fwd, nnod_rt,            &
!!     &                                 mphi, X_rtp, Nfft_r, X_fft)
!!        integer(kind = kint), intent(in) :: ncomp_fwd, nnod_rt
!!        integer(kind = kint), intent(in) :: mphi, Nfft_r
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: X_rtp(nnod_rt,mphi,ncomp_fwd)
!!      real(kind = kreal), intent(inout)                               &
!!     &                   :: X_fft(ncomp_fwd,nnod_rt,Nfft_r)
!!      subroutine rtp_field_from_pout_FFT                              &
!!     &         (ncomp_bwd, nnod_rt, Nfft_r, X_fft, mphi, X_rtp)
!!        integer(kind = kint), intent(in) :: ncomp_bwd, nnod_rt
!!        integer(kind = kint), intent(in) :: mphi, Nfft_r
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: X_fft(ncomp_bwd,nnod_rt,Nfft_r)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_rtp(nnod_rt,mphi,ncomp_bwd)
!!@endverbatim
!!
      module trim_sph_field_to_FFT
!
      use m_precision
      use m_machine_parameter
!
      use t_sph_comm_table_from_FFTW
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine prt_field_to_pin_FFT(ncomp_fwd, nnod_rt,               &
     &                                mphi, X_prt, Nfft_r, X_fft)
!
      integer(kind = kint), intent(in) :: ncomp_fwd, nnod_rt
      integer(kind = kint), intent(in) :: mphi, Nfft_r
      real(kind = kreal), intent(in)                                    &
     &                 :: X_prt(mphi,nnod_rt,ncomp_fwd)
      real(kind = kreal), intent(inout)                                 &
     &                 :: X_fft(Nfft_r,nnod_rt,ncomp_fwd)
!
      integer(kind = kint) :: kl, nd
!
!$omp parallel do private(kl,nd) collapse(2)
      do nd = 1, ncomp_fwd
        do kl = 1, nnod_rt
          X_fft(1:mphi,kl,nd) = X_prt(1:mphi,kl,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine prt_field_to_pin_FFT
!
! -----------------------------------------------------------------------
!
      subroutine prt_field_from_pin_FFT(ncomp_bwd, nnod_rt,             &
     &                                  Nfft_r, X_fft, mphi, X_prt)
!
      integer(kind = kint), intent(in) :: ncomp_bwd, nnod_rt
      integer(kind = kint), intent(in) :: mphi, Nfft_r
      real(kind=kreal), intent(in)                                      &
     &                 :: X_fft(Nfft_r,nnod_rt,ncomp_bwd)
!
      real(kind=kreal), intent(inout)                                   &
     &                 :: X_prt(mphi,nnod_rt,ncomp_bwd)
!
      integer(kind = kint) :: kl, nd
!
!$omp parallel do private(kl,nd) collapse(2)
      do nd = 1, ncomp_bwd
        do kl = 1, nnod_rt
          X_prt(1:mphi,kl,nd) = X_fft(1:mphi,kl,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine prt_field_from_pin_FFT
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine rtp_field_to_pout_FFT(ncomp_fwd, nnod_rt,              &
     &                                 mphi, X_rtp, Nfft_r, X_fft)
!
      integer(kind = kint), intent(in) :: ncomp_fwd, nnod_rt
      integer(kind = kint), intent(in) :: mphi, Nfft_r
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(nnod_rt,mphi,ncomp_fwd)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_fft(ncomp_fwd,nnod_rt,Nfft_r)
!
      integer(kind = kint) :: m, kl
!
!
!$omp parallel do private(m,kl)
      do m = 1, mphi
        do kl = 1, nnod_rt
          X_fft(1:ncomp_fwd,kl,m) = X_rtp(kl,m,1:ncomp_fwd)
        end do
      end do
!$omp end parallel do
!
      end subroutine rtp_field_to_pout_FFT
!
! -----------------------------------------------------------------------
!
      subroutine rtp_field_from_pout_FFT                                &
     &         (ncomp_bwd, nnod_rt, Nfft_r, X_fft, mphi, X_rtp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd, nnod_rt
      integer(kind = kint), intent(in) :: mphi, Nfft_r
      real(kind = kreal), intent(in)                                    &
     &                   :: X_fft(ncomp_bwd,nnod_rt,Nfft_r)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_rtp(nnod_rt,mphi,ncomp_bwd)
!
      integer(kind = kint) :: m, nd
!
!$omp parallel
      do nd = 1, ncomp_bwd
!$omp do private(m)
        do m = 1, mphi
          X_rtp(1:nnod_rt,m,nd) = X_fft(nd,1:nnod_rt,m)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine rtp_field_from_pout_FFT
!
! -----------------------------------------------------------------------
!
      end module trim_sph_field_to_FFT
