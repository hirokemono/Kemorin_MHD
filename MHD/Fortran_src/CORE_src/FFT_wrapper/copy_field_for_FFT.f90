!>@file   copy_field_for_FFT.f90
!!@brief  module copy_field_for_FFT
!!
!!@author H. Matsui
!!@date Programmed in April, 2026
!
!>@brief  Real field data copy for FFT
!!
!!@verbatim
!!      subroutine sel_copy_pin_field_to_FFT(Ncomp_r, Nfft, X,          &
!!     &                                     Nfft_r, X_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp_r
!!        integer(kind = kint), intent(in) :: Nfft, Nfft_r
!!        real(kind = kreal), intent(in) :: X(Nfft,Ncomp_r)
!!      subroutine sel_copy_pin_field_from_FFT(Ncomp_r, Nfft_r,         &
!!     &                                       X_FFT, Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp_r
!!        integer(kind = kint), intent(in) :: Nfft_r, Nfft
!!        real(kind = kreal), intent(in) :: X_FFT(Nfft_r,Ncomp_r)
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp_r)
!!
!!      subroutine copy_pout_field_to_FFT(ist_comp, Ncomp, Nfft, X,     &
!!     &                                  Ncomp_r, Nfft_r, X_fft)
!!        integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
!!        real(kind = kreal), intent(inout) :: X_fft(Ncomp_r,Nfft_r)
!!      subroutine copy_pout_field_from_FFT(Ncomp_r, Nfft_r, X_fft,     &
!!     &                                    Ncomp, Nfft, ist_comp, X)
!!        integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
!!        real(kind = kreal), intent(in) :: X_fft(Ncomp_r,Nfft_r)
!!        integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!@endverbatim
!
      module copy_field_for_FFT
!
      use omp_lib
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: copy_pin_fld_to_real_FFT
      private :: copy_pin_fld_from_real_FFT
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_pin_field_to_FFT(Ncomp_r, Nfft, X,            &
     &                                     Nfft_r, X_FFT)
!
      use copy_field_smp
!
      integer(kind = kint), intent(in) :: Ncomp_r
      integer(kind = kint), intent(in) :: Nfft, Nfft_r
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp_r)
!
      real(kind = kreal), intent(inout) :: X_FFT(Nfft_r,Ncomp_r)
!
!
      if(Nfft_r .eq. Nfft) then
        call copy_nod_scalar_smp((Nfft_r*Ncomp_r),                      &
     &                           X(1,1), X_FFT(1,1))
      else
        call copy_pin_fld_to_real_FFT(Ncomp_r, Nfft, X,                 &
     &                                Nfft_r, X_FFT)
      end if
!
      end subroutine sel_copy_pin_field_to_FFT
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_pin_field_from_FFT(Ncomp_r, Nfft_r,           &
     &                                       X_FFT, Nfft, X)
!
      use copy_field_smp
!
      integer(kind = kint), intent(in) :: Ncomp_r
      integer(kind = kint), intent(in) :: Nfft_r, Nfft
      real(kind = kreal), intent(in) :: X_FFT(Nfft_r,Ncomp_r)
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp_r)
!
!
      if(Nfft_r .eq. Nfft) then
        call copy_nod_scalar_smp((Nfft_r*Ncomp_r),                      &
     &                           X_FFT(1,1), X(1,1))
      else
        call copy_pin_fld_from_real_FFT(Ncomp_r, Nfft_r,                &
     &                                  X_FFT, Nfft, X)
      end if
!
      end subroutine sel_copy_pin_field_from_FFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_pout_field_to_FFT(ist_comp, Ncomp, Nfft, X,       &
     &                                  Ncomp_r, Nfft_r, X_fft)
!
      integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
      real(kind = kreal), intent(inout) :: X_fft(Ncomp_r,Nfft_r)
!
      integer(kind = kint) :: i
!
!
!$omp parallel do private(i)
      do i = 1, Nfft
        X_fft(1:Ncomp_r,i) = X(ist_comp:ist_comp+Ncomp_r-1,i)
      end do
!$omp end parallel do
!
      if(Nfft .ge. Nfft_r) return
!$omp parallel do private(i)
      do i = Nfft+1, Nfft_r
        X_fft(1:Ncomp_r,i) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine copy_pout_field_to_FFT
!
! ------------------------------------------------------------------
!
      subroutine copy_pout_field_from_FFT(Ncomp_r, Nfft_r, X_fft,       &
     &                                    Ncomp, Nfft, ist_comp, X)
!
      integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
      real(kind = kreal), intent(in) :: X_fft(Ncomp_r,Nfft_r)
!
      integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i
!
!$omp parallel do private(i)
      do i = 1, Nfft
        X(ist_comp:ist_comp+Ncomp_r-1,i) = X_fft(1:Ncomp_r,i)
      end do
!$omp end parallel do
!
      end subroutine copy_pout_field_from_FFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_pin_fld_to_real_FFT(Ncomp_r, Nfft, X,             &
     &                                    Nfft_r, X_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp_r
      integer(kind = kint), intent(in) :: Nfft, Nfft_r
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp_r)
!
      real(kind = kreal), intent(inout) :: X_FFT(Nfft_r,Ncomp_r)
!
      integer(kind = kint) :: nd, i
!
!
!$omp parallel do private(nd,i)
        do nd = 1, Ncomp_r
          do i = 1, Nfft
            X_FFT(i,nd) = X(i,nd)
          end do
          do i = Nfft+1, Nfft_r
            X_FFT(i,nd) = zero
          end do
        end do
!$omp end parallel do
!
      end subroutine copy_pin_fld_to_real_FFT
!
! ------------------------------------------------------------------
!
      subroutine copy_pin_fld_from_real_FFT(Ncomp_r, Nfft_r,            &
     &                                      X_FFT, Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp_r
      integer(kind = kint), intent(in) :: Nfft_r, Nfft
      real(kind = kreal), intent(in) :: X_FFT(Nfft_r,Ncomp_r)
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp_r)
!
      integer(kind = kint) :: nd, i
!
!
!$omp parallel do collapse(2) private(nd,i)
      do nd = 1, Ncomp_r
        do i = 1, Nfft
          X(i,nd) = X_FFT(i,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_pin_fld_from_real_FFT
!
! ------------------------------------------------------------------
!
      end module copy_field_for_FFT
