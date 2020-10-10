!>@file   copy_rtp_data_to_ISPACK.f90
!!@brief  module copy_rtp_data_to_ISPACK
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTPACK5
!!
!!@verbatim
!!      subroutine copy_ISPACK_to_prt_comp(nnod_rtp, nidx_rtp,          &
!!     &          irt_rtp_smp_stack, X_FFT, X_rtp)
!!      subroutine copy_ISPACK_to_prt_field(nnod_rtp, nidx_rtp,         &
!!     &          irt_rtp_smp_stack, ncomp_bwd, X_FFT, X_rtp)
!!
!!      subroutine copy_ISPACK_from_prt_comp(nnod_rtp, nidx_rtp,        &
!!     &          irt_rtp_smp_stack, X_rtp, X_FFT)
!!      subroutine copy_ISPACK_from_prt_field(nnod_rtp, nidx_rtp,       &
!!     &          irt_rtp_smp_stack, ncomp_fwd, X_rtp, X_FFT)
!!@endverbatim
!!
      module copy_rtp_data_to_ISPACK
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine copy_ISPACK_to_prt_comp(nnod_rtp, nidx_rtp,            &
     &          irt_rtp_smp_stack, X_FFT, X_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in) :: X_FFT(nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(nidx_rtp(3),irt_rtp_smp_stack(np_smp))
!
!
      integer(kind = kint) :: m, j, ip, ist, num
      integer(kind = kint) :: inod_c, ist_fft
!
!
!$omp parallel do private(ip,m,j,ist,num,inod_c,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
        do j = 1, num
          do m = 1, nidx_rtp(3)
            inod_c = (m-1)*num + ist_fft
            X_rtp(m,j+ist) = X_FFT(j+inod_c)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ISPACK_to_prt_comp
!
! ------------------------------------------------------------------
!
      subroutine copy_ISPACK_to_prt_field(nnod_rtp, nidx_rtp,           &
     &          irt_rtp_smp_stack, ncomp_bwd, X_FFT, X_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      real(kind = kreal), intent(in) :: X_FFT(nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(nidx_rtp(3),irt_rtp_smp_stack(np_smp),ncomp_bwd)
!
!
      integer(kind = kint) :: m, j, ip, ist, num, nd
      integer(kind = kint) :: inod_c, ist_fft
!
!
!$omp parallel do private(ip,m,j,ist,num,inod_c,ist_fft,nd)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
        do nd = 1, ncomp_bwd
          do j = 1, num
            do m = 1, nidx_rtp(3)
              inod_c = (j-1 + (m-1)*num + ist_fft) * ncomp_bwd
              X_rtp(m,j+ist,nd) = X_FFT(nd+inod_c)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ISPACK_to_prt_field
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_ISPACK_from_prt_comp(nnod_rtp, nidx_rtp,          &
     &          irt_rtp_smp_stack, X_rtp, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(nidx_rtp(3),irt_rtp_smp_stack(np_smp))
!
      real(kind = kreal), intent(inout) :: X_FFT(nnod_rtp)
!
      integer(kind = kint) :: m, j, ip, ist, num
      integer(kind = kint) :: inod_c, ist_fft
!
!
!$omp parallel do private(m,j,ist,num,inod_c,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num =  irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
!
        do m = 1, nidx_rtp(3)
          inod_c = (m-1)*num + ist_fft
          do j = 1, num
              X_FFT(j+inod_c) = X_rtp(m,j+ist)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ISPACK_from_prt_comp
!
! ------------------------------------------------------------------
!
      subroutine copy_ISPACK_from_prt_field(nnod_rtp, nidx_rtp,         &
     &          irt_rtp_smp_stack, ncomp_fwd, X_rtp, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(nidx_rtp(3),irt_rtp_smp_stack(np_smp),ncomp_fwd)
!
      real(kind = kreal), intent(inout) :: X_FFT(nnod_rtp*ncomp_fwd)
!
      integer(kind = kint) :: m, j, ip, ist, num
      integer(kind = kint) :: inod_c, ist_fft
!
!
!$omp parallel do private(m,j,ist,num,inod_c,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num =  irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
!
        do m = 1, nidx_rtp(3)
          do j = 1, num
              inod_c = (j-1 + (m-1)*num + ist_fft) * ncomp_fwd
              X_FFT(inod_c+1:inod_c+ncomp_fwd)                          &
     &               = X_rtp(m,j+ist,1:ncomp_fwd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ISPACK_from_prt_field
!
! ------------------------------------------------------------------
!
      end module copy_rtp_data_to_ISPACK
