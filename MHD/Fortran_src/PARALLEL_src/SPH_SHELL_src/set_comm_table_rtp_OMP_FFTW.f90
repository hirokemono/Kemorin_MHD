!>@file   set_comm_table_rtp_OMP_FFTW.F90
!!@brief  module set_comm_table_rtp_OMP_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  communication table from FFTW with OpenMP
!!
!!@verbatim
!!      subroutine set_comm_item_rtp_OMP_FFTW(nnod_rt, nnod_rtp,        &
!!     &          irev_sr_rtp, Nfft_c, aNfft, C_fft)
!!        type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFT
!!@endverbatim
!!
      module set_comm_table_rtp_OMP_FFTW
!
      use m_precision
      use m_constants
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
      subroutine set_comm_item_rtp_OMP_FFTW(nnod_rtp, nnod_rt,          &
     &          irev_sr_rtp, Nfft_c, aNfft, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c, nnod_rt
      real(kind = kreal), intent(in) :: aNfft
!
      type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFT
!
      integer(kind = kint) ::  m, j, ic_rtp, is_rtp, ic_send, is_send
!
!
      do j = 1, nnod_rt
        ic_send = irev_sr_rtp(j)
        if(ic_send .le. comm_sph_FFT%ntot_item) then
          comm_sph_FFT%kl_fftw(ic_send) = j
          comm_sph_FFT%m_fftw(ic_send) =  1
          comm_sph_FFT%cnrm_sr_rtp(ic_send) = aNfft * ru
        end if
      end do
!
!$omp parallel do private(m,j,ic_rtp,is_rtp,ic_send,is_send)
      do m = 2, Nfft_c-1
        do j = 1, nnod_rt
          ic_rtp = j + (2*m-2) * nnod_rt
          ic_send = irev_sr_rtp(ic_rtp)
          if(ic_send .le. comm_sph_FFT%ntot_item) then
            comm_sph_FFT%kl_fftw(ic_send) = j
            comm_sph_FFT%m_fftw(ic_send) =  m
            comm_sph_FFT%cnrm_sr_rtp(ic_send) = two * aNfft * ru
          end if
!
          is_rtp = j + (2*m-1) * nnod_rt
          is_send = irev_sr_rtp(is_rtp)
          if(is_send .le. comm_sph_FFT%ntot_item) then
            comm_sph_FFT%kl_fftw(is_send) = j
            comm_sph_FFT%m_fftw(is_send) =  m
            comm_sph_FFT%cnrm_sr_rtp(is_send) = two * aNfft * iu
          end if
        end do 
      end do
!$omp end parallel do
!
      do j = 1, nnod_rt
        ic_rtp = j + nnod_rt
        ic_send = irev_sr_rtp(ic_rtp)
        if(ic_send .le. comm_sph_FFT%ntot_item) then
          comm_sph_FFT%kl_fftw(ic_send) = j
          comm_sph_FFT%m_fftw(ic_send) =  2
          comm_sph_FFT%cnrm_sr_rtp(ic_send) = two * aNfft * ru
        end if
      end do
!
      end subroutine set_comm_item_rtp_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine set_OMP_FFTW_field_from_recv                           &
     &         (nnod_rt, nnod_rtp, ncomp_bwd,                           &
     &          n_WR, irev_sr_rtp, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: Nfft_c, nnod_rt
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind=kreal), intent(in):: WR(n_WR)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &                             :: C_fft(ncomp_bwd,nnod_rt,Nfft_c)
!
      integer(kind = kint) :: m, j, ic_rtp, is_rtp, ic_recv, is_recv
!
!
!   normalization
!$omp parallel do private(j,ic_recv)
      do j = 1, nnod_rt
        ic_recv = (irev_sr_rtp(j) - 1) * ncomp_bwd
        C_fft(1:ncomp_bwd,j,1) = cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd), &
     &                                 zero, kind(0d0))
      end do
!$omp end parallel do
!
!$omp parallel do private(m,j,ic_rtp,is_rtp,ic_recv,is_recv)
      do m = 2, Nfft_c-1
        do j = 1, nnod_rt
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          C_fft(1:ncomp_bwd,j,m)                                        &
     &        = half * cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),           &
     &                      -WR(is_recv+1:is_recv+ncomp_bwd),kind(0d0))
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j,ic_rtp,ic_recv)
      do j = 1, nnod_rt
        ic_rtp = j + nnod_rt
        ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        C_fft(1:ncomp_bwd,j,Nfft_c) = half                              &
     &                        * cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),  &
     &                                   zero, kind(0d0))
      end do
!$omp end parallel do
!
      end subroutine set_OMP_FFTW_field_from_recv
!
! ------------------------------------------------------------------
!
      subroutine set_OMP_FFTW_comp_from_recv                            &
     &         (nd, nnod_rt, nnod_rtp, ncomp_bwd,                       &
     &          n_WR, irev_sr_rtp, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: Nfft_c, nnod_rt
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind=kreal), intent(in):: WR(n_WR)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &                             :: C_fft(nnod_rt,Nfft_c)
!
      integer(kind = kint) :: m, j, ic_rtp, is_rtp, ic_recv, is_recv
!
!
!   normalization
!$omp parallel do private(j,ic_recv)
      do j = 1, nnod_rt
        ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp_bwd
        C_fft(j,1) = cmplx(WR(ic_recv), zero, kind(0d0))
      end do
!$omp end parallel do
!
!$omp parallel do private(m,j,ic_rtp,is_rtp,ic_recv,is_recv)
      do m = 2, Nfft_c-1
        do j = 1, nnod_rt
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          C_fft(j,m)                                                    &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j,ic_rtp,ic_recv)
      do j = 1, nnod_rt
        ic_rtp = j + nnod_rt
        ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        C_fft(j,Nfft_c) = half * cmplx(WR(ic_recv), zero, kind(0d0))
      end do
!$omp end parallel do
!
      end subroutine set_OMP_FFTW_comp_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_field_from_OMP_FFTW                           &
     &         (nnod_rtp, ncomp_bwd, X_FFT, X_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp_bwd
      real(kind = kreal), intent(in) :: X_FFT(ncomp_bwd, nnod_rtp)
!
      real(kind = kreal), intent(inout)  :: X_rtp(nnod_rtp, ncomp_bwd)
!
      integer(kind = kint) :: nd
!
!$omp parallel do private(nd)
      do nd = 1, ncomp_bwd
        X_rtp(1:nnod_rtp,nd) = X_FFT(nd,1:nnod_rtp)
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_field_from_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_field_to_OMP_FFTW                             &
     &         (nnod_rtp, ncomp_fwd, X_rtp, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp_fwd
      real(kind = kreal), intent(in)  :: X_rtp(nnod_rtp, ncomp_fwd)
!
      real(kind = kreal), intent(inout) :: X_FFT(ncomp_fwd, nnod_rtp)
!
      integer(kind = kint) :: inod
!
!$omp parallel do private(inod)
      do inod = 1, nnod_rtp
        X_FFT(1:ncomp_fwd,inod) = X_rtp(inod,1:ncomp_fwd)
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_field_to_OMP_FFTW
!
! ------------------------------------------------------------------
!
      end module set_comm_table_rtp_OMP_FFTW
