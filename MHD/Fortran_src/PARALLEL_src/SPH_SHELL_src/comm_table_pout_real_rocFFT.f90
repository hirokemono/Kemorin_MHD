!>@file   comm_table_pout_real_rocFFT.f90
!!@brief  module comm_table_pout_real_rocFFT
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTW
!!
!!@verbatim
!!      subroutine set_comm_item_pout_real_rocFFT(nnod_rtp, nnod_rt,    &
!!     &          irev_sr_rtp, Nfft_r, aNfft, comm_sph_FFT)
!!        integer(kind = kint), intent(in) :: nnod_rtp
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        integer(kind = kint), intent(in) :: Nfft_r, nnod_rt
!!        real(kind = kreal), intent(in) :: aNfft
!!        type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFT
!!
!!      subroutine pout_real_rocFFT_all_to_send(nnod_rt, Nfft_r,        &
!!     &          ncomp_fwd, C_fft, comm_sph_FFTW, n_WS, WS)
!!        integer(kind = kint), intent(in) :: nnod_rt, Nfft_r
!!        integer(kind = kint), intent(in) :: ncomp_fwd
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: C_fft(0:1,ncomp_fwd,nnod_rt,Nfft_r/2)
!!        type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!!        integer(kind = kint), intent(in) :: n_WS
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!      subroutine pout_real_rocFFT_all_from_recv                       &
!!     &         (nnod_rt, nnod_rtp, ncomp_bwd,                         &
!!     &          n_WR, irev_sr_rtp, WR, Nfft_r, C_fft)
!!        integer(kind = kint), intent(in) :: Nfft_r, nnod_rt
!!        integer(kind = kint), intent(in) :: nnod_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real(kind = kreal), intent(in):: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: C_fft(0:1,ncomp_bwd,nnod_rt,Nfft_r/2)
!!@endverbatim
!!
      module comm_table_pout_real_rocFFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
      subroutine set_comm_item_pout_real_rocFFT(nnod_rtp, nnod_rt,      &
     &          irev_sr_rtp, Nfft_r, aNfft, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_r, nnod_rt
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
      do m = 2, Nfft_r/2-1
        do j = 1, nnod_rt
          ic_rtp = j + (2*m-2) * nnod_rt
          ic_send = irev_sr_rtp(ic_rtp)
          if(ic_send .le. comm_sph_FFT%ntot_item) then
            comm_sph_FFT%kl_fftw(ic_send) =  j
            comm_sph_FFT%m_fftw(ic_send) =   m
            comm_sph_FFT%cnrm_sr_rtp(ic_send) = two * aNfft * ru
          end if
!
          is_rtp = j + (2*m-1) * nnod_rt
          is_send = irev_sr_rtp(is_rtp)
          if(is_send .le. comm_sph_FFT%ntot_item) then
            comm_sph_FFT%kl_fftw(is_send) =  j
            comm_sph_FFT%m_fftw(is_send) =  -m
            comm_sph_FFT%cnrm_sr_rtp(is_send) = - two * aNfft * ru
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
          comm_sph_FFT%m_fftw(ic_send) =  Nfft_r-1
          comm_sph_FFT%cnrm_sr_rtp(ic_send) = aNfft * ru
        end if
      end do
!
      end subroutine set_comm_item_pout_real_rocFFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pout_real_rocFFT_all_to_send(nnod_rt, Nfft_r,          &
     &          ncomp_fwd, C_fft, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in)                                    &
     &                   :: C_fft(0:1,ncomp_fwd,nnod_rt,Nfft_r/2)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
      integer(kind = kint), intent(in) :: n_WS
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kl, m, j, inum, ic_send
!
!
!$omp parallel do private(inum,kl,m,j,ic_send)
      do inum = 1, comm_sph_FFTW%ntot_item
        kl = comm_sph_FFTW%kl_fftw(inum)
        m =  comm_sph_FFTW%m_fftw(inum)
        j = (ione - sign(ione,m)) / 2
!
        ic_send = (inum-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &       = real(comm_sph_FFTW%cnrm_sr_rtp(inum))                    &
     &        * C_fft(j,1:ncomp_fwd,kl,m)
      end do
!$end parallel do
!
      end subroutine pout_real_rocFFT_all_to_send
!
! ------------------------------------------------------------------
!
      subroutine pout_real_rocFFT_all_from_recv                         &
     &         (nnod_rt, nnod_rtp, ncomp_bwd,                           &
     &          n_WR, irev_sr_rtp, WR, Nfft_r, C_fft)
!
      integer(kind = kint), intent(in) :: Nfft_r, nnod_rt
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind = kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: C_fft(0:1,ncomp_bwd,nnod_rt,Nfft_r/2)
!
      integer(kind = kint) :: m, j, ic_rtp, is_rtp, ic_recv, is_recv
!
!
!   normalization
!$omp parallel do private(j,ic_recv)
      do j = 1, nnod_rt
        ic_recv = (irev_sr_rtp(j) - 1) * ncomp_bwd
        C_fft(0,1:ncomp_bwd,j,1) = WR(ic_recv+1:ic_recv+ncomp_bwd)
        C_fft(1,1:ncomp_bwd,j,1) = zero
      end do
!$omp end parallel do
!
!$omp parallel do private(m,j,ic_rtp,is_rtp,ic_recv,is_recv)
      do m = 2, Nfft_r/2-1
        do j = 1, nnod_rt
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          C_fft(0,1:ncomp_bwd,j,m)                                      &
     &        =  half * WR(ic_recv+1:ic_recv+ncomp_bwd)
          C_fft(1,1:ncomp_bwd,j,m)                                      &
     &        = -half * WR(is_recv+1:is_recv+ncomp_bwd)
        end do
      end do
!$omp end parallel do
!
      m = Nfft_r/2
!$omp parallel do private(j,ic_rtp,ic_recv)
      do j = 1, nnod_rt
        ic_rtp = j + nnod_rt
        ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        C_fft(0,1:ncomp_bwd,j,m) = WR(ic_recv+1:ic_recv+ncomp_bwd)
        C_fft(1,1:ncomp_bwd,j,m) = zero
      end do
!$omp end parallel do
!
      end subroutine pout_real_rocFFT_all_from_recv
!
! ------------------------------------------------------------------
!
      end module comm_table_pout_real_rocFFT
