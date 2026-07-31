!>@file   comm_table_pin_real_rocFFT.f90
!!@brief  module comm_table_pin_real_rocFFT
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTW
!!
!!@verbatim
!!      subroutine set_comm_item_pin_real_rocFFT(nnod_rtp, nnod_rt,     &
!!     &          irev_sr_rtp, Nfft_r, comm_sph_FFT)
!!        integer(kind = kint), intent(in) :: nnod_rtp
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        integer(kind = kint), intent(in) :: Nfft_r, nnod_rt
!!        type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFT
!!
!!      subroutine pin_real_rocFFT_all_to_send(nnod_rt, Nfft_r,         &
!!     &          ncomp_fwd, X_fft, comm_sph_FFTW, n_WS, WS)
!!        complex(kind = fftw_complex), intent(in)                      &
!!     &                             :: X_fft(ncomp_fwd,nnod_rt,Nfft_r)
!!      subroutine pin_real_rocFFT_dmn_to_send(nd, nnod_rt, ncomp_fwd,  &
!!     &          Nfft_r, X_fft, comm_sph_FFTW, n_WS, WS)
!!        integer(kind = kint), intent(in) :: nd
!!        integer(kind = kint), intent(in) :: nnod_rt, Nfft_r
!!        integer(kind = kint), intent(in) :: ncomp_fwd
!!        real(kind = kreal), intent(in) :: X_fft(Nfft_r,nnod_rt)
!!        type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!!        integer(kind = kint), intent(in) :: n_WS
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!      subroutine pin_real_rocFFT_all_from_recv                        &
!!     &         (nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,            &
!!     &          ncomp_bwd, n_WR, WR, Nfft_r, X_fft)
!!        real(kind = kreal), intent(inout)                             &
!!     &              :: X_fft(Nfft_r,nnod_rt,ncomp_bwd)
!!      subroutine pin_real_rocFFT_dmn_from_recv                        &
!!     &         (nd, nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,        &
!!     &          ncomp_bwd, n_WR, WR, Nfft_r, X_fft)
!!        integer(kind = kint), intent(in) :: Nfft_r
!!        integer(kind = kint), intent(in) :: nnod_rtp, istep_rtp(3)
!!        integer(kind = kint), intent(in) :: nnod_rt
!!        integer(kind = kint), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real(kind=kreal), intent(in):: WR(n_WR)
!!       real(kind = kreal), intent(inout) :: X_fft(Nfft_r,nnod_rt)
!!@endverbatim
!!
      module comm_table_pin_real_rocFFT
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
      subroutine set_comm_item_pin_real_rocFFT                          &
     &        (nnod_rtp, istep_rtp, nnod_rt, ntot_sr_rtp, irev_sr_rtp,  &
     &         Nfft_r, comm_sph_FFTW)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_r
!
      type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!
      integer(kind = kint) ::  m, j, j0_rtp
      integer(kind = kint) ::  ic_rtp, is_rtp, ic_send, is_send
!      integer(kind = kint) ::  i
!
!
!$omp  parallel do                                                      &
!$omp& private(j,m,j0_rtp,ic_rtp,is_rtp,ic_send,is_send)
      do j = 1, nnod_rt
!        ist_c = 1 + Nfft_r * (j-1)
        j0_rtp = 1 + (j-1) * istep_rtp(1)
        ic_send = irev_sr_rtp(j0_rtp)
        if(ic_send .le. ntot_sr_rtp) then
          comm_sph_FFTW%kl_fftw(ic_send) = j
          comm_sph_FFTW%m_fftw(ic_send) =  1
          comm_sph_FFTW%cnrm_sr_rtp(ic_send) = ru
        end if
!        WS(ic_send) = aNfft * real(C_fft(ist_c))
!
        do m = 2, Nfft_r/2-1
!          ist_c = m + Nfft_r * (j-1)
          ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
          is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
          ic_send = irev_sr_rtp(ic_rtp)
          is_send = irev_sr_rtp(is_rtp)
          if(ic_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%kl_fftw(ic_send) = j
            comm_sph_FFTW%m_fftw(ic_send) =  2*m-1
            comm_sph_FFTW%cnrm_sr_rtp(ic_send) = two * ru
          end if
!          WS(ic_send) = two*aNfft * real(C_fft(ist_c))
!
          if(is_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%kl_fftw(is_send) = j
            comm_sph_FFTW%m_fftw(is_send) =  2*m
            comm_sph_FFTW%cnrm_sr_rtp(is_send) = -two * ru
          end if
!          WS(is_send) = two*aNfft * real(C_fft(ist_c)*iu)
        end do 
!        ist_c = Nfft_r + Nfft_r * (j-1)
        ic_rtp = j0_rtp + istep_rtp(3)
        ic_send = irev_sr_rtp(ic_rtp)
        if(ic_send .le. ntot_sr_rtp) then
          comm_sph_FFTW%kl_fftw(ic_send) = j
          comm_sph_FFTW%m_fftw(ic_send) =  Nfft_r-1
          comm_sph_FFTW%cnrm_sr_rtp(ic_send) = ru
        end if
!        WS(ic_send) = aNfft * real(C_fft(ist_c))
      end do
!$omp end parallel do
!
      end subroutine set_comm_item_pin_real_rocFFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pin_real_rocFFT_all_to_send(nnod_rt, ncomp_fwd,        &
     &          Nfft_r, X_fft, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rt, Nfft_r
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in)                                    &
     &              :: X_fft(Nfft_r,nnod_rt,ncomp_fwd)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kl, m, inum, ic_send
!
!
!$omp parallel do private(inum,kl,m,ic_send)
      do inum = 1, comm_sph_FFTW%ntot_item
        kl = comm_sph_FFTW%kl_fftw(inum)
        m =  comm_sph_FFTW%m_fftw(inum)
!
        ic_send = (inum-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &     = real(comm_sph_FFTW%cnrm_sr_rtp(inum))                      &
     &            * X_fft(m,kl,1:ncomp_fwd)
      end do
!$end parallel do
!
      end subroutine pin_real_rocFFT_all_to_send
!
! ------------------------------------------------------------------
!
      subroutine pin_real_rocFFT_dmn_to_send(nd, nnod_rt, ncomp_fwd,    &
     &          Nfft_r, X_fft, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: nnod_rt, Nfft_r
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_fft(Nfft_r,nnod_rt)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kl, m, inum, ic_send
!
!
!$omp parallel do private(inum,kl,m,ic_send)
      do inum = 1, comm_sph_FFTW%ntot_item
        kl = comm_sph_FFTW%kl_fftw(inum)
        m =  comm_sph_FFTW%m_fftw(inum)
!
        ic_send = nd + (inum-1) * ncomp_fwd
        WS(ic_send)                                                     &
     &     = real(comm_sph_FFTW%cnrm_sr_rtp(inum)) * X_fft(m,kl)
      end do
!$end parallel do
!
      end subroutine pin_real_rocFFT_dmn_to_send
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pin_real_rocFFT_all_from_recv                          &
     &         (nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,              &
     &          ncomp_bwd, n_WR, WR, Nfft_r, X_fft)
!
      integer(kind = kint), intent(in) :: Nfft_r
      integer(kind = kint), intent(in) :: nnod_rtp, istep_rtp(3)
      integer(kind = kint), intent(in) :: nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &              :: X_fft(Nfft_r,nnod_rt,ncomp_bwd)
!
      integer(kind = kint) :: nd, j, j0_rtp, m
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp  parallel do                                                      &
!$omp& private(nd,j,m,j0_rtp,ic_rtp,is_rtp,ic_recv,is_recv)
      do j = 1, nnod_rt
        j0_rtp = 1 + (j-1) * istep_rtp(1)
        do nd = 1, ncomp_bwd
          ic_recv = nd + (irev_sr_rtp(j0_rtp) - 1) * ncomp_bwd
          X_fft(1,j,nd) = WR(ic_recv)
          do m = 2, Nfft_r/2-1
            ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
            is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            X_fft(2*m-1,j,nd) =  half * WR(ic_recv)
            X_fft(2*m,  j,nd) = -half * WR(is_recv)
          end do
          ic_rtp = j0_rtp + istep_rtp(3)
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          X_fft(Nfft_r-1,j,nd) = WR(ic_recv)
        end do
      end do
!$omp end parallel do
!
      end subroutine pin_real_rocFFT_all_from_recv
!
! ------------------------------------------------------------------
!
      subroutine pin_real_rocFFT_dmn_from_recv                          &
     &         (nd, nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,          &
     &          ncomp_bwd, n_WR, WR, Nfft_r, X_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: Nfft_r
      integer(kind = kint), intent(in) :: nnod_rtp, istep_rtp(3)
      integer(kind = kint), intent(in) :: nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_fft(Nfft_r,nnod_rt)
!
      integer(kind = kint) :: j, j0_rtp, m
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp  parallel do                                                      &
!$omp& private(j,m,j0_rtp,ic_rtp,is_rtp,ic_recv,is_recv)
      do j = 1, nnod_rt
        j0_rtp = 1 + (j-1) * istep_rtp(1)
        ic_recv = nd + (irev_sr_rtp(j0_rtp) - 1) * ncomp_bwd
        X_fft(1,j) = WR(ic_recv)
        do m = 2, Nfft_r/2-1
          ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
          is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          X_fft(2*m-1,j) =  half * WR(ic_recv)
          X_fft(2*m,  j) = -half * WR(is_recv)
        end do
        ic_rtp = j0_rtp + istep_rtp(3)
        ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        X_fft(Nfft_r-1,j) = WR(ic_recv)
      end do
!$omp end parallel do
!
      end subroutine pin_real_rocFFT_dmn_from_recv
!
! ------------------------------------------------------------------
!
      end module comm_table_pin_real_rocFFT
