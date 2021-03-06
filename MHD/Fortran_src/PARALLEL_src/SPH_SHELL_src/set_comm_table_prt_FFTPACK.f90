!>@file   set_comm_table_prt_FFTPACK.f90
!!@brief  module set_comm_table_prt_FFTPACK
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTPACK5
!!
!!@verbatim
!!      subroutine set_comm_item_prt_4_FFTPACK                          &
!!     &         (nnod_rtp, ntot_sr_rtp, irev_sr_rtp,                   &
!!     &          mphi_rtp, nnod_rt, comm_sph_FFT)
!!        type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!!      subroutine copy_prt_FFTPACK_to_send(nnod_rtp, irev_sr_rtp,      &
!!     &          mphi_rtp, nnod_rt, ncomp_fwd, X_FFT, n_WS, WS)
!!      subroutine copy_prt_comp_FFTPACK_to_send                        &
!!     &         (nd, nnod_rtp, irev_sr_rtp, mphi_rtp, nnod_rt,         &
!!     &          ncomp_fwd, X_FFT, n_WS, WS)
!!        type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!!
!!      subroutine copy_prt_FFTPACK_from_recv                           &
!!     &         (nnod_rtp, irev_sr_rtp, mphi_rtp, nnod_rt,             &
!!     &          ncomp_bwd, n_WR, WR, X_FFT)
!!      subroutine copy_prt_comp_FFTPACK_from_recv                      &
!!     &         (nd, nnod_rtp, irev_sr_rtp, mphi_rtp, nnod_rt,         &
!!     &          ncomp_bwd, n_WR, WR, X_FFT)
!!@endverbatim
!!
      module set_comm_table_prt_FFTPACK
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_sph_comm_table_from_FFT
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine set_comm_item_prt_4_FFTPACK                            &
     &         (nnod_rtp, ntot_sr_rtp, irev_sr_rtp,                     &
     &          mphi_rtp, nnod_rt, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      integer(kind = kint) :: m, j
      integer(kind = kint) :: ic_send, is_send, ic_rtp, is_rtp
!
!
!$omp parallel do private(j,m,ic_rtp,is_rtp,ic_send,is_send)
      do j = 1, nnod_rt
        ic_rtp = j
        ic_send = irev_sr_rtp(ic_rtp)
        if(ic_send .le. ntot_sr_rtp) then
          comm_sph_FFT%kl_fft(ic_send) = j
          comm_sph_FFT%m_fft(ic_send) =  1
          comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
        end if
!
        is_rtp = j + nnod_rt
        is_send = irev_sr_rtp(is_rtp)
        if(is_send .le. ntot_sr_rtp) then
          comm_sph_FFT%kl_fft(is_send) = j
          comm_sph_FFT%m_fft(is_send) =  mphi_rtp
          comm_sph_FFT%rnorm_sr_rtp(is_send) = one
        end if
!
        do m = 1, mphi_rtp/2 - 1
          ic_rtp = j + (2*m  ) * nnod_rt
          ic_send = irev_sr_rtp(ic_rtp)
          if(ic_send .le. ntot_sr_rtp) then
            comm_sph_FFT%kl_fft(ic_send) = j
            comm_sph_FFT%m_fft(ic_send) =  2*m
            comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
          end if
!
          is_rtp = j + (2*m+1) * nnod_rt
          is_send = irev_sr_rtp(is_rtp)
          if(is_send .le. ntot_sr_rtp) then
            comm_sph_FFT%kl_fft(is_send) = j
            comm_sph_FFT%m_fft(is_send) =  2*m+1
            comm_sph_FFT%rnorm_sr_rtp(is_send) = one
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_comm_item_prt_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine copy_prt_FFTPACK_to_send(nnod_rtp, irev_sr_rtp,        &
     &          mphi_rtp, nnod_rt, ncomp_fwd, X_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_FFT(nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, j, jst
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do private(j,m,ic_rtp,is_rtp,ic_send,is_send,jst)
      do j = 1, nnod_rt
        jst = (j-1)*mphi_rtp
!
        is_rtp = j + nnod_rt
        ic_send = (irev_sr_rtp(j) - 1) * ncomp_fwd
        is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &            = X_fft(1+jst,1:ncomp_fwd)
        WS(is_send+1:is_send+ncomp_fwd)                                 &
     &            = X_fft(mphi_rtp+jst,1:ncomp_fwd)
        do m = 1, mphi_rtp/2 - 1
          ic_rtp = j + (2*m  ) * nnod_rt
          is_rtp = j + (2*m+1) * nnod_rt
          ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send+1:ic_send+ncomp_fwd)                               &
     &            = X_fft(2*m  +jst,1:ncomp_fwd)
          WS(is_send+1:is_send+ncomp_fwd)                               &
     &            = X_fft(2*m+1+jst,1:ncomp_fwd)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_prt_FFTPACK_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_prt_comp_FFTPACK_to_send                          &
     &         (nd, nnod_rtp, irev_sr_rtp, mphi_rtp, nnod_rt,           &
     &          ncomp_fwd, X_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, j, jst
      integer(kind = kint) :: ic_send, is_send, ic_rtp, is_rtp
!
!
!$omp parallel do private(j,m,ic_rtp,is_rtp,ic_send,is_send,jst)
      do j = 1, nnod_rt
        jst = (j-1)*mphi_rtp
!
        is_rtp = j + nnod_rt
        ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp_fwd
        is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
        WS(ic_send) = X_fft(1+jst)
        WS(is_send) = X_fft(mphi_rtp+jst)
        do m = 1, mphi_rtp/2 - 1
          ic_rtp = j + (2*m  ) * nnod_rt
          is_rtp = j + (2*m+1) * nnod_rt
          ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send) = X_fft(2*m  +jst)
          WS(is_send) = X_fft(2*m+1+jst)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_prt_comp_FFTPACK_to_send
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_prt_FFTPACK_from_recv                             &
     &         (nnod_rtp, irev_sr_rtp, mphi_rtp, nnod_rt,               &
     &          ncomp_bwd, n_WR, WR, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_FFT(nnod_rtp,ncomp_bwd)
!
      integer(kind = kint) :: j, m, jst
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do private(j,m,ic_rtp,is_rtp,ic_recv,is_recv,jst)
      do j = 1, nnod_rt
        jst = (j-1)*mphi_rtp
!
        is_rtp = j + nnod_rt
        ic_recv = (irev_sr_rtp(j) - 1) * ncomp_bwd
        is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
        X_fft(1+jst,1:ncomp_bwd)                                        &
     &        =WR(ic_recv+1:ic_recv+ncomp_bwd)
        X_fft(mphi_rtp+jst,1:ncomp_bwd)                                 &
     &        = WR(is_recv+1:is_recv+ncomp_bwd)
        do m = 1, mphi_rtp/2 - 1
          ic_rtp = j + (2*m  ) * nnod_rt
          is_rtp = j + (2*m+1) * nnod_rt
          ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          X_fft(2*m  +jst,1:ncomp_bwd)                                  &
     &           = WR(ic_recv+1:ic_recv+ncomp_bwd)
          X_fft(2*m+1+jst,1:ncomp_bwd)                                  &
     &           = WR(is_recv+1:is_recv+ncomp_bwd)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_prt_FFTPACK_from_recv
!
! ------------------------------------------------------------------
!
      subroutine copy_prt_comp_FFTPACK_from_recv                        &
     &         (nd, nnod_rtp, irev_sr_rtp, mphi_rtp, nnod_rt,           &
     &          ncomp_bwd, n_WR, WR, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_FFT(nnod_rtp)
!
      integer(kind = kint) :: j, m, jst
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do private(j,m,ic_rtp,is_rtp,ic_recv,is_recv,jst)
      do j = 1, nnod_rt
        jst = (j-1)*mphi_rtp
!
        is_rtp = j + nnod_rt
        ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp_bwd
        is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
        X_fft(1+jst) =        WR(ic_recv)
        X_fft(mphi_rtp+jst) = WR(is_recv)
        do m = 1, mphi_rtp/2 - 1
          ic_rtp = j + (2*m  ) * nnod_rt
          is_rtp = j + (2*m+1) * nnod_rt
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          X_fft(2*m  +jst) = WR(ic_recv)
          X_fft(2*m+1+jst) = WR(is_recv)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_prt_comp_FFTPACK_from_recv
!
! ------------------------------------------------------------------
!
      end module set_comm_table_prt_FFTPACK
