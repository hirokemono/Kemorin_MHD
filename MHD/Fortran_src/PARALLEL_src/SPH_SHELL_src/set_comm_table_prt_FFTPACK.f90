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
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                &
!!     &          ntot_sr_rtp, irev_sr_rtp, comm_sph_FFT)
!!      subroutine copy_prt_FFTPACK_to_send(nnod_rtp, nidx_rtp,         &
!!     &          irt_rtp_smp_stack, ncomp_fwd, irev_sr_rtp, X_FFT,     &
!!     &          n_WS, WS)
!!      subroutine copy_prt_comp_FFTPACK_to_send(nd, nnod_rtp, nidx_rtp,&
!!     &          irt_rtp_smp_stack, ncomp_fwd, irev_sr_rtp, X_FFT,     &
!!     &          n_WS, WS)
!!        type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
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
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                  &
     &          ntot_sr_rtp, irev_sr_rtp, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      integer(kind = kint) :: m, j
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!      integer(kind = kint) :: inod_s, inod_c
!
!
      do j = 1, irt_rtp_smp_stack(np_smp)
!        inod_c = 1 + (j-1)*nidx_rtp(3)
        ic_rtp = j
        ic_send = irev_sr_rtp(ic_rtp)
        if(ic_send .le. ntot_sr_rtp) then
          comm_sph_FFT%kl_fft(ic_send) = j
          comm_sph_FFT%m_fft(ic_send) =  1
          comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
        end if
!
!        inod_s = nidx_rtp(3) + (j-1)*nidx_rtp(3) 
        is_rtp = j + nidx_rtp(1)*nidx_rtp(2)
        is_send = irev_sr_rtp(is_rtp)
        if(is_send .le. ntot_sr_rtp) then
          comm_sph_FFT%kl_fft(is_send) = j
          comm_sph_FFT%m_fft(is_send) =  nidx_rtp(3)
          comm_sph_FFT%rnorm_sr_rtp(is_send) = one
        end if
      end do
!
!$omp parallel do private(m,j,ic_rtp,is_rtp,ic_send,is_send)
      do m = 1, nidx_rtp(3)/2 - 1
        do j = 1, irt_rtp_smp_stack(np_smp)
!          inod_c = (2*m  ) + (j-1)*nidx_rtp(3)
          ic_rtp = j + (2*m  ) * nidx_rtp(1)*nidx_rtp(2)
          ic_send = irev_sr_rtp(ic_rtp)
              if(ic_send .le. ntot_sr_rtp) then
                comm_sph_FFT%kl_fft(ic_send) = j
                comm_sph_FFT%m_fft(ic_send) = 2*m
                comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
              end if
!
!          inod_s = (2*m+1) + (j-1)*nidx_rtp(3)
          is_rtp = j + (2*m+1) * nidx_rtp(1)*nidx_rtp(2)
          is_send = irev_sr_rtp(is_rtp)
          if(is_send .le. ntot_sr_rtp) then
            comm_sph_FFT%kl_fft(is_send) = j
            comm_sph_FFT%m_fft(is_send) = 2*m+1
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
      subroutine copy_prt_FFTPACK_to_send(nnod_rtp, nidx_rtp,           &
     &          irt_rtp_smp_stack, ncomp_fwd, irev_sr_rtp, X_FFT,       &
     &          n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, j, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inod_s, inod_c
!
!
      do j = 1, irt_rtp_smp_stack(np_smp)
        do nd = 1, ncomp_fwd
          inod_c = nd + (j-1) * ncomp_fwd*nidx_rtp(3)
          inod_s = nd + (nidx_rtp(3)-1) * ncomp_fwd                     &
     &                  + (j-1) * ncomp_fwd*nidx_rtp(3) 
          ic_rtp = j
          is_rtp = j + nidx_rtp(1)*nidx_rtp(2)
          ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send) = X_FFT(inod_c)
          WS(is_send) = X_FFT(inod_s)
        end do
      end do
!
!$omp parallel do private(m,j,nd,inod_c,inod_s,                         &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
      do m = 1, (nidx_rtp(3)+1)/2 - 1
        do j = 1, irt_rtp_smp_stack(np_smp)
          do nd = 1, ncomp_fwd
            inod_c = nd + (2*m-1) * ncomp_fwd                           &
     &              + (j-1) * ncomp_fwd*nidx_rtp(3)
            inod_s = nd + (2*m  ) * ncomp_fwd                           &
     &              + (j-1) * ncomp_fwd*nidx_rtp(3)
            ic_rtp = j + (2*m  ) * nidx_rtp(1)*nidx_rtp(2)
            is_rtp = j + (2*m+1) * nidx_rtp(1)*nidx_rtp(2)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            WS(ic_send) = X_FFT(inod_c)
            WS(is_send) = X_FFT(inod_s)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_prt_FFTPACK_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_prt_comp_FFTPACK_to_send(nd, nnod_rtp, nidx_rtp,  &
     &          irt_rtp_smp_stack, ncomp_fwd, irev_sr_rtp, X_FFT,       &
     &          n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, j
      integer(kind = kint) :: ic_send, is_send, inod_s, inod_c
!
!
      do j = 1, irt_rtp_smp_stack(np_smp)
        inod_c = 1 + (j-1) * nidx_rtp(3)
        inod_s = nidx_rtp(3) + (j-1) * nidx_rtp(3)
        ic_send = nd + (irev_sr_rtp(inod_c  ) - 1) * ncomp_fwd
        is_send = nd + (irev_sr_rtp(inod_c+1) - 1) * ncomp_fwd
        WS(ic_send) = X_FFT(inod_c)
        WS(is_send) = X_FFT(inod_s)
      end do
!
!$omp parallel do private(m,j,inod_c,inod_s,ic_send,is_send)
      do m = 1, (nidx_rtp(3)+1)/2 - 1
        do j = 1, irt_rtp_smp_stack(np_smp)
          inod_c = (2*m  ) + (j-1) * nidx_rtp(3)
          inod_s = (2*m+1) + (j-1) * nidx_rtp(3)
          ic_send = nd + (irev_sr_rtp(inod_c+1) - 1) * ncomp_fwd
          is_send = nd + (irev_sr_rtp(inod_s+1) - 1) * ncomp_fwd
          WS(ic_send) = X_FFT(inod_c)
          WS(is_send) = X_FFT(inod_s)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_prt_comp_FFTPACK_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_prt_comp_FFTPACK_from_recv                        &
     &         (nd, nnod_rtp, nidx_rtp, irt_rtp_smp_stack,              &
     &          ncomp_bwd, irev_sr_rtp, n_WR, WR, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_FFT(nnod_rtp)
!
      integer(kind = kint) :: m, j, inod_s, inod_c, ic_recv, is_recv
!
!
        do j = 1, irt_rtp_smp_stack(np_smp)
          inod_c = 1 + (j-1)*nidx_rtp(3)
          inod_s = nidx_rtp(3) + (j-1)*nidx_rtp(3)
          ic_recv = (irev_sr_rtp(inod_c  ) - 1) * ncomp_bwd
          is_recv = (irev_sr_rtp(inod_c+1) - 1) * ncomp_bwd
          X_FFT(inod_c) = WR(ic_recv+nd)
          X_FFT(inod_s) = WR(is_recv+nd)
        end do
!
!$omp parallel private(j)
        do j = 1, irt_rtp_smp_stack(np_smp)
!$omp do private(m,inod_s,inod_c,ic_recv,is_recv)
          do m = 1, nidx_rtp(3)/2 - 1
            inod_c = (2*m  ) + (j-1)*nidx_rtp(3)
            inod_s = (2*m+1) + (j-1)*nidx_rtp(3)
            ic_recv = (irev_sr_rtp(inod_c+1) - 1) * ncomp_bwd
            is_recv = (irev_sr_rtp(inod_s+1) - 1) * ncomp_bwd
      !
            X_FFT(inod_c) = WR(ic_recv+nd)
            X_FFT(inod_s) = WR(is_recv+nd)
          end do
!$omp end do nowait
        end do
!$omp end parallel
!
      end subroutine copy_prt_comp_FFTPACK_from_recv
!
! ------------------------------------------------------------------
!
      end module set_comm_table_prt_FFTPACK
