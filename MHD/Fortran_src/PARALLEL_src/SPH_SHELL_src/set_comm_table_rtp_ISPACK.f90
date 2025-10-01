!>@file   set_comm_table_rtp_ISPACK.f90
!!@brief  module set_comm_table_rtp_ISPACK
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from ISPACK
!!
!!@verbatim
!!      subroutine set_comm_item_rtp_4_ISPACK                           &
!!     &         (nnod_rtp, nphi_rtp, istep_rtp, irt_rtp_smp_stack,     &
!!     &          ntot_sr_rtp, irev_sr_rtp, comm_sph_FFT)
!!
!!      subroutine copy_ISPACK_field_from_recv                          &
!!     &         (nnod_rtp, nphi_rtp, istep_rtp, irt_rtp_smp_stack,     &
!!     &          ncomp_bwd, irev_sr_rtp, n_WR, WR, X_FFT)
!!      subroutine copy_ISPACK_comp_from_recv                           &
!!     &         (nd, nnod_rtp, nphi_rtp, istep_rtp, irt_rtp_smp_stack, &
!!     &          ncomp_bwd, irev_sr_rtp,  n_WR, WR, X_FFT)
!!        type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!!@endverbatim
!!
      module set_comm_table_rtp_ISPACK
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
      subroutine set_comm_item_rtp_4_ISPACK                             &
     &         (nnod_rtp, nphi_rtp, istep_rtp, irt_rtp_smp_stack,       &
     &          ntot_sr_rtp, irev_sr_rtp, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: istep_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      integer(kind = kint) :: ip, m, j, ist, num, j0_rtp
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do private(ip,m,j,ist,num,j0_rtp,                        &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
          do j = 1, num
            j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
            ic_send = irev_sr_rtp(j0_rtp)
!           inod_c = j + (1-1) * num
            if(ic_send .le. ntot_sr_rtp) then
              comm_sph_FFT%ip_smp_fft(ic_send) = ip
              comm_sph_FFT%kl_fft(ic_send) = j
              comm_sph_FFT%m_fft(ic_send) =  1
              comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
            end if
!
            is_rtp = j0_rtp + istep_rtp(3)
            is_send = irev_sr_rtp(is_rtp)
!            inod_s = j + (2-1) * num
            if(is_send .le. ntot_sr_rtp) then
              comm_sph_FFT%ip_smp_fft(is_send) = ip
              comm_sph_FFT%kl_fft(is_send) = j
              comm_sph_FFT%m_fft(is_send) =  2
              comm_sph_FFT%rnorm_sr_rtp(is_send) = one
            end if
          end do
!
          do m = 2, nphi_rtp/2
            do j = 1, num
              j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
              ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
              ic_send = irev_sr_rtp(ic_rtp)
              is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
              is_send = irev_sr_rtp(is_rtp)
!              inod_s = j + (2*m-1) * num
!              inod_c = j + (2*m-2) * num
              if(ic_send .le. ntot_sr_rtp) then
                comm_sph_FFT%ip_smp_fft(ic_send) = ip
                comm_sph_FFT%kl_fft(ic_send) = j
                comm_sph_FFT%m_fft(ic_send) =  2*m-1
                comm_sph_FFT%rnorm_sr_rtp(ic_send) = two
              end if
!
              if(is_send .le. ntot_sr_rtp) then
                comm_sph_FFT%ip_smp_fft(is_send) = ip
                comm_sph_FFT%kl_fft(is_send) = j
                comm_sph_FFT%m_fft(is_send) =  2*m
                comm_sph_FFT%rnorm_sr_rtp(is_send) = -two
            end if
            end do
          end do
        end do
!$omp end parallel do
!
      end subroutine set_comm_item_rtp_4_ISPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_ISPACK_field_from_recv                            &
     &         (nnod_rtp, nphi_rtp, istep_rtp, irt_rtp_smp_stack,       &
     &          ncomp_bwd, irev_sr_rtp, n_WR, WR, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nphi_rtp, istep_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_FFT(ncomp_bwd*nnod_rtp)
!
      integer(kind = kint) :: m, j, ip, ist, ntot, num, inum, nd
      integer(kind = kint) :: j0_rtp, inod_s, inod_c, ist_fft
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num,ntot,inum,nd,inod_s,inod_c,       &
!$omp&                 j0_rtp,ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_bwd * num
        ist_fft = ncomp_bwd*nphi_rtp*irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
          j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
          do nd = 1, ncomp_bwd
            inum = nd + (j-1) * ncomp_bwd
            ic_rtp = j0_rtp
            is_rtp = j0_rtp + istep_rtp(3)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            inod_c = inum +                   ist_fft
            inod_s = inum + ncomp_bwd * num + ist_fft
            X_FFT(inod_c) = WR(ic_recv)
            X_FFT(inod_s) = WR(is_recv)
          end do
        end do
        do m = 2, nphi_rtp/2
          do j = 1, num
            j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (2*m-2) * ncomp_bwd * num + ist_fft
              inod_s = inum + (2*m-1) * ncomp_bwd * num + ist_fft
              ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
              is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              X_FFT(inod_c) =  half * WR(ic_recv)
              X_FFT(inod_s) = -half * WR(is_recv)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ISPACK_field_from_recv
!
! ------------------------------------------------------------------
!
      subroutine copy_ISPACK_comp_from_recv                             &
     &         (nd, nnod_rtp, nphi_rtp, istep_rtp, irt_rtp_smp_stack,   &
     &          ncomp_bwd, irev_sr_rtp,  n_WR, WR, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: istep_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_FFT(nnod_rtp)
!
      integer(kind = kint) :: m, j, j0_rtp, ip, ist, num
      integer(kind = kint) :: inod_s, inod_c, ist_fft
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do private(ip,m,j,ist,num,j0_rtp,inod_s,inod_c,          &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nphi_rtp
!
        do j = 1, num
          inod_c = j +       ist_fft
          inod_s = j + num + ist_fft
          j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
          ic_rtp = j0_rtp
          is_rtp = j0_rtp + istep_rtp(3)
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          X_FFT(inod_c) = WR(ic_recv)
          X_FFT(inod_s) = WR(is_recv)
        end do
        do m = 2, nphi_rtp/2
          do j = 1, num
            inod_c = j + (2*m-2) * num + ist_fft
            inod_s = j + (2*m-1) * num + ist_fft
            j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
            ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
            is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            X_FFT(inod_c) =  half * WR(ic_recv)
            X_FFT(inod_s) = -half * WR(is_recv)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ISPACK_comp_from_recv
!
! ------------------------------------------------------------------
!
      end module set_comm_table_rtp_ISPACK
