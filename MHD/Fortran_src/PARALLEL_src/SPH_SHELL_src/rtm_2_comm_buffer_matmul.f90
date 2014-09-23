!>@file   rtm_2_comm_buffer_matmul.f90
!!@brief  module rtm_2_comm_buffer_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform for testing
!!
!!@verbatim
!!      subroutine set_rtm_comm_tbl_matmul(np_smp, idx_rtm_smp_stack,   &
!!     &          nnod_rtm, nidx_rtm, istep_rtm, irev_sr_rtm,           &
!!     &          ntot_sr_rtm, item_sr_rtm_mat_n, item_sr_rtm_mat_s,    &
!!     &          irecv_sr_rtm_mat_n, irecv_sr_rtm_mat_s,               &
!!     &          sgn_sym_2_send, sgn_sym_2_vt)
!!
!!      subroutine copy_recv_2_rtm_sym_mat(nnod_rtm, ntot_sr_rtm,       &
!!     &          irecv_sr_rtm_mat_n, irecv_sr_rtm_mat_s, sgn_sym_2_vt, &
!!     &           ncomp, nvector, nscalar, WR, v_rtm, s_rtm)
!!      subroutine copy_rtm_sym_mat_2_send(nnod_rtm, ntot_sr_rtm,       &
!!     &          item_sr_rtm_mat_n, item_sr_rtm_mat_s, sgn_sym_2_send, &
!!     &          ncomp, nvector, nscalar, v_rtm, s_rtm, WS)
!!
!!      subroutine copy_recv_2_rtm_mat(nnod_rtm, ntot_sr_rtm,           &
!!     &          irecv_sr_rtm_mat_n, ncomp, nvector, nscalar,          &
!!     &          WR, v_rtm, s_rtm)
!!      subroutine copy_rtm_mat_2_send(nnod_rtm, ntot_sr_rtm,           &
!!     &          item_sr_rtm_mat_n, ncomp, nvector, nscalar,           &
!!     &          v_rtm, s_rtm, WS)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module rtm_2_comm_buffer_matmul
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_rtm_comm_tbl_matmul(np_smp, idx_rtm_smp_stack,     &
     &          nnod_rtm, nidx_rtm, istep_rtm, item_sr_rtm, irev_sr_rtm,&
     &          ntot_sr_rtm, item_sr_rtm_mat_n, item_sr_rtm_mat_s,      &
     &          irecv_sr_rtm_mat_n, irecv_sr_rtm_mat_s,                 &
     &          sgn_sym_2_send, sgn_sym_2_vt)
!
      integer(kind = kint), intent(in) :: np_smp, nnod_rtm
      integer(kind = kint), intent(in) :: ntot_sr_rtm
      integer(kind = kint), intent(in) :: idx_rtm_smp_stack(0:np_smp,3)
      integer(kind = kint), intent(in) :: nidx_rtm(3), istep_rtm(3)
      integer(kind = kint), intent(in) :: item_sr_rtm(ntot_sr_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
!
      integer(kind = kint), intent(inout)                               &
     &                      :: item_sr_rtm_mat_n(ntot_sr_rtm)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_sr_rtm_mat_s(ntot_sr_rtm)
      integer(kind = kint), intent(inout)                               &
     &                      :: irecv_sr_rtm_mat_n(nnod_rtm)
      integer(kind = kint), intent(inout)                               &
     &                      :: irecv_sr_rtm_mat_s(nnod_rtm)
      real(kind = kreal), intent(inout) :: sgn_sym_2_send(ntot_sr_rtm)
      real(kind = kreal), intent(inout) :: sgn_sym_2_vt(nnod_rtm)
!
      integer(kind = kint) :: k_rtm, ip, nkr, kk, l_rtm, lo_rtm
      integer(kind = kint) :: i_rtm, m_rtm, inod, itmp, inum
      integer(kind = kint) :: imat_kt, imat_tk
      integer(kind = kint) :: imat, in_rtm, is_rtm, in_recv, is_recv
!
!
      do inum = 1, ntot_sr_rtm
        i_rtm = item_sr_rtm(inum)
        l_rtm = 1 + mod((i_rtm-1),nidx_rtm(2))
        itmp =  1 + (inum - l_rtm) / nidx_rtm(2)
        k_rtm = 1 + mod((itmp-1),nidx_rtm(1))
        m_rtm =  1 + (itmp - k_rtm) / nidx_rtm(1)
        lo_rtm = 1 + nidx_rtm(2) - l_rtm
!
        imat_tk = l_rtm + (kk-1) * nidx_rtm(2)                          &
     &              + (m_rtm-1) * nkr*nidx_rtm(2)                       &
     &              + idx_rtm_smp_stack(ip-1,1)*nidx_rtm(2)*nidx_rtm(3)
      end do
!
      do ip = 1, np_smp
        nkr = idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1)
        do m_rtm = 1, nidx_rtm(3)
          do l_rtm = 1, nidx_rtm(2)
            do kk = 1, nkr
              k_rtm = kk + idx_rtm_smp_stack(ip-1,1)
              imat_kt = kk + (l_rtm-1) * nkr                            &
     &              + (m_rtm-1) * nkr*nidx_rtm(2)                       &
     &              + idx_rtm_smp_stack(ip-1,1)*nidx_rtm(2)*nidx_rtm(3)
!
              in_rtm = 1 + (l_rtm-1) * istep_rtm(2)                     &
     &                   + (k_rtm-1) * istep_rtm(1)                     &
     &                   + (m_rtm-1) * istep_rtm(3)
              is_rtm = 1 + (nidx_rtm(2)-l_rtm) * istep_rtm(2)           &
     &                   + (k_rtm-1) * istep_rtm(1)                     &
     &                   + (m_rtm-1) * istep_rtm(3)
              in_recv = irev_sr_rtm(in_rtm)
              is_recv = irev_sr_rtm(is_rtm)
              irecv_sr_rtm_mat_n(imat_kt) =   in_recv
              irecv_sr_rtm_mat_s(imat_kt) =   is_recv
!
              if(l_rtm .le. (nidx_rtm(2)/2)) then
                sgn_sym_2_vt(imat_kt) = one
              else if(in_rtm .eq. is_rtm) then
                sgn_sym_2_vt(imat_kt) = zero
              else
                sgn_sym_2_vt(imat_kt) = dminus
              end if
            end do
          end do
        end do
      end do
!
      do inod = 1, nnod_rtm
        in_recv = irecv_sr_rtm_mat_n(inod)
        is_recv = irecv_sr_rtm_mat_s(inod)
        if(in_recv .le. ntot_sr_rtm) then
          item_sr_rtm_mat_n(in_recv) = inod
          sgn_sym_2_send(in_recv) = sgn_sym_2_vt(inod)
        end if
        if(is_recv .le. ntot_sr_rtm) item_sr_rtm_mat_s(is_recv) = inod
      end do
!
      end subroutine set_rtm_comm_tbl_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_recv_2_rtm_sym_mat(nnod_rtm, ntot_sr_rtm,         &
     &          irecv_sr_rtm_mat_n, irecv_sr_rtm_mat_s, sgn_sym_2_vt,   &
     &           ncomp, nvector, nscalar, WR, v_rtm, s_rtm)
!
      integer(kind = kint), intent(in) :: nnod_rtm, ntot_sr_rtm
!
      integer(kind = kint), intent(in)                                  &
     &                      :: irecv_sr_rtm_mat_n(nnod_rtm)
      integer(kind = kint), intent(in)                                  &
     &                      :: irecv_sr_rtm_mat_s(nnod_rtm)
      real(kind = kreal), intent(in) :: sgn_sym_2_vt(nnod_rtm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(inout) :: WR(ncomp*ntot_sr_rtm)
!
      real(kind = kreal), intent(inout) :: v_rtm(nvector*nnod_rtm,3)
      real(kind = kreal), intent(inout) :: s_rtm(nscalar*nnod_rtm)
!
      integer(kind = kint) :: ii, nd, i, in_recv, is_recv
!
!
!
!$omp parallel do private(nd,in_recv)
      do nd = 1, ncomp
        in_recv = nd + ncomp*ntot_sr_rtm
        WR(in_recv) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(ii,nd,i,in_recv,is_recv)
      do ii = 1, nvector*nnod_rtm
        nd =   1 + mod(ii-1,nvector)
        i = 1 + (ii - nd) / nvector
        in_recv = 3*nd + (irecv_sr_rtm_mat_n(i)-1) * ncomp
        is_recv = 3*nd + (irecv_sr_rtm_mat_s(i)-1) * ncomp
!
        v_rtm(ii,1) = half * (WR(in_recv-2) * sgn_sym_2_vt(i)           &
     &                      + WR(is_recv-2) )
        v_rtm(ii,2) = half * (WR(in_recv-1) * sgn_sym_2_vt(i)           &
     &                      + WR(is_recv-1) )
        v_rtm(ii,3) = half * (WR(in_recv  ) * sgn_sym_2_vt(i)           &
     &                      + WR(is_recv  ) )
      end do
!$omp end parallel do
!
!$omp parallel do private(ii,nd,i,in_recv,is_recv)
      do ii = 1, nscalar*nnod_rtm
        nd =   1 + mod(ii-1,nscalar)
        i = 1 + (ii - nd) / nscalar
        in_recv = nd + 3*nvector + (irecv_sr_rtm_mat_n(i)-1) * ncomp
        is_recv = nd + 3*nvector + (irecv_sr_rtm_mat_s(i)-1) * ncomp
        s_rtm(ii) = half * (WR(in_recv  ) * sgn_sym_2_vt(i)             &
     &                    + WR(is_recv  ) )
      end do
!$omp end parallel do
!
      end subroutine copy_recv_2_rtm_sym_mat
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtm_sym_mat_2_send(nnod_rtm, ntot_sr_rtm,         &
     &          item_sr_rtm_mat_n, item_sr_rtm_mat_s, sgn_sym_2_send,   &
     &          ncomp, nvector, nscalar, v_rtm, s_rtm, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm, ntot_sr_rtm
!
      integer(kind = kint), intent(in)                                  &
     &                      :: item_sr_rtm_mat_n(ntot_sr_rtm)
      integer(kind = kint), intent(in)                                  &
     &                      :: item_sr_rtm_mat_s(ntot_sr_rtm)
      real(kind = kreal), intent(in) :: sgn_sym_2_send(nnod_rtm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: v_rtm(nvector*nnod_rtm,3)
      real(kind = kreal), intent(in) :: s_rtm(nscalar*nnod_rtm)
!
      real(kind = kreal), intent(inout) :: WS(ncomp*ntot_sr_rtm)
!
      integer(kind = kint) :: i, ii, nd, inum, is_send, ia_send
!
!
!
!$omp parallel
!$omp do private(ii,nd,i,inum,is_send,ia_send)
      do ii = 1, nvector*ntot_sr_rtm
        nd =   1 + mod(ii-1,nvector)
        i = 1 + (ii - nd) / nvector
        inum = 3*nd + (i-1)*ncomp
        is_send = nd + (item_sr_rtm_mat_n(i)-1) * ncomp
        ia_send = nd + (item_sr_rtm_mat_s(i)-1) * ncomp
        WS(3*inum-2) = v_rtm(is_send,1)                                 &
     &                + sgn_sym_2_send(i) *v_rtm(ia_send,1)
        WS(3*inum-1) = v_rtm(is_send,2)                                 &
     &                + sgn_sym_2_send(i) *v_rtm(ia_send,2)
        WS(3*inum  ) = v_rtm(is_send,3)                                 &
     &                + sgn_sym_2_send(i) *v_rtm(ia_send,3)
      end do
!$omp end do
!
!$omp do private(ii,nd,i,inum,is_send,ia_send)
      do ii = 1, nscalar*ntot_sr_rtm
        nd =   1 + mod(ii-1,nscalar)
        i = 1 + (ii - nd) / nscalar
        inum = nd + 3*nvector + (i-1)*ncomp
        is_send = nd + 3*nvector + (item_sr_rtm_mat_n(i)-1) * ncomp
        ia_send = nd + 3*nvector + (item_sr_rtm_mat_s(i)-1) * ncomp
        WS(inum) = s_rtm(is_send)                                       &
     &            + sgn_sym_2_send(i) *s_rtm(ia_send)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine copy_rtm_sym_mat_2_send
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_recv_2_rtm_mat(nnod_rtm, ntot_sr_rtm,             &
     &          irecv_sr_rtm_mat_n, ncomp, nvector, nscalar,           &
     &          WR, v_rtm, s_rtm)
!
      integer(kind = kint), intent(in) :: nnod_rtm, ntot_sr_rtm
!
      integer(kind = kint), intent(in)                                  &
     &                      :: irecv_sr_rtm_mat_n(nnod_rtm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(inout) :: WR(ncomp*(ntot_sr_rtm+1))
!
      real(kind = kreal), intent(inout) :: v_rtm(nvector*nnod_rtm,3)
      real(kind = kreal), intent(inout) :: s_rtm(nscalar*nnod_rtm)
!
      integer(kind = kint) :: i, ii, nd, in_recv
!
!
!
!$omp parallel do private(nd,in_recv)
      do nd = 1, ncomp
        in_recv = nd + ncomp*ntot_sr_rtm
        WR(in_recv) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel
!$omp do private(ii,nd,i,in_recv)
      do ii = 1, nvector*nnod_rtm
        nd =   1 + mod(ii-1,nvector)
        i = 1 + (ii - nd) / nvector
        in_recv = 3*nd + (irecv_sr_rtm_mat_n(i)-1) * ncomp
!
        v_rtm(ii,1) = WR(in_recv-2)
        v_rtm(ii,2) = WR(in_recv-1)
        v_rtm(ii,3) = WR(in_recv  )
      end do
!$omp end do
!
!$omp do private(ii,nd,i,in_recv)
      do ii = 1, nscalar*nnod_rtm
        nd =   1 + mod(ii-1,nscalar)
        i = 1 + (ii - nd) / nscalar
        in_recv = nd + 3*nvector + (irecv_sr_rtm_mat_n(i)-1) * ncomp
        s_rtm(ii) = WR(in_recv)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine copy_recv_2_rtm_mat
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtm_mat_2_send(nnod_rtm, ntot_sr_rtm,             &
     &          item_sr_rtm_mat_n, ncomp, nvector, nscalar,             &
     &          v_rtm, s_rtm, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm, ntot_sr_rtm
      integer(kind = kint), intent(in)                                  &
     &                      :: item_sr_rtm_mat_n(ntot_sr_rtm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: v_rtm(nvector*nnod_rtm,3)
      real(kind = kreal), intent(in) :: s_rtm(nscalar*nnod_rtm)
!
      real(kind = kreal), intent(inout) :: WS(ncomp*ntot_sr_rtm)
!
      integer(kind = kint) :: i, ii, nd, inum, is_send
!
!
!$omp parallel
!$omp do private(ii,nd,i,inum,is_send)
      do ii = 1, nvector*ntot_sr_rtm
        nd =   1 + mod(ii-1,nvector)
        i = 1 + (ii - nd) / nvector
        inum = 3*nd + (i-1)*ncomp
        is_send = nd + (item_sr_rtm_mat_n(i)-1) * ncomp
        WS(3*inum-2) = v_rtm(is_send,1)
        WS(3*inum-1) = v_rtm(is_send,2)
        WS(3*inum  ) = v_rtm(is_send,3)
      end do
!$omp end do
!
!$omp do private(ii,nd,i,inum,is_send)
      do ii = 1, nscalar*ntot_sr_rtm
        nd =   1 + mod(ii-1,nscalar)
        i = 1 + (ii - nd) / nscalar
        inum = nd + 3*nvector + (i-1)*ncomp
        is_send = nd + 3*nvector + (item_sr_rtm_mat_n(i)-1) * ncomp
        WS(inum) = s_rtm(is_send)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine copy_rtm_mat_2_send
!
! -----------------------------------------------------------------------
!
      end module rtm_2_comm_buffer_matmul
