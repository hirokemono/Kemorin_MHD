!>@file   copy_SR_4_leg_trans_spin.f90
!!@brief  module copy_SR_4_leg_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine get_rlm_from_recv_spin                               &
!!     &         (nnod_rlm, nidx_rlm, inod_rlm_smp_stack,               &
!!     &          ncomp, nvector, nscalar,WRecv, sp_rlm_spin)
!!      subroutine get_rtm_from_recv_spin(nnod_rtm, inod_rtm_smp_stack, &
!!     &          ncomp, nvector, nscalar, WRecv, vr_rtm_spin)
!!
!!      subroutine set_rlm_to_send_spin(nnod_rlm, nidx_rlm,             &
!!     &          ncomp, nvector, nscalar, sp_rlm_spin, Wsend)
!!      subroutine set_rtm_to_send_spin                                 &
!!     &         (nnod_rtm, ncomp, nvector, nscalar, vr_rtm_spin, Wsend)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module copy_SR_4_leg_trans_spin
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_sph_trans_comm_table
      use m_sph_trans_comm_tbl_1D
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine get_rlm_from_recv_spin                                 &
     &         (nnod_rlm, nidx_rlm, inod_rlm_smp_stack,                 &
     &          ncomp, nvector, nscalar,WRecv, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: inod_rlm_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: WRecv(ncomp*nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, nd, i_rlm_0
      integer(kind = kint) :: j_rlm, k_rlm
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,nd,i_rlm_0,j_rlm,k_rlm)
      do ip = 1, np_smp
        ist = nvector*inod_rlm_smp_stack(ip-1) + 1
        ied = nvector*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          inod = 1 + mod( (inum-1),nnod_rlm)
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
          nd =   1 + (inum - inod) / nnod_rlm
!
          i_rlm_0 = 3*nd + (irev_rlm_1D(j_rlm,k_rlm) - 1) * ncomp
!
          sp_rlm_spin(j_rlm,k_rlm,nd          ) = WRecv(i_rlm_0-2)
          sp_rlm_spin(j_rlm,k_rlm,nd+nvector  ) = WRecv(i_rlm_0-1)
          sp_rlm_spin(j_rlm,k_rlm,nd+2*nvector) = WRecv(i_rlm_0  )
        end do
!
        ist = nscalar*inod_rlm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          inod = 1 + mod( (inum-1),nnod_rlm)
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
          nd =   1 + (inum - inod) / nnod_rlm
!
          i_rlm_0 = nd + 3*nvector                                      &
     &                 + (irev_rlm_1D(j_rlm,k_rlm) - 1) * ncomp
!
          sp_rlm_spin(j_rlm,k_rlm,nd+3*nvector) = WRecv(i_rlm_0)
        end do
      end do
!$omp end parallel do
!
      end subroutine get_rlm_from_recv_spin
!
! -----------------------------------------------------------------------
!
      subroutine get_rtm_from_recv_spin(nnod_rtm, inod_rtm_smp_stack,   &
     &          ncomp, nvector, nscalar, WRecv, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: inod_rtm_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: WRecv(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(1),ncomp,nidx_rtm(3))
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,i_rtm_0,nd,kr_nd,       &
!$omp&                     k_rtm,l_rtm,m_rtm)
      do ip = 1, np_smp
        ist = nvector*inod_rtm_smp_stack(ip-1) + 1
        ied = nvector*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          l_rtm = 1 + mod((inum-1),nidx_rtm(2))
          inod =  1 + (inum - l_rtm) / nidx_rtm(2)
          m_rtm = 1 + mod( (inod-1),nidx_rtm(3))
          kr_nd = 1 + (inod - m_rtm) / nidx_rtm(3)
          k_rtm = 1 + mod( (kr_nd-1),nidx_rtm(1))
          nd =    1 + (kr_nd-k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = 3*nd                                                &
     &             + (irev_rtm_1D(l_rtm,k_rtm,m_rtm) - 1) * ncomp
!
          vr_rtm_spin(l_rtm,k_rtm,nd,m_rtm)                             &
     &            = WRecv(i_rtm_0-2)
          vr_rtm_spin(l_rtm,k_rtm,nd+nvector,m_rtm)                     &
     &            = WRecv(i_rtm_0-1)
          vr_rtm_spin(l_rtm,k_rtm,nd+2*nvector,m_rtm)                   &
     &            = WRecv(i_rtm_0  )
        end do
!
        ist = nscalar*inod_rtm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          l_rtm = 1 + mod((inum-1),nidx_rtm(2))
          inod =  1 + (inum - l_rtm) / nidx_rtm(2)
          m_rtm = 1 + mod( (inod-1),nidx_rtm(3))
          kr_nd = 1 + (inod - m_rtm) / nidx_rtm(3)
          k_rtm = 1 + mod( (kr_nd-1),nidx_rtm(1))
          nd =    1 + (kr_nd-k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = 3*nd + 3*nvector                                    &
     &             + (irev_rtm_1D(l_rtm,k_rtm,m_rtm) - 1) * ncomp
!
          vr_rtm_spin(l_rtm,k_rtm,nd+3*nvector,m_rtm)                   &
     &            = WRecv(i_rtm_0  )
        end do
      end do
!$omp end parallel do
!
      end subroutine get_rtm_from_recv_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rlm_to_send_spin(nnod_rlm, nidx_rlm,               &
     &          ncomp, nvector, nscalar, sp_rlm_spin, Wsend)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_spin(nidx_rlm(1),ncomp,nidx_rlm(2))
      real(kind = kreal), intent(inout) :: Wsend(ncomp*nnod_rlm)
!
      integer(kind = kint) :: inum, inod, icou
      integer(kind = kint) :: i_rlm_0, nd, j_rlm, k_rlm
!
!
!$omp  parallel do private(inum,inod,nd,i_rlm_0,j_rlm,k_rlm,icou)
      do icou = 1, nvector*istack_sr_rlm(nneib_domain_rlm)
        nd =    1 + mod( (icou-1),nvector)
        inum =  1 + (icou - nd) / nvector
        k_rlm = isend_rlm_1D(inum,1)
        j_rlm = isend_rlm_1D(inum,2)
!
        i_rlm_0 = 3*nd + (icou-1) * ncomp
!
        Wsend(i_rlm_0-2) = sp_rlm_spin(k_rlm,nd,          j_rlm)
        Wsend(i_rlm_0-1) = sp_rlm_spin(k_rlm,nd+nvector,  j_rlm)
        Wsend(i_rlm_0  ) = sp_rlm_spin(k_rlm,nd+2*nvector,j_rlm)
      end do
!$omp end parallel do
!
!$omp  parallel do private(inum,inod,nd,i_rlm_0,j_rlm,k_rlm,icou)
      do icou = 1, nscalar*istack_sr_rlm(nneib_domain_rlm)
        nd =    1 + mod( (icou-1),nscalar)
        inum =  1 + (icou - nd) / nscalar
        k_rlm = isend_rlm_1D(inum,1)
        j_rlm = isend_rlm_1D(inum,2)
!
        i_rlm_0 = nd + 3*nvector + (icou-1) * ncomp
!
        Wsend(i_rlm_0  ) = sp_rlm_spin(k_rlm,nd+3*nvector,j_rlm)
      end do
!$omp end parallel do
!
!
      end subroutine set_rlm_to_send_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rtm_to_send_spin                                   &
     &         (nnod_rtm, ncomp, nvector, nscalar, vr_rtm_spin, Wsend)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_spin(nidx_rtm(1),ncomp,nidx_rtm(3),nidx_rtm(2))
      real(kind = kreal), intent(inout) :: Wsend(ncomp*nnod_rtm)
!
      integer(kind = kint) :: inum, inod, icou
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm, nd
!
!
!$omp parallel do private(i_rtm_0,k_rtm,l_rtm,nd,inod,  &
!$omp&                    m_rtm,inum,icou)
      do icou = 1, nvector*istack_sr_rtm(nneib_domain_rtm)
        nd = 1 + mod(icou-1,nvector)
        inum = 1 + (icou - nd) / nvector
!
        k_rtm = isend_rtm_1D(inum,1)
        l_rtm = isend_rtm_1D(inum,2)
        m_rtm = isend_rtm_1D(inum,3)
!
        i_rtm_0 = 3*nd + (inum-1) * ncomp
!
        Wsend(i_rtm_0-2) = vr_rtm_spin(k_rtm,nd,          m_rtm,l_rtm)
        Wsend(i_rtm_0-1) = vr_rtm_spin(k_rtm,nd+nvector,  m_rtm,l_rtm)
        Wsend(i_rtm_0  ) = vr_rtm_spin(k_rtm,nd+2*nvector,m_rtm,l_rtm)
      end do
!$omp end parallel do
!
!$omp parallel do private(i_rtm_0,k_rtm,l_rtm,nd,inod,  &
!$omp&                    m_rtm,inum,icou)
      do icou = 1, nscalar*istack_sr_rtm(nneib_domain_rtm)
        nd = 1 + mod(icou-1,nscalar)
        inum = 1 + (icou - nd) / nscalar
!
        k_rtm = isend_rtm_1D(inum,1)
        l_rtm = isend_rtm_1D(inum,2)
        m_rtm = isend_rtm_1D(inum,3)
!
        i_rtm_0 = nd + 3*nvector + (inum-1) * ncomp
!
        Wsend(i_rtm_0  ) = vr_rtm_spin(k_rtm,nd+3*nvector,m_rtm,l_rtm)
      end do
!$omp end parallel do
!
      end subroutine set_rtm_to_send_spin
!
! -----------------------------------------------------------------------
!
      end module copy_SR_4_leg_trans_spin

