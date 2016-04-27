!>@file   copy_SR_4_leg_trans_fldout.f90
!!@brief  module copy_SR_4_leg_trans_fldout
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine get_rlm_from_recv_fldout                             &
!!     &         (nnod_rlm, inod_rlm_smp_stack, irev_sr_rlm,            &
!!     &          ncomp, WRecv, sp_rlm_fdout)
!!      subroutine get_rtm_from_recv_fldout                             &
!!     &         (nnod_rtm, inod_rtm_smp_stack, irev_sr_rtm,            &
!!     &          ncomp, WRecv, vr_rtm_fdout)
!!
!!      subroutine set_rlm_to_send_fldout(nnod_rlm, nneib_domain_rlm,   &
!!     &          ntot_item_sr_rlm, istack_sr_rlm, item_sr_rlm,         &
!!     &          ncomp, sp_rlm_fdout, Wsend)
!!      subroutine set_rtm_to_send_fldout(nnod_rtm, nneib_domain_rtm,   &
!!     &          ntot_item_sr_rtm, istack_sr_rtm, item_sr_rtm,         &
!!     &          ncomp, vr_rtm_fdout, Wsend)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module copy_SR_4_leg_trans_fldout
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine get_rlm_from_recv_fldout                               &
     &         (nnod_rlm, inod_rlm_smp_stack, irev_sr_rlm,              &
     &          ncomp, WRecv, sp_rlm_fdout)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: inod_rlm_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: WRecv(ncomp*nnod_rlm)
      real(kind = kreal), intent(inout) :: sp_rlm_fdout(nnod_rlm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inod, nd, i_rlm_0
!
!
!$omp parallel private(nd)
      do nd = 1, ncomp
!$omp do private(ip,ist,ied,inod,i_rlm_0)
        do ip = 1, np_smp
          ist = inod_rlm_smp_stack(ip-1) + 1
          ied = inod_rlm_smp_stack(ip)
          do inod = ist, ied
            i_rlm_0 = nd + (irev_sr_rlm(inod) - 1) * ncomp
!
            sp_rlm_fdout(inod,nd) = WRecv(i_rlm_0)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine get_rlm_from_recv_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine get_rtm_from_recv_fldout                               &
     &         (nnod_rtm, inod_rtm_smp_stack, irev_sr_rtm,              &
     &          ncomp, WRecv, vr_rtm_fdout)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: inod_rtm_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: WRecv(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout) :: vr_rtm_fdout(nnod_rtm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inod, i_rtm_0, nd
!
!
!$omp parallel private(nd)
      do nd = 1, ncomp
!$omp do private(ist,ied,i_rtm_0,inod)
        do ip = 1, np_smp
          ist = inod_rtm_smp_stack(ip-1) + 1
          ied = inod_rtm_smp_stack(ip)
          do inod = ist, ied
            i_rtm_0 = nd + (irev_sr_rtm(inod) - 1) * ncomp
!
            vr_rtm_fdout(inod,nd) = WRecv(i_rtm_0)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine get_rtm_from_recv_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rlm_to_send_fldout(nnod_rlm, nneib_domain_rlm,     &
     &          ntot_item_sr_rlm, istack_sr_rlm, item_sr_rlm,           &
     &          ncomp, sp_rlm_fdout, Wsend)
!
      integer(kind = kint), intent(in) :: nneib_domain_rlm
      integer(kind = kint), intent(in) :: ntot_item_sr_rlm
      integer(kind = kint), intent(in)                                  &
     &              :: istack_sr_rlm(0:nneib_domain_rlm)
      integer(kind = kint), intent(in) :: item_sr_rlm(ntot_item_sr_rlm)
!
      integer(kind = kint), intent(in) :: nnod_rlm, ncomp
      real(kind = kreal), intent(in) :: sp_rlm_fdout(nnod_rlm,ncomp)
      real(kind = kreal), intent(inout) :: Wsend(ncomp*nnod_rlm)
!
      integer(kind = kint) :: icou, inum, inod, nd
!
!
!$omp  parallel do private(icou,inum,inod,nd)
      do icou = 1, ncomp*istack_sr_rlm(nneib_domain_rlm)
        nd =    1 + mod( (icou-1),ncomp)
        inum =  1 + (icou - nd) / ncomp
        inod = item_sr_rlm(inum)
!
        Wsend(icou) = sp_rlm_fdout(inod,nd)
      end do
!$omp end parallel do
!
      end subroutine set_rlm_to_send_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rtm_to_send_fldout(nnod_rtm, nneib_domain_rtm,     &
     &          ntot_item_sr_rtm, istack_sr_rtm, item_sr_rtm,           &
     &          ncomp, vr_rtm_fdout, Wsend)
!
      integer(kind = kint), intent(in) :: nneib_domain_rtm
      integer(kind = kint), intent(in) :: ntot_item_sr_rtm
      integer(kind = kint), intent(in)                                  &
     &              :: istack_sr_rtm(0:nneib_domain_rtm)
      integer(kind = kint), intent(in) :: item_sr_rtm(ntot_item_sr_rtm)
!
      integer(kind = kint), intent(in) :: nnod_rtm, ncomp
      real(kind = kreal), intent(in) :: vr_rtm_fdout(nnod_rtm,ncomp)
      real(kind = kreal), intent(inout) :: Wsend(ncomp*nnod_rtm)
!
      integer(kind = kint) :: icou, inum, inod, nd
!
!
!$omp  parallel do private(icou,inum,inod,nd)
      do icou = 1, ncomp*istack_sr_rtm(nneib_domain_rtm)
        nd =    1 + mod( (icou-1),ncomp)
        inum =  1 + (icou - nd) / ncomp
        inod = item_sr_rtm(inum)
!
        Wsend(icou)  = vr_rtm_fdout(inod,nd)
      end do
!$omp end parallel do
!
      end subroutine set_rtm_to_send_fldout
!
! -----------------------------------------------------------------------
!
      end module copy_SR_4_leg_trans_fldout
