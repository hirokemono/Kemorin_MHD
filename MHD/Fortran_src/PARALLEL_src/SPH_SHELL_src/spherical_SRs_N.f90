!>@file   spherical_SRs_N.f90
!!@brief  module spherical_SRs_N
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communications 
!!@n      for spherical harmonics transform
!!
!!@verbatim
!!      subroutine buffer_size_sph_send_recv(NB)
!!
!!      subroutine send_recv_rtp_2_rtm_N(NB, X_rtp, X_rtm)
!!      subroutine send_recv_rtm_2_rtp_N(NB, X_rtm, X_rtp)
!!      subroutine send_recv_rj_2_rlm_N(NB, X_rj, X_rlm)
!!      subroutine send_recv_rlm_2_rj_N(NB, X_rlm, X_rj)
!!
!!      subroutine finish_send_recv_rtp_2_rtm
!!      subroutine finish_send_recv_rtm_2_rtp
!!      subroutine finish_send_recv_rj_2_rlm
!!      subroutine finish_send_recv_rlm_2_rj
!!@endverbatim
!!
!!
!!@n @param  NB    Number of components for communication
!!@n @param  X_rtp(NB*nnod_rtp)  @f$ f(r,\theta,\phi) @f$
!!@n               (Order, X_rtp(i_comp,inod))
!!@n @param  X_rtm(NB*nnod_rtm)  @f$ f(r,\theta,m) @f$
!!@n               (Order, X_rtm(i_comp,inod))
!!@n @param  X_rlm(NB*nnod_rlm)  @f$ f(r,l,m) @f$
!!@n               (Order, X_rlm(i_comp,inod))
!!@n @param  X_rj(NB*nnod_rj)    @f$ f(r,j) @f$
!!@n               (Order, X_rj(i_comp,inod))
!
!
      module spherical_SRs_N
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_solver_SR
!
      use m_sel_spherical_SRs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine buffer_size_sph_send_recv(NB)
!
      use calypso_mpi
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sph_communicators
!
      integer (kind=kint), intent(in) :: NB
!
      integer (kind=kint) :: ip
      integer (kind=kint) :: nneib_max_send,  nneib_max_recv
      integer (kind=kint) :: nnod_max_send,   nnod_max_recv
      integer (kind=kint) :: nmax_item_rj,  nmax_item_rlm
      integer (kind=kint) :: nmax_item_rtp, nmax_item_rtm
!
!
      nneib_max_send = nneib_domain_rtp
      nneib_max_recv = nneib_domain_rtm
      nnod_max_send =  ntot_item_sr_rtp
      nnod_max_recv =  ntot_item_sr_rtm
!
      nneib_max_send = max(nneib_max_send,nneib_domain_rtm)
      nneib_max_recv = max(nneib_max_recv,nneib_domain_rtp)
      nnod_max_send =  max(nnod_max_send,ntot_item_sr_rtm)
      nnod_max_recv =  max(nnod_max_recv,ntot_item_sr_rtp)
!
      nneib_max_send = max(nneib_max_send,nneib_domain_rj)
      nneib_max_recv = max(nneib_max_recv,nneib_domain_rlm)
      nnod_max_send =  max(nnod_max_send,ntot_item_sr_rj)
      nnod_max_recv =  max(nnod_max_recv,ntot_item_sr_rlm)
!
      nneib_max_send = max(nneib_max_send,nneib_domain_rlm)
      nneib_max_recv = max(nneib_max_recv,nneib_domain_rj)
      nnod_max_send =  max(nnod_max_send,ntot_item_sr_rlm)
      nnod_max_recv =  max(nnod_max_recv,ntot_item_sr_rj)
!
!      iflag_sph_commN = iflag_send_recv
      if    (iflag_sph_commN .eq. iflag_SR_UNDEFINED                    &
     &  .or. iflag_sph_commN .eq. iflag_alltoall) then
        nmax_item_rtp = istack_sr_rtp(1) - istack_sr_rtp(0)
        nmax_item_rtm = istack_sr_rtm(1) - istack_sr_rtm(0)
        nmax_item_rlm = istack_sr_rlm(1) - istack_sr_rlm(0)
        nmax_item_rj =  istack_sr_rj(1) -  istack_sr_rj(0)
        do ip = 2, nneib_domain_rtp
          nmax_item_rtp                                                 &
     &     = max(nmax_item_rtp,istack_sr_rtp(ip) - istack_sr_rtp(ip-1))
          nmax_item_rtm                                                 &
     &     = max(nmax_item_rtm,istack_sr_rtm(ip) - istack_sr_rtm(ip-1))
        end do
        do ip = 2, nneib_domain_rj
          nmax_item_rlm                                                 &
     &     = max(nmax_item_rlm,istack_sr_rlm(ip) - istack_sr_rlm(ip-1))
          nmax_item_rj                                                  &
     &     = max(nmax_item_rj, istack_sr_rj(ip) -  istack_sr_rj(ip-1) )
        end do
!
        nmax_item_rtp = max(nmax_item_rtp,nmax_item_rtm)
        nmax_item_rj = max(nmax_item_rj,  nmax_item_rlm)
        call MPI_allREDUCE (nmax_item_rtp, nmax_sr_rtp, ione,           &
     &    CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
        call MPI_allREDUCE (nmax_item_rj, nmax_sr_rj, ione,             &
     &    CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      end if
!
      call resize_work_sph_SR(NB, nneib_max_send, nneib_max_recv,       &
     &    ntot_item_sr_rtp, ntot_item_sr_rtm)
!
      end subroutine buffer_size_sph_send_recv
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine send_recv_rtp_2_rtm_N(NB, X_rtp, X_rtm)
!
      use m_sph_communicators
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer (kind=kint), intent(in) :: NB
      real (kind=kreal), intent(in)::    X_rtp(NB*nnod_rtp)
      real (kind=kreal), intent(inout):: X_rtm(NB*nnod_rtm)
!
!
!      iflag_sph_commN = iflag_send_recv
      call sel_calypso_sph_send_recv_N                                  &
     &             (NB, nnod_rtp, nnod_rtm, nmax_sr_rtp,                &
     &              nneib_domain_rtp, iflag_self_rtp,                   &
     &              id_domain_rtp, istack_sr_rtp, item_sr_rtp,          &
     &              nneib_domain_rtm, iflag_self_rtm,                   &
     &              id_domain_rtm, istack_sr_rtm, item_sr_rtm,          &
     &              irev_sr_rtm, X_rtp, X_rtm, CALYPSO_RTP_COMM)
!
      end subroutine send_recv_rtp_2_rtm_N
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rtm_2_rtp_N(NB, X_rtm, X_rtp)
!
      use m_sph_communicators
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer (kind=kint), intent(in) :: NB
      real (kind=kreal), intent(in)::    X_rtm(NB*nnod_rtm)
      real (kind=kreal), intent(inout):: X_rtp(NB*nnod_rtp)
!
!
!      iflag_sph_commN = iflag_send_recv
      call sel_calypso_sph_send_recv_N                                  &
     &             (NB, nnod_rtm, nnod_rtp, nmax_sr_rtp,                &
     &              nneib_domain_rtm, iflag_self_rtm,                   &
     &              id_domain_rtm, istack_sr_rtm, item_sr_rtm,          &
     &              nneib_domain_rtp, iflag_self_rtp,                   &
     &              id_domain_rtp, istack_sr_rtp, item_sr_rtp,          &
     &              irev_sr_rtp, X_rtm, X_rtp, CALYPSO_RTP_COMM)
!
      end subroutine send_recv_rtm_2_rtp_N
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rj_2_rlm_N(NB, X_rj, X_rlm)
!
      use m_sph_communicators
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer (kind=kint), intent(in) :: NB
      real (kind=kreal), intent(in)::    X_rj(NB*nnod_rj)
      real (kind=kreal), intent(inout):: X_rlm(NB*nnod_rlm)
!
!
!      iflag_sph_commN = iflag_send_recv
      call sel_calypso_sph_send_recv_N                                  &
     &             (NB, nnod_rj, nnod_rlm, nmax_sr_rj,                  &
     &              nneib_domain_rj, iflag_self_rj,                     &
     &              id_domain_rj, istack_sr_rj, item_sr_rj,             &
     &              nneib_domain_rlm, iflag_self_rlm,                   &
     &              id_domain_rlm, istack_sr_rlm, item_sr_rlm,          &
     &              irev_sr_rlm, X_rj, X_rlm, CALYPSO_RJ_COMM)
!
      end subroutine send_recv_rj_2_rlm_N
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rlm_2_rj_N(NB, X_rlm, X_rj)
!
      use m_sph_communicators
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer (kind=kint), intent(in) :: NB
      real (kind=kreal), intent(in)::    X_rlm(NB*nnod_rlm)
      real (kind=kreal), intent(inout):: X_rj(NB*nnod_rj)
!
!
!      iflag_sph_commN = iflag_send_recv
      call sel_calypso_sph_send_recv_N                                  &
     &             (NB, nnod_rlm, nnod_rj,  nmax_sr_rj,                 &
     &              nneib_domain_rlm, iflag_self_rlm,                   &
     &              id_domain_rlm, istack_sr_rlm, item_sr_rlm,          &
     &              nneib_domain_rj, iflag_self_rj,                     &
     &              id_domain_rj, istack_sr_rj, item_sr_rj,             &
     &              irev_sr_rj, X_rlm, X_rj, CALYPSO_RJ_COMM)
!
      end subroutine send_recv_rlm_2_rj_N
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine finish_send_recv_rtp_2_rtm
!
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      call finish_sph_send_recv(nneib_domain_rtp, iflag_self_rtp)
!
      end subroutine finish_send_recv_rtp_2_rtm
!
! ----------------------------------------------------------------------
!
      subroutine finish_send_recv_rtm_2_rtp
!
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      call finish_sph_send_recv(nneib_domain_rtm, iflag_self_rtm)
!
      end subroutine finish_send_recv_rtm_2_rtp
!
! ----------------------------------------------------------------------
!
      subroutine finish_send_recv_rj_2_rlm
!
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      call finish_sph_send_recv(nneib_domain_rj, iflag_self_rj)
!
      end subroutine finish_send_recv_rj_2_rlm
!
! ----------------------------------------------------------------------
!
      subroutine finish_send_recv_rlm_2_rj
!
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      call finish_sph_send_recv(nneib_domain_rlm, iflag_self_rlm)
!
      end subroutine finish_send_recv_rlm_2_rj
!
! ----------------------------------------------------------------------
!
      end module spherical_SRs_N
