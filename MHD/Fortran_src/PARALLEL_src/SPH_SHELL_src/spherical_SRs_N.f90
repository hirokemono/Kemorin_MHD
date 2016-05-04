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
!!      subroutine check_calypso_sph_buffer_N(NB)
!!
!!      subroutine send_recv_sph_trans_N(NB, nnod_send, nnod_recv,      &
!!     &          send_comm, recv_comm, X_send, X_recv)
!!
!!      subroutine check_calypso_sph_comm_buf_N(NB, send_comm, recv_comm)
!!      subroutine calypso_sph_comm_N(NB, send_comm, recv_comm)
!!      subroutine calypso_sph_to_send_N(NB, nnod_org,                  &
!!     &          comm_sph, n_WS, X_org, WS)
!!      subroutine calypso_sph_from_recv_N(NB, nnod_sph,                &
!!     &          comm_sph, n_WR, WR, X_sph)
!!
!!      subroutine finish_send_recv_sph(comm_sph)
!!        type(sph_comm_tbl), intent(in) :: comm_sph
!!@endverbatim
!!
!!
!!@n @param  NB    Number of components for communication
!!@n @param  WR(NB*ntot_recv) Communication buffer for recieving
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
      use m_constants
      use m_work_time
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
      use m_sph_communicators
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sph_communicators
      use m_solver_SR
!
      use m_sel_spherical_SRs
!
      integer (kind=kint), intent(in) :: NB
!
      integer (kind=kint) :: ip, num
      integer (kind=kint) :: nneib_max_send,  nneib_max_recv
      integer (kind=kint) :: nnod_max_send,   nnod_max_recv
      integer (kind=kint) :: nmax_item_rj,  nmax_item_rlm
      integer (kind=kint) :: nmax_item_rtp, nmax_item_rtm
!
!
      nneib_max_send = comm_rtp1%nneib_domain
      nneib_max_recv = comm_rtm1%nneib_domain
      nnod_max_send =  comm_rtp1%ntot_item_sr
      nnod_max_recv =  comm_rtm1%ntot_item_sr
!
      nneib_max_send = max(nneib_max_send,comm_rtm1%nneib_domain)
      nneib_max_recv = max(nneib_max_recv,comm_rtp1%nneib_domain)
      nnod_max_send =  max(nnod_max_send,comm_rtm1%ntot_item_sr)
      nnod_max_recv =  max(nnod_max_recv,comm_rtp1%ntot_item_sr)
!
      nneib_max_send = max(nneib_max_send,comm_rj1%nneib_domain)
      nneib_max_recv = max(nneib_max_recv,comm_rlm1%nneib_domain)
      nnod_max_send =  max(nnod_max_send,comm_rj1%ntot_item_sr)
      nnod_max_recv =  max(nnod_max_recv,comm_rlm1%ntot_item_sr)
!
      nneib_max_send = max(nneib_max_send,comm_rlm1%nneib_domain)
      nneib_max_recv = max(nneib_max_recv,comm_rj1%nneib_domain)
      nnod_max_send =  max(nnod_max_send,comm_rlm1%ntot_item_sr)
      nnod_max_recv =  max(nnod_max_recv,comm_rj1%ntot_item_sr)
!
!      iflag_sph_commN = iflag_send_recv
      if    (iflag_sph_commN .eq. iflag_SR_UNDEFINED                    &
     &  .or. iflag_sph_commN .eq. iflag_alltoall) then
        nmax_item_rtp = comm_rtp1%istack_sr(1) - comm_rtp1%istack_sr(0)
        nmax_item_rtm = comm_rtm1%istack_sr(1) - comm_rtm1%istack_sr(0)
        nmax_item_rlm = comm_rlm1%istack_sr(1) - comm_rlm1%istack_sr(0)
        nmax_item_rj =  comm_rj1%istack_sr(1) -  comm_rj1%istack_sr(0)
        do ip = 2, comm_rtp1%nneib_domain
          num = comm_rtp1%istack_sr(ip) - comm_rtp1%istack_sr(ip-1)
          nmax_item_rtp = max(nmax_item_rtp,num)
!
          num = comm_rtm1%istack_sr(ip) - comm_rtm1%istack_sr(ip-1)
          nmax_item_rtm = max(nmax_item_rtm,num)
        end do
        do ip = 2, comm_rj1%nneib_domain
          num = comm_rlm1%istack_sr(ip) - comm_rlm1%istack_sr(ip-1)
          nmax_item_rlm = max(nmax_item_rlm,num)
!
          num = comm_rj1%istack_sr(ip) - comm_rj1%istack_sr(ip-1)
          nmax_item_rj = max(nmax_item_rj, num)
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
      call check_calypso_sph_buffer_N(NB)
!
      end subroutine buffer_size_sph_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine check_calypso_sph_buffer_N(NB)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
      use m_solver_SR
!
      integer (kind=kint), intent(in) :: NB
!
!
      call check_calypso_sph_comm_buf_N(NB, comm_rtp1, comm_rtm1)
      call check_calypso_sph_comm_buf_N(NB, comm_rtm1, comm_rtp1)
      call check_calypso_sph_comm_buf_N(NB, comm_rj1, comm_rlm1)
      call check_calypso_sph_comm_buf_N(NB, comm_rlm1, comm_rj1)
!
      end subroutine check_calypso_sph_buffer_N
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine send_recv_sph_trans_N(NB, nnod_send, nnod_recv,        &
     &          send_comm, recv_comm, X_send, X_recv)
!
      use m_sel_spherical_SRs
      use m_solver_SR
      use t_sph_trans_comm_tbl
!
      integer (kind=kint), intent(in) :: NB, nnod_send, nnod_recv
      type(sph_comm_tbl), intent(in) :: send_comm
      type(sph_comm_tbl), intent(in) :: recv_comm
      real (kind=kreal), intent(in)::    X_send(NB*nnod_send)
!
      real (kind=kreal), intent(inout):: X_recv(NB*nnod_recv)
!
!
      call check_calypso_sph_comm_buf_N(NB, send_comm, recv_comm)
!
      call calypso_sph_to_send_N(NB, nnod_send,                         &
     &    send_comm, n_WS, X_send, WS)
      call calypso_sph_comm_N(NB, send_comm, recv_comm)
      call calypso_sph_from_recv_N(NB, nnod_recv,                       &
     &    recv_comm, n_WR, WR, X_recv)
!
      call finish_send_recv_sph(send_comm)
!
      end subroutine send_recv_sph_trans_N
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_calypso_sph_comm_buf_N(NB, send_comm, recv_comm)
!
      use m_sel_spherical_SRs
      use m_solver_SR
      use t_sph_trans_comm_tbl
!
      integer (kind=kint), intent(in) :: NB
      type(sph_comm_tbl), intent(in) :: send_comm
      type(sph_comm_tbl), intent(in) :: recv_comm
!
      call check_calypso_sph_buf_N(NB,                                  &
     &    send_comm%nneib_domain, send_comm%istack_sr,                  &
     &    recv_comm%nneib_domain,  recv_comm%istack_sr)
!
      end subroutine check_calypso_sph_comm_buf_N
!
! ----------------------------------------------------------------------
!
      subroutine calypso_sph_comm_N(NB, send_comm, recv_comm)
!
      use m_spheric_parameter
      use m_sel_spherical_SRs
      use t_sph_trans_comm_tbl
!
      integer (kind=kint), intent(in) :: NB
      type(sph_comm_tbl), intent(in) :: send_comm
      type(sph_comm_tbl), intent(in) :: recv_comm
!
!
      call sel_calypso_sph_comm_N(NB,                                   &
     &    send_comm%nneib_domain, send_comm%iflag_self,                 &
     &    send_comm%id_domain, send_comm%istack_sr,                     &
     &    recv_comm%nneib_domain, recv_comm%iflag_self,                 &
     &    recv_comm%id_domain, recv_comm%istack_sr)
!
      end subroutine calypso_sph_comm_N
!
! ----------------------------------------------------------------------
!
      subroutine calypso_sph_to_send_N(NB, nnod_org,                    &
     &          comm_sph, n_WS, X_org, WS)
!
      use t_sph_trans_comm_tbl
      use set_to_send_buffer
!
      type(sph_comm_tbl), intent(in) :: comm_sph
      integer (kind=kint), intent(in) :: NB
      integer (kind=kint), intent(in) :: nnod_org, n_WS
      real (kind=kreal), intent(in) ::   X_org(NB*nnod_org)
      real (kind=kreal), intent(inout):: WS(NB*comm_sph%ntot_item_sr)
!
!
      call start_eleps_time(36)
      call set_to_send_buf_N(NB, nnod_org,                              &
     &    comm_sph%istack_sr(comm_sph%nneib_domain), comm_sph%item_sr,  &
     &    X_org, WS(1))
      call end_eleps_time(36)
!
      end subroutine calypso_sph_to_send_N
!
! ----------------------------------------------------------------------
!
      subroutine calypso_sph_from_recv_N(NB, nnod_sph,                  &
     &          comm_sph, n_WR, WR, X_sph)
!
      use m_sel_spherical_SRs
      use t_sph_trans_comm_tbl
!
      type(sph_comm_tbl), intent(in) :: comm_sph
      integer (kind=kint), intent(in) :: NB, nnod_sph, n_WR
!
      real (kind=kreal), intent(inout) :: WR(n_WR)
      real (kind=kreal), intent(inout):: X_sph(NB*nnod_sph)
!
!
      call sel_calypso_from_recv_N(NB, nnod_sph, n_WR,                  &
     &    comm_sph%nneib_domain, comm_sph%istack_sr,                    &
     &    comm_sph%item_sr, comm_sph%irev_sr, WR, X_sph)
!
      end subroutine calypso_sph_from_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine finish_send_recv_sph(comm_sph)
!
      use m_sel_spherical_SRs
      use t_sph_trans_comm_tbl
!
      type(sph_comm_tbl), intent(in) :: comm_sph
!
!
      call finish_sph_send_recv                                         &
     &   (comm_sph%nneib_domain, comm_sph%iflag_self)
!
      end subroutine finish_send_recv_sph
!
! ----------------------------------------------------------------------
!
      end module spherical_SRs_N
