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
!!      subroutine send_recv_sph_trans_N                                &
!!     &         (iflag_recv, NB, nnod_send, nnod_recv,                 &
!!     &          send_comm, recv_comm, X_send, X_recv)
!!      subroutine send_recv_sph_trans_6                                &
!!     &         (iflag_recv, nnod_send, nnod_recv,                     &
!!     &          send_comm, recv_comm, X_send, X_recv)
!!      subroutine send_recv_sph_trans_3                                &
!!     &         (iflag_recv, nnod_send, nnod_recv,                     &
!!     &          send_comm, recv_comm, X_send, X_recv)
!!      subroutine send_recv_sph_trans_2                                &
!!     &         (iflag_recv, nnod_send, nnod_recv,                     &
!!     &          send_comm, recv_comm, X_send, X_recv)
!!      subroutine send_recv_sph_trans                                  &
!!     &         (iflag_recv, nnod_send, nnod_recv,                     &
!!     &          send_comm, recv_comm, X_send, X_recv)
!!      subroutine send_recv_sph_trans_int                              &
!!     &         (iflag_recv, nnod_send, nnod_recv,                     &
!!     &          send_comm, recv_comm, iX_send, iX_recv)
!!
!!      subroutine check_calypso_sph_comm_buf_N(NB, send_comm, recv_comm)
!!      subroutine calypso_sph_comm_N(NB, send_comm, recv_comm)
!!      subroutine calypso_sph_to_send_N(NB, nnod_org,                  &
!!     &          comm_sph, n_WS, X_org, WS)
!!      subroutine calypso_sph_from_recv_N(iflag_recv, NB, nnod_sph,    &
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
      use t_sph_trans_comm_tbl
      use t_solver_SR
      use m_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_sph_trans_N                                  &
     &         (iflag_recv, NB, nnod_send, nnod_recv,                   &
     &          send_comm, recv_comm, X_send, X_recv)
!
      use sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: iflag_recv
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
     &    send_comm, SR_r1%n_WS, X_send, SR_r1%WS)
      call calypso_sph_comm_N(NB, send_comm, recv_comm)
      call calypso_sph_from_recv_N(iflag_recv, NB, nnod_recv,           &
     &    recv_comm, SR_r1%n_WR, SR_r1%WR, X_recv)
!
      call finish_send_recv_sph(send_comm)
!
      end subroutine send_recv_sph_trans_N
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_sph_trans_6                                  &
     &         (iflag_recv, nnod_send, nnod_recv,                       &
     &          send_comm, recv_comm, X_send, X_recv)
!
      use sel_spherical_SRs
      use calypso_SR_6
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer (kind=kint), intent(in) :: nnod_send, nnod_recv
      type(sph_comm_tbl), intent(in) :: send_comm
      type(sph_comm_tbl), intent(in) :: recv_comm
      real (kind=kreal), intent(in)::    X_send(isix*nnod_send)
      real (kind=kreal), intent(inout):: X_recv(isix*nnod_recv)
!
!
      call calypso_send_recv_6(iflag_recv, nnod_send, nnod_recv,        &
     &    send_comm%nneib_domain, send_comm%iflag_self,                 &
     &    send_comm%id_domain, send_comm%istack_sr, send_comm%item_sr,  &
     &    recv_comm%nneib_domain, recv_comm%iflag_self,                 &
     &    recv_comm%id_domain, recv_comm%istack_sr, recv_comm%item_sr,  &
     &    recv_comm%irev_sr, SR_sig1, SR_r1, X_send, X_recv)
!
      end subroutine send_recv_sph_trans_6
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_sph_trans_3                                  &
     &         (iflag_recv, nnod_send, nnod_recv,                       &
     &          send_comm, recv_comm, X_send, X_recv)
!
      use sel_spherical_SRs
      use calypso_SR_3
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer (kind=kint), intent(in) :: nnod_send, nnod_recv
      type(sph_comm_tbl), intent(in) :: send_comm
      type(sph_comm_tbl), intent(in) :: recv_comm
      real (kind=kreal), intent(in)::    X_send(ithree*nnod_send)
      real (kind=kreal), intent(inout):: X_recv(ithree*nnod_recv)
!
!
      call calypso_send_recv_3(iflag_recv, nnod_send, nnod_recv,        &
     &    send_comm%nneib_domain, send_comm%iflag_self,                 &
     &    send_comm%id_domain, send_comm%istack_sr, send_comm%item_sr,  &
     &    recv_comm%nneib_domain, recv_comm%iflag_self,                 &
     &    recv_comm%id_domain, recv_comm%istack_sr, recv_comm%item_sr,  &
     &    recv_comm%irev_sr, SR_sig1, SR_r1, X_send, X_recv)
!
      end subroutine send_recv_sph_trans_3
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_sph_trans_2                                  &
     &         (iflag_recv, nnod_send, nnod_recv,                       &
     &          send_comm, recv_comm, X_send, X_recv)
!
      use sel_spherical_SRs
      use calypso_SR_2
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer (kind=kint), intent(in) :: nnod_send, nnod_recv
      type(sph_comm_tbl), intent(in) :: send_comm
      type(sph_comm_tbl), intent(in) :: recv_comm
      real (kind=kreal), intent(in)::    X_send(ithree*nnod_send)
      real (kind=kreal), intent(inout):: X_recv(ithree*nnod_recv)
!
!
      call calypso_send_recv_2(iflag_recv, nnod_send, nnod_recv,        &
     &    send_comm%nneib_domain, send_comm%iflag_self,                 &
     &    send_comm%id_domain, send_comm%istack_sr, send_comm%item_sr,  &
     &    recv_comm%nneib_domain, recv_comm%iflag_self,                 &
     &    recv_comm%id_domain, recv_comm%istack_sr, recv_comm%item_sr,  &
     &    recv_comm%irev_sr, SR_sig1, SR_r1, X_send, X_recv)
!
      end subroutine send_recv_sph_trans_2
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_sph_trans                                    &
     &         (iflag_recv, nnod_send, nnod_recv,                       &
     &          send_comm, recv_comm, X_send, X_recv)
!
      use sel_spherical_SRs
      use calypso_SR
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer (kind=kint), intent(in) :: nnod_send, nnod_recv
      type(sph_comm_tbl), intent(in) :: send_comm
      type(sph_comm_tbl), intent(in) :: recv_comm
      real (kind=kreal), intent(in)::    X_send(nnod_send)
      real (kind=kreal), intent(inout):: X_recv(nnod_recv)
!
!
      call calypso_send_recv(iflag_recv, nnod_send, nnod_recv,          &
     &    send_comm%nneib_domain, send_comm%iflag_self,                 &
     &    send_comm%id_domain, send_comm%istack_sr, send_comm%item_sr,  &
     &    recv_comm%nneib_domain, recv_comm%iflag_self,                 &
     &    recv_comm%id_domain, recv_comm%istack_sr, recv_comm%item_sr,  &
     &    recv_comm%irev_sr, SR_sig1, SR_r1, X_send, X_recv)
!
      end subroutine send_recv_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_sph_trans_int                                &
     &         (iflag_recv, nnod_send, nnod_recv,                       &
     &          send_comm, recv_comm, iX_send, iX_recv)
!
      use sel_spherical_SRs
      use calypso_SR_int
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer (kind=kint), intent(in) :: nnod_send, nnod_recv
      type(sph_comm_tbl), intent(in) :: send_comm
      type(sph_comm_tbl), intent(in) :: recv_comm
      integer (kind=kint), intent(in)::    iX_send(nnod_send)
      integer (kind=kint), intent(inout):: iX_recv(nnod_recv)
!
!
      call calypso_send_recv_int(iflag_recv, nnod_send, nnod_recv,      &
     &    send_comm%nneib_domain, send_comm%iflag_self,                 &
     &    send_comm%id_domain, send_comm%istack_sr, send_comm%item_sr,  &
     &    recv_comm%nneib_domain, recv_comm%iflag_self,                 &
     &    recv_comm%id_domain, recv_comm%istack_sr, recv_comm%item_sr,  &
     &    recv_comm%irev_sr, SR_sig1, SR_i1, iX_send, iX_recv)
!
      end subroutine send_recv_sph_trans_int
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_calypso_sph_comm_buf_N(NB, send_comm, recv_comm)
!
      use sel_spherical_SRs
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
      use sel_spherical_SRs
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
      use m_elapsed_labels_SEND_RECV
      use set_to_send_buffer
!
      type(sph_comm_tbl), intent(in) :: comm_sph
      integer (kind=kint), intent(in) :: NB
      integer (kind=kint), intent(in) :: nnod_org, n_WS
      real (kind=kreal), intent(in) ::   X_org(NB*nnod_org)
      real (kind=kreal), intent(inout):: WS(NB*comm_sph%ntot_item_sr)
!
!
      if(iflag_CSR_time) call start_elapsed_time(ist_elapsed_CSR+1)
      call set_to_send_buf_N(NB, nnod_org,                              &
     &    comm_sph%istack_sr(comm_sph%nneib_domain), comm_sph%item_sr,  &
     &    X_org, WS(1))
      if(iflag_CSR_time) call end_elapsed_time(ist_elapsed_CSR+1)
!
      end subroutine calypso_sph_to_send_N
!
! ----------------------------------------------------------------------
!
      subroutine calypso_sph_from_recv_N(iflag_recv, NB, nnod_sph,      &
     &          comm_sph, n_WR, WR, X_sph)
!
      use sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(sph_comm_tbl), intent(in) :: comm_sph
      integer (kind=kint), intent(in) :: NB, nnod_sph, n_WR
!
      real (kind=kreal), intent(inout) :: WR(n_WR)
      real (kind=kreal), intent(inout):: X_sph(NB*nnod_sph)
!
!
      call sel_calypso_from_recv_N(iflag_recv, NB, nnod_sph, n_WR,      &
     &    comm_sph%nneib_domain, comm_sph%istack_sr,                    &
     &    comm_sph%item_sr, comm_sph%irev_sr, WR, X_sph)
!
      end subroutine calypso_sph_from_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine finish_send_recv_sph(comm_sph)
!
      use sel_spherical_SRs
!
      type(sph_comm_tbl), intent(in) :: comm_sph
!
!
      call calypso_send_recv_fin                                        &
     &   (comm_sph%nneib_domain, comm_sph%iflag_self, SR_sig1)
!
      end subroutine finish_send_recv_sph
!
! ----------------------------------------------------------------------
!
      end module spherical_SRs_N
