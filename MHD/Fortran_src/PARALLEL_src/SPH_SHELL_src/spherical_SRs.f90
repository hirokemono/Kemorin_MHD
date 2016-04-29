!>@file   spherical_SRs.f90
!!@brief  module spherical_SRs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  One component data communications 
!!@n      for spherical harmonics transform
!!
!!@verbatim
!!      subroutine init_sph_send_recv_1(X_rtp, X_rtm, X_rlm, X_rj)
!!
!!      subroutine send_recv_rtp_2_rtm(X_rtp, X_rtm)
!!      subroutine send_recv_rtm_2_rtp(X_rtm, X_rtp)
!!      subroutine send_recv_rj_2_rlm(X_rj, X_rlm)
!!      subroutine send_recv_rlm_2_rj(X_rlm, X_rj)
!!@endverbatim
!!
!!@n @param  X_rtp(nnod_rtp)  @f$ f(r,\theta,\phi) @f$
!!@n @param  X_rtm(nnod_rtm)  @f$ f(r,\theta,m) @f$
!!@n @param  X_rlm(nnod_rlm)  @f$ f(r,l,m) @f$
!!@n @param  X_rj(nnod_rj)    @f$ f(r,j) @f$
!
      module spherical_SRs
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use select_copy_from_recv
!
      use calypso_SR
      use m_solver_SR
!
      implicit none
!
!>      Data communication mode for scalar
      integer(kind = kint) :: iflag_sph_SR =  iflag_import_item
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_sph_send_recv_1(X_rtp, X_rtm, X_rlm, X_rj)
!
      use calypso_mpi
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      real (kind=kreal), intent(inout) :: X_rtp(nnod_rtp)
      real (kind=kreal), intent(inout) :: X_rtm(nnod_rtm)
      real (kind=kreal), intent(inout) :: X_rlm(nnod_rlm)
      real (kind=kreal), intent(inout)::  X_rj(nnod_rj)
!
      integer (kind=kint) :: nneib_max_send, nneib_max_recv
      integer (kind=kint) :: nnod_max_send,  nnod_max_recv
      integer (kind=kint), parameter :: NB = 1
!
      real(kind = kreal) :: starttime, endtime
      real(kind = kreal) :: etime_item_import, etime_irev_import
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
      call resize_work_4_SR(NB, nneib_max_send, nneib_max_recv,         &
     &    comm_rtp1%ntot_item_sr, comm_rtm1%ntot_item_sr)
!
!
!
      iflag_sph_SR = iflag_import_item
      starttime = MPI_WTIME()
      call send_recv_rtp_2_rtm(X_rtp, X_rtm)
      call send_recv_rtm_2_rtp(X_rtm, X_rtp)
      call send_recv_rj_2_rlm(X_rj, X_rlm)
      call send_recv_rlm_2_rj(X_rlm, X_rj)
!
      endtime = MPI_WTIME() - starttime
      call MPI_allREDUCE (endtime, etime_item_import, ione,             &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      iflag_sph_SR = iflag_import_rev
      starttime = MPI_WTIME()
      call send_recv_rtp_2_rtm(X_rtp, X_rtm)
      call send_recv_rtm_2_rtp(X_rtm, X_rtp)
      call send_recv_rj_2_rlm(X_rj, X_rlm)
      call send_recv_rlm_2_rj(X_rlm, X_rj)
!
      endtime = MPI_WTIME() - starttime
      call MPI_allREDUCE (endtime, etime_irev_import, ione,             &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if(etime_irev_import .le. etime_item_import) then
        iflag_sph_SR = iflag_import_rev
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) 'SPH_SR_scalar_mode: ', iflag_sph_SR
        write(*,*) '0: Time by reg. import list: ', etime_item_import
        write(*,*) '1: Time by rev. import list: ', etime_irev_import
      end if
!
!
      end subroutine init_sph_send_recv_1
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine send_recv_rtp_2_rtm(X_rtp, X_rtm)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      real (kind=kreal), intent(in)::    X_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: X_rtm(nnod_rtm)
!
!
      call calypso_send_recv(iflag_sph_SR, nnod_rtp, nnod_rtm,          &
     &    comm_rtp1%nneib_domain, comm_rtp1%iflag_self,                 &
     &    comm_rtp1%id_domain, comm_rtp1%istack_sr, comm_rtp1%item_sr,  &
     &    comm_rtm1%nneib_domain, comm_rtm1%iflag_self,                 &
     &    comm_rtm1%id_domain, comm_rtm1%istack_sr, comm_rtm1%item_sr,  &
     &    comm_rtm1%irev_sr, X_rtp, X_rtm)
!
      end subroutine send_recv_rtp_2_rtm
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rtm_2_rtp(X_rtm, X_rtp)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      real (kind=kreal), intent(in)::    X_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: X_rtp(nnod_rtp)
!
!
      call calypso_send_recv(iflag_sph_SR, nnod_rtm, nnod_rtp,          &
     &    comm_rtm1%nneib_domain, comm_rtm1%iflag_self,                 &
     &    comm_rtm1%id_domain, comm_rtm1%istack_sr, comm_rtm1%item_sr,  &
     &    comm_rtp1%nneib_domain, comm_rtp1%iflag_self,                 &
     &    comm_rtp1%id_domain, comm_rtp1%istack_sr, comm_rtp1%item_sr,  &
     &    comm_rtp1%irev_sr, X_rtm, X_rtp)
!
      end subroutine send_recv_rtm_2_rtp
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rj_2_rlm(X_rj, X_rlm)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      real (kind=kreal), intent(in)::    X_rj(nnod_rj)
      real (kind=kreal), intent(inout):: X_rlm(nnod_rlm)
!
!
      call calypso_send_recv(iflag_sph_SR, nnod_rj, nnod_rlm,           &
     &    comm_rj1%nneib_domain, comm_rj1%iflag_self,                   &
     &    comm_rj1%id_domain, comm_rj1%istack_sr, comm_rj1%item_sr,     &
     &    comm_rlm1%nneib_domain, comm_rlm1%iflag_self,                 &
     &    comm_rlm1%id_domain, comm_rlm1%istack_sr, comm_rlm1%item_sr,  &
     &    comm_rlm1%irev_sr, X_rj, X_rlm)
!
      end subroutine send_recv_rj_2_rlm
!
! ----------------------------------------------------------------------
!
      subroutine send_recv_rlm_2_rj(X_rlm, X_rj)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      real (kind=kreal), intent(in)::    X_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: X_rj(nnod_rj)
!
!
      call calypso_send_recv(iflag_sph_SR, nnod_rlm, nnod_rj,           &
     &    comm_rlm1%nneib_domain, comm_rlm1%iflag_self,                 &
     &    comm_rlm1%id_domain, comm_rlm1%istack_sr, comm_rlm1%item_sr,  &
     &    comm_rj1%nneib_domain, comm_rj1%iflag_self,                   &
     &    comm_rj1%id_domain, comm_rj1%istack_sr, comm_rj1%item_sr,     &
     &    comm_rj1%irev_sr, X_rlm, X_rj)
!
      end subroutine send_recv_rlm_2_rj
!
! ----------------------------------------------------------------------
!
      end module spherical_SRs
