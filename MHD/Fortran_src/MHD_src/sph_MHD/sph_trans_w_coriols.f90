!>@file   sph_trans_w_coriols.f90
!!@brief  module sph_trans_w_coriols
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform
!!       including Coriolis terms
!!
!!@verbatim
!!      subroutine sph_b_trans_w_coriolis(ncomp_trans, nvector, nscalar,&
!!     &          sph, comms_sph, leg, n_WS, n_WR, WS, WR, trns_MHD)
!!      subroutine sph_f_trans_w_coriolis(ncomp_trans, nvector, nscalar,&
!!     &          sph, comms_sph, leg, trns_MHD, n_WS, n_WR, WS, WR)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!
!!      subroutine sph_b_trans_licv(ncomp_trans,                        &
!!     &          sph_rlm, comm_rlm, comm_rj, leg, trns_MHD, n_WR, WR)
!!      subroutine sph_f_trans_licv(ncomp_trans,                        &
!!     &         sph_rlm, comm_rlm, comm_rj, trns_MHD, n_WS, WS)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!
!!   input /outpt arrays for single vector
!!      radial component:      v_rtp(i_rtp,1)
!!      elevetional component: v_rtp(i_rtp,2)
!!      azimuthal component:   v_rtp(i_rtp,3)
!!
!!      Poloidal component:          WR(3*i_rj-2)
!!      diff. of Poloidal component: WR(3*i_rj-1)
!!      Toroidal component:          WR(3*i_rj  )
!!@endverbatim
!!
!!@param ncomp_trans Number of components for transform
!!@param nvector     Number of vectors for transform
!!@param nscalar     Number of scalars for transform
!!
!!@param n_WS Number of components for send buffrt
!!@param n_WR Number of components for recieve buffer
!!@param WS Send buffer
!!@param WR Recieve buffer
!!@param fld_rtp Field data to make nonlinear terms
!!@param frc_rtp Nonlinear terms
!
      module sph_trans_w_coriols
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_machine_parameter
      use m_work_4_sph_trans
      use MHD_FFT_selector
      use legendre_transform_select
      use spherical_SRs_N
      use const_coriolis_sph_rlm
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_w_coriolis(ncomp_trans, nvector, nscalar,  &
     &          sph, comms_sph, leg, n_WS, n_WR, WS, WR, trns_MHD)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp_trans, nvector, nscalar
!
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      if(iflag_debug .gt. 0) write(*,*) 'calypso_sph_comm_rj_2_rlm_N'
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rj, comms_sph%comm_rlm)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'sum_coriolis_rlm'
      call sum_coriolis_rlm(ncomp_trans,                                &
     &    sph%sph_rlm, comms_sph%comm_rlm, trns_MHD, leg, n_WR, WR)
      call finish_send_recv_sph(comms_sph%comm_rj)
      call end_eleps_time(13)
!
      call start_eleps_time(22)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &    'sel_backward_legendre_trans', ncomp_trans, nvector, nscalar
      call sel_backward_legendre_trans                                  &
     &   (ncomp_trans, nvector, nscalar, sph%sph_rlm, sph%sph_rtm,      &
     &    comms_sph%comm_rlm, comms_sph%comm_rtm, leg,                  &
     &    n_WR, n_WS, WR, WS)
      call end_eleps_time(22)
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &      'calypso_sph_comm_rtm_2_rtp_N'
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rtm, comms_sph%comm_rtp)
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!
      call start_eleps_time(24)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &    'back_MHD_FFT_sel_from_recv', ncomp_trans, nvector, nscalar
      call back_MHD_FFT_sel_from_recv                                   &
     &   (sph%sph_rtp, comms_sph%comm_rtp, ncomp_trans,                 &
     &    n_WR, WR, trns_MHD%fld_rtp)
      call end_eleps_time(24)
!
      if(iflag_debug .gt. 0) write(*,*) 'finish_send_recv_rtm_2_rtp'
      call finish_send_recv_sph(comms_sph%comm_rtm)
!
      end subroutine sph_b_trans_w_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_w_coriolis(ncomp_trans, nvector, nscalar,  &
     &          sph, comms_sph, leg, trns_MHD, n_WS, n_WR, WS, WR)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp_trans, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      call start_eleps_time(24)
      call fwd_MHD_FFT_sel_from_recv                                    &
     &   (sph%sph_rtp, comms_sph%comm_rtp, ncomp_trans,                 &
     &    n_WS, trns_MHD%frc_rtp, WS)
      call end_eleps_time(24)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(20)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call finish_send_recv_sph(comms_sph%comm_rtp)
      call end_eleps_time(20)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(23)
      if(iflag_debug .gt. 0) write(*,*) 'sel_forward_legendre_trans'
      call sel_forward_legendre_trans                                   &
     &   (ncomp_trans, nvector, nscalar, sph%sph_rtm, sph%sph_rlm,      &
     &    comms_sph%comm_rtm, comms_sph%comm_rlm, leg,                  &
     &    n_WR, n_WS, WR, WS)
      call end_eleps_time(23)
!
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      call copy_coriolis_terms_rlm(ncomp_trans,                         &
     &   sph%sph_rlm, comms_sph%comm_rlm, trns_MHD, n_WS, WS)
      call end_eleps_time(13)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call calypso_sph_comm_N                                           &
     &   (ncomp_trans, comms_sph%comm_rlm, comms_sph%comm_rj)
      call finish_send_recv_sph(comms_sph%comm_rlm)
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      end subroutine sph_f_trans_w_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_licv(ncomp_trans,                          &
     &          sph_rlm, comm_rlm, comm_rj, leg, trns_MHD, n_WR, WR)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call calypso_sph_comm_N(ncomp_trans, comm_rj, comm_rlm)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'sum_coriolis_rlm'
      call sum_coriolis_rlm(ncomp_trans,                                &
     &    sph_rlm, comm_rlm, trns_MHD, leg, n_WR, WR)
      call end_eleps_time(13)
!
      call finish_send_recv_sph(comm_rj)
!
      end subroutine sph_b_trans_licv
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_licv(ncomp_trans,                          &
     &         sph_rlm, comm_rlm, comm_rj, trns_MHD, n_WS, WS)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      call start_eleps_time(13)
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      call copy_coriolis_terms_rlm                                      &
     &   (ncomp_trans, sph_rlm, comm_rlm, trns_MHD, n_WS, WS)
      call end_eleps_time(24)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call calypso_sph_comm_N(ncomp_trans, comm_rlm, comm_rj)
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call finish_send_recv_sph(comm_rlm)
!
      end subroutine sph_f_trans_licv
!
! -----------------------------------------------------------------------
!
      end module sph_trans_w_coriols
