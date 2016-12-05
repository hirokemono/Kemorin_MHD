!>@file   sph_transforms_snapshot.f90
!!@brief  module sph_transforms_snapshot
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p, &
!!     &          ipol, rj_fld, trns_snap)
!!      subroutine sph_forward_trans_snapshot_MHD                       &
!!     &         (sph, comms_sph, trans_p, trns_snap, ipol, rj_fld)
!!
!!      subroutine sph_forward_trans_tmp_snap_MHD                       &
!!     &         (sph, comms_sph, trans_p, trns_tmp, ipol, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(address_4_sph_trans), intent(in) :: trns_tmp
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
      module sph_transforms_snapshot
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
!
      use legendre_transform_select
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,   &
     &          ipol, rj_fld, trns_snap)
!
      use m_solver_SR
      use sph_transforms
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(address_4_sph_trans), intent(inout) :: trns_snap
!
      integer(kind = kint) :: nscalar_trans
!
!
      if(trns_snap%ncomp_rj_2_rtp .le. 0) return
!
      nscalar_trans = trns_snap%nscalar_rj_2_rtp                        &
     &               + 6*trns_snap%ntensor_rj_2_rtp
      call check_calypso_sph_comm_buf_N(trns_snap%ncomp_rj_2_rtp,       &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_snap%ncomp_rj_2_rtp,       &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'copy_snap_spectr_to_send'
      call copy_snap_spectr_to_send(sph%sph_rtp%nnod_pole,              &
     &    trns_snap%ncomp_rj_2_rtp, trns_snap%b_trns,                   &
     &    sph%sph_rj, comms_sph%comm_rj, ipol, rj_fld,                  &
     &    n_WS, WS, trns_snap%flc_pole)
!
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'copy_snap_spectr_to_send'
      call sph_b_trans_w_poles                                          &
     &   (trns_snap%ncomp_rj_2_rtp, trns_snap%nvector_rj_2_rtp,         &
     &    nscalar_trans, sph, comms_sph, trans_p,                       &
     &    n_WS, n_WR, WS(1), WR(1),                                     &
     &    trns_snap%fld_rtp, trns_snap%flc_pole, trns_snap%fld_pole)
!
      end subroutine sph_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_snapshot_MHD                         &
     &         (sph, comms_sph, trans_p, trns_snap, ipol, rj_fld)
!
      use m_solver_SR
      use sph_transforms
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(trns_snap%ncomp_rtp_2_rj .le. 0) return
!
      call check_calypso_sph_comm_buf_N(trns_snap%ncomp_rtp_2_rj,       &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N(trns_snap%ncomp_rtp_2_rj,       &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
!   transform for vectors and scalars
      call sph_forward_transforms(trns_snap%ncomp_rtp_2_rj,             &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    sph, comms_sph, trans_p, trns_snap%frc_rtp,                   &
     &    n_WS, n_WR, WS(1), WR(1))
!
      call copy_snap_vec_spec_from_trans                                &
     &   (trns_snap%ncomp_rtp_2_rj, trns_snap%f_trns,                   &
     &    comms_sph%comm_rj, ipol, n_WR, WR(1), rj_fld)
!
      end subroutine sph_forward_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_tmp_snap_MHD                         &
     &         (sph, comms_sph, trans_p, trns_tmp, ipol, rj_fld)
!
      use m_solver_SR
      use sph_transforms
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_4_sph_trans), intent(in) :: trns_tmp
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(trns_tmp%ncomp_rtp_2_rj .eq. 0) return
!
      call check_calypso_sph_comm_buf_N(trns_tmp%ncomp_rtp_2_rj,        &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N(trns_tmp%ncomp_rtp_2_rj,        &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
!   transform for vectors and scalars
      call sph_forward_transforms(trns_tmp%ncomp_rtp_2_rj,              &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    sph, comms_sph, trans_p, trns_tmp%frc_rtp,                    &
     &    n_WS, n_WR, WS, WR)
!
      call copy_tmp_scl_spec_from_trans                                 &
     &   (trns_tmp%ncomp_rtp_2_rj, trns_tmp%f_trns,                     &
     &    comms_sph%comm_rj, ipol, n_WR, WR, rj_fld)
!
      end subroutine sph_forward_trans_tmp_snap_MHD
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_snapshot
