!>@file   sph_transforms_4_SGS.f90
!!@brief  module sph_transforms_4_SGS
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,      &
!!     &          rj_fld, trns_bwd, WK_leg, WK_FFTs)
!!      subroutine sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,   &
!!     &          trns_fwd, WK_leg, WK_FFTs, rj_fld)
!!      subroutine sph_pole_trans_SGS_MHD                               &
!!     &         (sph, comms_sph, trans_p, rj_fld, trns_bwd)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_bwd
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!@endverbatim
!!
      module sph_transforms_4_SGS
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
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
!
      use m_legendre_transform_list
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,        &
     &          rj_fld, trns_bwd, WK_leg, WK_FFTs)
!
      use m_solver_SR
      use set_address_sph_trans_MHD
      use spherical_transforms
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_data), intent(in) :: rj_fld
!
      type(spherical_transform_data), intent(inout) :: trns_bwd
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
      if(trns_bwd%ncomp .le. 0) return
!
      call check_calypso_sph_comm_buf_N(trns_bwd%ncomp,                 &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_bwd%ncomp,                 &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call mhd_spectr_to_sendbuf                                        &
     &   (trns_bwd, comms_sph%comm_rj, rj_fld, SR_r1%n_WS, SR_r1%WS(1))
!
      call sph_backward_transforms                                      &
     &   (trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar,     &
     &    sph, comms_sph, trans_p,                                      &
     &    SR_r1%n_WS, SR_r1%n_WR, SR_r1%WS(1), SR_r1%WR(1),             &
     &    trns_bwd%fld_rtp, WK_leg, WK_FFTs)
!
!
      end subroutine sph_back_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,     &
     &          trns_fwd, WK_leg, WK_FFTs, rj_fld)
!
      use m_solver_SR
      use set_address_sph_trans_MHD
      use spherical_transforms
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(spherical_transform_data), intent(inout) :: trns_fwd
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(trns_fwd%ncomp .le. 0) return
!
      call check_calypso_sph_comm_buf_N(trns_fwd%ncomp,                 &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N(trns_fwd%ncomp,                 &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
!   transform for vectors and scalars
      call sph_forward_transforms                                       &
     &   (trns_fwd%ncomp, trns_fwd%num_vector, trns_fwd%num_scalar,     &
     &    sph, comms_sph, trans_p, trns_fwd%fld_rtp,                    &
     &    SR_r1%n_WS, SR_r1%n_WR, SR_r1%WS(1), SR_r1%WR(1),             &
     &    WK_leg, WK_FFTs)
!
      call mhd_spectr_from_recvbuf(trans_p%iflag_SPH_recv,              &
     &    trns_fwd, comms_sph%comm_rj, SR_r1%n_WR, SR_r1%WR(1), rj_fld)
!
      end subroutine sph_forward_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_pole_trans_SGS_MHD                                 &
     &         (sph, comms_sph, trans_p, rj_fld, trns_bwd)
!
      use m_solver_SR
      use sph_transforms_4_MHD
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_data), intent(in) :: rj_fld
      type(spherical_transform_data), intent(inout) :: trns_bwd
!
      integer(kind = kint) :: nscalar_trans
!
!
      if(trns_bwd%ncomp .le. 0) return
!
      nscalar_trans = trns_bwd%num_scalar + 6*trns_bwd%num_tensor
      call sph_pole_trans_4_MHD                                         &
     &   (sph, comms_sph, trans_p, rj_fld, trns_bwd)
!
      end subroutine sph_pole_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_SGS
