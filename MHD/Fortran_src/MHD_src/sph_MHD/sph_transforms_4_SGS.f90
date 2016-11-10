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
!!     &          ipol, rj_fld, trns_SGS, SGS_mul_FFTW)
!!      subroutine sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,   &
!!     &          ipol, trns_SGS, SGS_mul_FFTW, rj_fld)
!!      subroutine sph_pole_trans_SGS_MHD(sph, comms_sph, trans_p,      &
!!     &          ipol, rj_fld, trns_SGS)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
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
      subroutine sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,        &
     &          ipol, rj_fld, trns_SGS, SGS_mul_FFTW)
!
      use m_solver_SR
      use sph_trans_w_coriols
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
!
!
      if(trns_SGS%ncomp_rj_2_rtp .le. 0) return
!
      call check_calypso_sph_comm_buf_N(trns_SGS%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_SGS%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call copy_SGS_spectr_to_send(sph%sph_rtp%nnod_pole,               &
     &    trns_SGS%ncomp_rj_2_rtp, trns_SGS%b_trns,                     &
     &    sph%sph_rj, comms_sph%comm_rj, ipol, rj_fld, n_WS, WS)
!
      call sph_b_transform_SGS                                          &
     &   (trns_SGS%ncomp_rj_2_rtp, trns_SGS%nvector_rj_2_rtp,           &
     &    trns_SGS%ntensor_rj_2_rtp, sph, comms_sph, trans_p,           &
     &    n_WS, n_WR, WS(1), WR(1), trns_SGS, SGS_mul_FFTW)
!
      end subroutine sph_back_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,     &
     &          ipol, trns_SGS, SGS_mul_FFTW, rj_fld)
!
      use m_solver_SR
      use sph_trans_w_coriols
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(trns_SGS%ncomp_rtp_2_rj .le. 0) return
!
      call check_calypso_sph_comm_buf_N(trns_SGS%ncomp_rtp_2_rj,        &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N(trns_SGS%ncomp_rtp_2_rj,        &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
!   transform for vectors and scalars
      call sph_f_transform_SGS(trns_SGS%ncomp_rtp_2_rj,                 &
     &    trns_SGS%nvector_rtp_2_rj, trns_SGS%nscalar_rtp_2_rj,         &
     &    sph, comms_sph, trans_p, trns_SGS,                            &
     &    n_WS, n_WR, WS(1), WR(1), SGS_mul_FFTW)
!
      call copy_SGS_vec_spec_from_trans                                 &
     &   (trns_SGS%ncomp_rtp_2_rj, trns_SGS%f_trns,                     &
     &    comms_sph%comm_rj, ipol, n_WR, WR(1), rj_fld)
!
      end subroutine sph_forward_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_pole_trans_SGS_MHD(sph, comms_sph, trans_p,        &
     &          ipol, rj_fld, trns_SGS)
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
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
      integer(kind = kint) :: nscalar_trans
!
!
      if(trns_SGS%ncomp_rj_2_rtp .le. 0) return
!
      nscalar_trans = trns_SGS%nscalar_rj_2_rtp                         &
     &               + 6*trns_SGS%ntensor_rj_2_rtp
      call check_calypso_sph_comm_buf_N(trns_SGS%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_SGS%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call copy_SGS_spectr_to_send(sph%sph_rtp%nnod_pole,               &
     &    trns_SGS%ncomp_rj_2_rtp, trns_SGS%b_trns,                     &
     &    sph%sph_rj, comms_sph%comm_rj, ipol, rj_fld, n_WS, WS)
!
      call pole_b_transform(trns_SGS%ncomp_rj_2_rtp,                    &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%nscalar_rj_2_rtp,         &
     &    sph, comms_sph, trans_p, n_WS, n_WR, WS(1), WR(1),            &
     &    trns_SGS%flc_pole, trns_SGS%fld_pole)
!
      end subroutine sph_pole_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_SGS
