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
!!     &          rj_fld, trns_bwd, WK_sph, SGS_mul_FFTW)
!!      subroutine sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,   &
!!     &          trns_fwd, WK_sph, SGS_mul_FFTW, rj_fld)
!!      subroutine sph_pole_trans_SGS_MHD                               &
!!     &         (sph, comms_sph, trans_p, rj_fld, trns_bwd)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_bwd
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(spherical_trns_works), intent(inout) :: WK_sph
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
      use t_sph_transforms
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
     &          rj_fld, trns_bwd, WK_sph, SGS_mul_FFTW)
!
      use m_solver_SR
      use sph_trans_w_coriols
      use set_address_sph_trans_MHD
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_data), intent(in) :: rj_fld
!
      type(address_each_sph_trans), intent(inout) :: trns_bwd
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
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
     &   (trns_bwd, comms_sph%comm_rj, rj_fld, n_WS, WS(1))
!
      call sph_b_transform_SGS                                          &
     &   (sph, comms_sph, trans_p, n_WS, n_WR, WS(1), WR(1),            &
     &    trns_bwd, WK_sph, SGS_mul_FFTW)
!
      end subroutine sph_back_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,     &
     &          trns_fwd, WK_sph, SGS_mul_FFTW, rj_fld)
!
      use m_solver_SR
      use sph_trans_w_coriols
      use set_address_sph_trans_MHD
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
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
      call sph_f_transform_SGS(sph, comms_sph, trans_p, trns_fwd,       &
     &    n_WS, n_WR, WS(1), WR(1), WK_sph, SGS_mul_FFTW)
!
      call mhd_spectr_from_recvbuf                                      &
     &   (trns_fwd, comms_sph%comm_rj, n_WR, WR(1), rj_fld)
!
      end subroutine sph_forward_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_pole_trans_SGS_MHD                                 &
     &         (sph, comms_sph, trans_p, rj_fld, trns_bwd)
!
      use m_solver_SR
      use t_sph_transforms
      use sph_transforms_4_MHD
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_data), intent(in) :: rj_fld
      type(address_each_sph_trans), intent(inout) :: trns_bwd
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
