!>@file   nonlinear_gradient_sph_SGS.f90
!!@brief  module nonlinear_gradient_sph_SGS
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine cal_nonlinear_gradient_sph_SGS                       &
!!     &         (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,           &
!!     &          ipol, trns_MHD, WK_sph, rj_fld, trns_ngSGS,           &
!!     &          SGS_mul_ngSGS)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_ngSGS
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_ngSGS
!!      subroutine cal_wide_nonlinear_grad_sph_SGS                      &
!!     &         (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,           &
!!     &          ipol, trns_MHD, WK_sph, rj_fld, trns_ngSGS,           &
!!     &          SGS_mul_ngSGS)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_ngSGS
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_ngSGS
!!@endverbatim
!
      module nonlinear_gradient_sph_SGS
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_nonlinear_gradient_sph_SGS                         &
     &         (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,             &
     &          ipol, trns_MHD, WK_sph, rj_fld, trns_ngSGS,             &
     &          SGS_mul_ngSGS)
!
      use copy_rtp_vectors_4_grad
      use sph_transforms_4_SGS
      use cal_grad_of_sph_vectors
      use nolinear_gradient_sph_MHD
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_ngSGS
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_ngSGS
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_vectors_rtp_4_grad'
      call copy_vectors_rtp_4_grad                                      &
     &   (sph, trns_MHD%b_trns, trns_ngSGS%f_trns,                      &
     &    trns_MHD%backward, trns_ngSGS%forward)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_ngSGS%forward, WK_sph, SGS_mul_ngSGS, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'overwrt_grad_of_vectors_sph'
      call overwrt_grad_of_vectors_sph(sph, r_2nd,                      &
     &    sph_MHD_bc, trans_p%leg, ipol, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_ngSGS%backward, WK_sph, SGS_mul_ngSGS)
!
      if (iflag_debug.eq.1) write(*,*) 'nl_gradient_SGS_terms_rtp'
      call nl_gradient_SGS_terms_rtp(sph, gamma, MHD_prop,              &
     &    trns_MHD%b_trns, trns_ngSGS%b_trns, trns_SGS%f_trns,          &
     &    trns_MHD%backward, trns_ngSGS%backward, trns_SGS%forward)
!
      end subroutine cal_nonlinear_gradient_sph_SGS
!
! ----------------------------------------------------------------------
!
      subroutine cal_wide_nonlinear_grad_sph_SGS                        &
     &         (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,             &
     &          ipol, trns_SGS, WK_sph, rj_fld, trns_ngDNMC,            &
     &          SGS_mul_ngDNMC)
!
      use copy_rtp_vectors_4_grad
      use sph_transforms_4_SGS
      use cal_grad_of_sph_vectors
      use nolinear_gradient_sph_MHD
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(in) :: trns_SGS
!
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_ngDNMC
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_ngDNMC
!
      if (iflag_debug.eq.1) write(*,*) 'copy_filter_vecs_rtp_4_grad'
      call copy_filter_vecs_rtp_4_grad                                  &
     &   (sph, trns_SGS%b_trns, trns_ngDNMC%f_trns,                     &
     &    trns_SGS%backward, trns_ngDNMC%forward)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_ngDNMC%forward, WK_sph, SGS_mul_ngDNMC, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'overwrt_grad_filter_vecs_sph'
      call overwrt_grad_filter_vecs_sph(sph%sph_rj, r_2nd,              &
     &    sph_MHD_bc, trans_p%leg, ipol, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_ngDNMC%backward, WK_sph, SGS_mul_ngDNMC)
!
      if (iflag_debug.eq.1) write(*,*) 'nl_gradient_SGS_terms_rtp'
      call wider_nl_grad_SGS_rtp(sph, gamma, MHD_prop,                  &
     &    trns_SGS%b_trns, trns_ngDNMC%b_trns, trns_DYNS%f_trns,        &
     &    trns_SGS%backward, trns_ngDNMC%backward, trns_DYNS%forward)
!
      end subroutine cal_wide_nonlinear_grad_sph_SGS
!
! ----------------------------------------------------------------------
!
      end module nonlinear_gradient_sph_SGS
