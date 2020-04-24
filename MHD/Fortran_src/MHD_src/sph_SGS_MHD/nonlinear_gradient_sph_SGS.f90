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
!!     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc,          &
!!     &          trans_p, dynamic_SPH, ipol, trns_MHD, WK_sph, rj_fld, &
!!     &          trns_ngTMP, trns_SGS)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_ngTMP
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!      subroutine cal_wide_nonlinear_grad_sph_SGS                      &
!!     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, &
!!     &          dynamic_SPH, ipol, ipol_LES, trns_SIMI, WK_sph,       &
!!     &          rj_fld, trns_DYNG, trns_Csim)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_ngSGS
!!        type(address_4_sph_trans), intent(inout) :: trns_SIMI
!!@endverbatim
!
      module nonlinear_gradient_sph_SGS
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_addresses_sph_transform
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
      use t_sph_transforms
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_boundary_data_sph_MHD
      use t_control_parameter
      use t_sph_filtering
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
     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc,            &
     &          trans_p, dynamic_SPH, ipol, trns_MHD, WK_sph, rj_fld,   &
     &          trns_ngTMP, trns_SGS)
!
      use copy_rtp_vectors_4_grad
      use sph_transforms_4_SGS
      use cal_grad_of_sph_vectors
      use nolinear_gradient_terms_sph
      use sph_poynting_flux_smp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_ngTMP
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_vectors_rtp_4_grad'
      call copy_vectors_rtp_4_grad                                      &
     &   (sph, trns_MHD%b_trns, trns_ngTMP%f_trns,                      &
     &    trns_MHD%backward, trns_ngTMP%forward)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &        'sph_forward_trans_SGS_MHD trns_ngTMP'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_ngTMP%forward, WK_sph, trns_ngTMP%mul_FFTW, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'overwrt_grad_of_vectors_sph'
      call overwrt_grad_of_vectors_sph                                  &
     &   (sph, r_2nd, sph_MHD_bc, trans_p%leg, ipol%base,               &
     &    ipol%diff_vector, ipol%grad_fld, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &       'sph_back_trans_SGS_MHD trns_ngTMP'
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_ngTMP%backward, WK_sph, trns_ngTMP%mul_FFTW)
!
      if (iflag_debug.eq.1) write(*,*) 'nl_gradient_SGS_terms_rtp'
      call nl_gradient_SGS_terms_rtp                                    &
     &    (sph, dynamic_SPH%sph_filters(1), MHD_prop,                   &
     &    trns_MHD%b_trns%base, trns_ngTMP%b_trns%grad_fld,             &
     &    trns_ngTMP%b_trns%diff_vector, trns_SGS%f_trns%SGS_term,      &
     &    trns_MHD%backward, trns_ngTMP%backward, trns_SGS%forward)
!
      end subroutine cal_nonlinear_gradient_sph_SGS
!
! ----------------------------------------------------------------------
!
      subroutine cal_wide_nonlinear_grad_sph_SGS                        &
     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p,   &
     &          dynamic_SPH, ipol, ipol_LES, trns_SIMI, WK_sph,         &
     &          rj_fld, trns_DYNG, trns_Csim)
!
      use copy_rtp_vectors_4_grad
      use sph_transforms_4_SGS
      use cal_grad_of_sph_vectors
      use nolinear_gradient_terms_sph
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(address_4_sph_trans), intent(in) :: trns_SIMI
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_DYNG
      type(address_4_sph_trans), intent(inout) :: trns_Csim
!
      if (iflag_debug.eq.1) write(*,*) 'copy_filter_vecs_rtp_4_grad'
      call copy_filter_vecs_rtp_4_grad(sph,                             &
     &    trns_SIMI%b_trns%filter_fld,                                  &
     &    trns_DYNG%f_trns_LES%diff_fil_vect,                           &
     &    trns_SIMI%backward, trns_DYNG%forward)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'sph_forward_trans_SGS_MHD trns_DYNG'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_DYNG%forward, WK_sph, trns_DYNG%mul_FFTW, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'overwrt_grad_of_vectors_sph'
      call overwrt_grad_of_vectors_sph                                  &
     &   (sph, r_2nd,  sph_MHD_bc, trans_p%leg, ipol%filter_fld,        &
     &    ipol_LES%diff_fil_vect, ipol_LES%grad_fil_fld, rj_fld)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'sph_back_trans_SGS_MHD trns_DYNG'
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_DYNG%backward, WK_sph, trns_DYNG%mul_FFTW)
!
      if (iflag_debug.eq.1) write(*,*) 'wider_nl_grad_SGS_rtp'
      call wider_nl_grad_SGS_rtp                                        &
     &   (sph, dynamic_SPH%sph_filters(2), MHD_prop,                    &
     &    trns_SIMI%b_trns%filter_fld,                                  &
     &    trns_DYNG%b_trns_LES%grad_fil_fld,                            &
     &    trns_DYNG%b_trns_LES%diff_fil_vect,                           &
     &    trns_Csim%b_trns_LES%wide_SGS,                                &
     &    trns_SIMI%backward, trns_DYNG%backward, trns_Csim%backward)
!
      end subroutine cal_wide_nonlinear_grad_sph_SGS
!
! ----------------------------------------------------------------------
!
      end module nonlinear_gradient_sph_SGS
