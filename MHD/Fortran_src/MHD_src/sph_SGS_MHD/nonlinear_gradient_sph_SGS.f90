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
!!     &         (sph, comms_sph, r_2nd, sph_MHD_bc, MHD_prop,          &
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
!!     &         (sph, comms_sph, r_2nd, sph_MHD_bc, MHD_prop, trans_p, &
!!     &          dynamic_SPH, ipol, trns_SIMI, WK_sph, rj_fld,         &
!!     &          trns_ngDNMC, trns_DYNS)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
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
      use t_phys_address
      use t_phys_data
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
     &         (sph, comms_sph, r_2nd, sph_MHD_bc, MHD_prop,            &
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
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(MHD_evolution_param), intent(in) :: MHD_prop
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
      if (iflag_debug.eq.1) write(*,*) 'Forward transform: trns_ngTMP'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_ngTMP%forward, WK_sph, trns_ngTMP%mul_FFTW, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'overwrt_grad_of_vectors_sph'
      call overwrt_grad_of_vectors_sph(sph, r_2nd,                      &
     &    sph_MHD_bc, trans_p%leg, ipol, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_ngTMP%backward, WK_sph, trns_ngTMP%mul_FFTW)
!
      if (iflag_debug.eq.1) write(*,*) 'nl_gradient_SGS_terms_rtp'
      call nl_gradient_SGS_terms_rtp                                    &
     &    (sph, dynamic_SPH%sph_filters(1), MHD_prop,                   &
     &    trns_MHD%b_trns, trns_ngTMP%b_trns, trns_SGS%f_trns,          &
     &    trns_MHD%backward, trns_ngTMP%backward, trns_SGS%forward)
!
      end subroutine cal_nonlinear_gradient_sph_SGS
!
! ----------------------------------------------------------------------
!
      subroutine cal_wide_nonlinear_grad_sph_SGS                        &
     &         (sph, comms_sph, r_2nd, sph_MHD_bc, MHD_prop, trans_p,   &
     &          dynamic_SPH, ipol, trns_SIMI, WK_sph, rj_fld,           &
     &          trns_DYNS, trns_Csim)
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
      type(address_4_sph_trans), intent(in) :: trns_SIMI
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
      type(address_4_sph_trans), intent(inout) :: trns_Csim
!
      if (iflag_debug.eq.1) write(*,*) 'copy_filter_vecs_rtp_4_grad'
      call copy_filter_vecs_rtp_4_grad                                  &
     &   (sph, trns_SIMI%b_trns, trns_SIMI%f_trns,                      &
     &    trns_SIMI%backward, trns_DYNS%forward)
      call calypso_mpi_barrier
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_DYNS%forward, WK_sph, trns_DYNS%mul_FFTW, rj_fld)
      call calypso_mpi_barrier
!
      if (iflag_debug.eq.1) write(*,*) 'overwrt_grad_filter_vecs_sph'
      call overwrt_grad_filter_vecs_sph(sph, r_2nd,                     &
     &    sph_MHD_bc, trans_p%leg, ipol, rj_fld)
      call calypso_mpi_barrier
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_DYNS%backward, WK_sph, trns_DYNS%mul_FFTW)
      call calypso_mpi_barrier
!
      if (iflag_debug.eq.1) write(*,*) 'wider_nl_grad_SGS_rtp'
      call wider_nl_grad_SGS_rtp                                        &
     &   (sph, dynamic_SPH%sph_filters(2), MHD_prop,                    &
     &    trns_SIMI%b_trns, trns_DYNS%b_trns, trns_Csim%b_trns,         &
     &    trns_SIMI%backward, trns_DYNS%backward, trns_Csim%backward)
      call calypso_mpi_barrier
!
      end subroutine cal_wide_nonlinear_grad_sph_SGS
!
! ----------------------------------------------------------------------
!
      end module nonlinear_gradient_sph_SGS
