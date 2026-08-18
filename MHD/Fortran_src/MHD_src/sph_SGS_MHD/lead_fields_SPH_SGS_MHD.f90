!>@file   lead_fields_SPH_SGS_MHD.f90
!!@brief  module lead_fields_SPH_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine lead_fields_4_SPH_SGS_MHD(SGS_par, monitor, r_2nd,   &
!!     &          MHD_prop, sph_MHD_bc, trans_p, ipol_LES, sph_MHD_mat, &
!!     &          WK, WK_LES, dynamic_SPH, SPH_MHD, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_mhd_monitor_data), intent(in) :: monitor
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module lead_fields_SPH_SGS_MHD
!
      use m_precision
      use m_machine_parameter
!
      use t_control_parameter
      use t_SGS_control_parameter
      use t_SPH_mesh_field_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_sph_matrices
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_sph_filtering
      use t_phys_address
      use t_SGS_model_addresses
      use t_solver_SR
!
      implicit none
!
      private :: pressure_SGS_SPH_MHD, grad_of_filter_vectors_sph
      private :: enegy_fluxes_SPH_SGS_MHD, lead_SGS_terms_4_SPH
      private :: lead_filter_flds_by_sph_trans
      private :: compatible_magnetic_terms_SPH
      private :: cal_axial_dipole_magnetic_work
      private :: compatible_magnetic_terms_by_sym_SPH
      private :: compatible_one_magnetic_interaction
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lead_fields_4_SPH_SGS_MHD(SGS_par, monitor, r_2nd,     &
     &          MHD_prop, sph_MHD_bc, trans_p, ipol_LES, sph_MHD_mat,   &
     &          WK, WK_LES, dynamic_SPH, SPH_MHD, SR_sig, SR_r)
!
      use t_sph_mhd_monitor_data_IO
      use sph_transforms_4_MHD
      use cal_energy_flux_rtp
      use lead_fields_4_sph_mhd
      use cal_self_buoyancies_sph
      use self_buoyancy_w_filter_sph
      use decomp_w_sym_rj_base_field
      use adjust_scalar_rj_fields
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_mhd_monitor_data), intent(in) :: monitor
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: ibuo_temp,  ibuo_comp
!
!
      call cal_self_buoyancy_sph_SGS_MHD                                &
     &   (SPH_MHD%sph, trans_p%leg, SPH_MHD%ipol, ipol_LES,             &
     &    MHD_prop, sph_MHD_bc%sph_bc_U, SPH_MHD%fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call pressure_SGS_SPH_MHD                                       &
     &     (SGS_par%model_p, SPH_MHD%sph, MHD_prop, sph_MHD_bc,         &
     &      r_2nd, trans_p%leg, sph_MHD_mat%band_p_poisson,             &
     &      SPH_MHD%ipol, ipol_LES, SPH_MHD%fld)
      end if
!
      call s_decomp_w_sym_rj_base_field(SPH_MHD%sph%sph_rj,             &
     &    SPH_MHD%ipol%base, SPH_MHD%ipol%sym_fld,                      &
     &    SPH_MHD%ipol%asym_fld, SPH_MHD%fld)
      call s_adjust_scalar_rj_fields(SPH_MHD%sph,                       &
     &    SPH_MHD%ipol%base, SPH_MHD%ipol%sym_fld,                     &
     &    SPH_MHD%ipol%asym_fld, SPH_MHD%ipol%fld_cmp,                 &
     &    SPH_MHD%ipol%prod_fld, SPH_MHD%fld)
      call adjust_sym_scalar_rj_fields(SPH_MHD%sph,                     &
     &    SPH_MHD%ipol%sym_fld, SPH_MHD%ipol%fld_cmp, SPH_MHD%fld)
!
      call sel_field_address_for_buoyancies(SPH_MHD%ipol%sym_fld,       &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    ibuo_temp, ibuo_comp)
      call sel_buoyancies_sph_MHD(SPH_MHD%sph%sph_rj, trans_p%leg,      &
     &    SPH_MHD%ipol%forces_by_sym_asym, MHD_prop%fl_prop,            &
     &    sph_MHD_bc%sph_bc_U, ibuo_temp, ibuo_comp, SPH_MHD%fld)
      call cal_total_buoyancy(SPH_MHD%ipol%forces_by_sym_asym,          &
     &                        SPH_MHD%fld)
!
      call sel_field_address_for_buoyancies(SPH_MHD%ipol%asym_fld,      &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    ibuo_temp, ibuo_comp)
      call sel_buoyancies_sph_MHD(SPH_MHD%sph%sph_rj, trans_p%leg,      &
     &    SPH_MHD%ipol%forces_by_sym_sym, MHD_prop%fl_prop,             &
     &    sph_MHD_bc%sph_bc_U, ibuo_temp, ibuo_comp, SPH_MHD%fld)
      call cal_total_buoyancy(SPH_MHD%ipol%forces_by_sym_sym,           &
     &                        SPH_MHD%fld)
!
!
      call lead_fields_by_sph_trans(SPH_MHD%sph, SPH_MHD%comms,         &
     &    MHD_prop, trans_p, WK%trns_MHD, WK%trns_snap,                 &
     &    WK%WK_leg, WK%WK_FFTs, SPH_MHD%fld, SR_sig, SR_r)
      call lead_filter_flds_by_sph_trans(SPH_MHD%sph, SPH_MHD%comms,    &
     &    MHD_prop, trans_p, WK_LES%trns_fil_MHD, WK_LES%trns_fil_snap, &
     &    WK%WK_leg, WK%WK_FFTs, SPH_MHD%fld, SR_sig, SR_r)
!
      call gradients_of_vectors_sph                                     &
     &   (SPH_MHD%sph, SPH_MHD%comms, r_2nd, sph_MHD_bc, trans_p,       &
     &    SPH_MHD%ipol, WK%trns_snap, WK%trns_difv,                     &
     &    WK%WK_leg, WK%WK_FFTs, SPH_MHD%fld, SR_sig, SR_r)
      call grad_of_filter_vectors_sph                                   &
     &   (SPH_MHD%sph, SPH_MHD%comms, r_2nd, sph_MHD_bc, trans_p,       &
     &    ipol_LES, WK_LES%trns_fil_snap, WK_LES%trns_fil_difv,         &
     &    WK%WK_leg, WK%WK_FFTs, SPH_MHD%fld, SR_sig, SR_r)
!
      call lead_SGS_terms_4_SPH                                         &
     &   (SGS_par%model_p, SPH_MHD%sph, SPH_MHD%comms, trans_p,         &
     &    WK_LES%trns_Csim, WK_LES%trns_SGS, WK_LES%trns_SGS_snap,      &
     &    dynamic_SPH, SPH_MHD%fld, SR_sig, SR_r)
!
      call enegy_fluxes_SPH_SGS_MHD(monitor%ltr_crust,                  &
     &    monitor%ltr_lowpass, monitor%mtr_lowpass,                     &
     &    SGS_par%model_p, SPH_MHD%sph, SPH_MHD%comms,                  &
     &    r_2nd, MHD_prop, sph_MHD_bc, trans_p,                         &
     &    SPH_MHD%ipol, ipol_LES, WK%trns_MHD, WK_LES%trns_SGS,         &
     &    WK_LES%trns_fil_MHD, WK_LES%trns_fil_snap,                    &
     &    WK%trns_snap, WK%trns_difv, WK%trns_eflux,                    &
     &    WK_LES%trns_SGS_snap, WK%WK_leg, WK%WK_FFTs,                  &
     &    SPH_MHD%fld, SR_sig, SR_r)
!
      end subroutine lead_fields_4_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine pressure_SGS_SPH_MHD(SGS_param, sph, MHD_prop,         &
     &          sph_MHD_bc, r_2nd, leg, band_p_poisson,                 &
     &          ipol, ipol_LES, rj_fld)
!
      use cal_sol_sph_fluid_crank
!
      use cal_sph_divergence_of_force
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use cal_div_of_SGS_forces
      use sph_radial_grad_4_velocity
      use cal_sph_rotation_of_SGS
      use cal_sph_rot_filtered_force
      use sum_rot_of_filter_forces
      use cal_self_buoyancies_sph
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(band_matrices_type), intent(in) :: band_p_poisson
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2                                      &
     &   (sph%sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg%g_sph_rj,        &
     &    ipol%forces, ipol%div_forces, rj_fld)
!     &   ipol%base, ipol%grad_fld, ipol%forces, ipol%div_forces, rj_fld)
      call cal_div_of_filter_forces_sph_2                               &
     &   (sph%sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg%g_sph_rj,        &
     &    ipol_LES%force_by_filter, ipol_LES%div_frc_by_filter, rj_fld)
!     &   ipol_LES%filter_fld, ipol_LES%grad_fil_fld,                   &
!     &    ipol_LES%force_by_filter, ipol_LES%div_frc_by_filter, rj_fld)
!
      call s_const_radial_forces_on_bc(sph%sph_rj, leg%g_sph_rj,        &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,                        &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    ipol%base, ipol%diffusion, ipol%forces, ipol%div_forces,      &
     &    rj_fld)
      call const_radial_fil_forces_on_bc(sph%sph_rj, leg%g_sph_rj,      &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,                        &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    ipol_LES%filter_fld, ipol_LES%force_by_filter,                &
     &    ipol_LES%div_frc_by_filter, rj_fld)
!
      call cal_total_div_buoyancy(ipol%div_forces, rj_fld)
      call cal_total_div_buoyancy(ipol_LES%div_frc_by_filter, rj_fld)
      call sum_div_of_forces                                            &
     &   (MHD_prop%fl_prop, ipol%base, ipol%div_forces, rj_fld)
      call sum_div_of_filtered_forces(MHD_prop%fl_prop,                 &
     &    ipol%base, ipol_LES%div_frc_by_filter, rj_fld)
!
!   ----  Add divegence of SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        call cal_div_of_SGS_forces_sph_2                                &
     &     (sph%sph_rj, r_2nd, sph_MHD_bc, leg%g_sph_rj,                &
     &      ipol_LES%SGS_term, ipol_LES%div_SGS, rj_fld)
        call sum_div_of_SGS_forces(ipol%base, ipol_LES%div_SGS, rj_fld)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v(sph%sph_rj, sph_MHD_bc%sph_bc_U,   &
     &    band_p_poisson, ipol, rj_fld)
!
      if(ipol%forces%i_press_grad .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient                                    &
     &     (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_U,                     &
     &      leg%g_sph_rj, MHD_prop%fl_prop%coef_press,                  &
     &      ipol%base%i_press, ipol%forces%i_press_grad, rj_fld)
      end if
!
      end subroutine pressure_SGS_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine lead_filter_flds_by_sph_trans                          &
     &         (sph, comms_sph, MHD_prop, trans_p,                      &
     &          trns_fil_MHD, trns_fil_snap, WK_leg, WK_FFTs,           &
     &          rj_fld, SR_sig, SR_r)
!
      use sph_transforms_snapshot
      use cal_nonlinear_sph_MHD
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(SGS_address_sph_trans), intent(inout) :: trns_fil_MHD
      type(SGS_address_sph_trans), intent(inout) :: trns_fil_snap
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_debug.gt.0) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p, rj_fld, &
     &    trns_fil_snap%backward, WK_leg, WK_FFTs, SR_sig, SR_r)
!
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
        if (iflag_debug.gt.0) write(*,*) 'nonlinear_terms_on_node'
        call nonlinear_terms_on_node(MHD_prop,                          &
     &     trns_fil_snap%b_trns_LES%filter_fld,                         &
     &     trns_fil_MHD%f_trns_LES%force_by_filter,                     &
     &     sph%sph_rtp%nnod_pole, trns_fil_snap%backward%ncomp,         &
     &     trns_fil_snap%backward%fld_pole, trns_fil_MHD%forward%ncomp, &
     &     trns_fil_MHD%forward%fld_pole)
      end if
!
      end subroutine lead_filter_flds_by_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine grad_of_filter_vectors_sph                             &
     &         (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,             &
     &          ipol_LES, trns_fil_snap, trns_fil_difv,                 &
     &          WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      use sph_transforms_snapshot
      use copy_rtp_vectors_4_grad
      use cal_grad_of_sph_vectors
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SGS_address_sph_trans), intent(in) :: trns_fil_snap
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(SGS_address_sph_trans), intent(inout) :: trns_fil_difv
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_vectors_rtp_4_grad'
      call copy_vectors_rtp_4_grad                                      &
     &   (sph, trns_fil_snap%b_trns_LES%filter_fld,                     &
     &    trns_fil_difv%f_trns_LES%diff_fil_vect,                       &
     &    trns_fil_snap%backward, trns_fil_difv%forward)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &      'sph_forward_trans_snapshot_MHD for diff of vector'
      call sph_forward_trans_snapshot_MHD(sph, comms_sph, trans_p,      &
     &    trns_fil_difv%forward, WK_leg, WK_FFTs, rj_fld,               &
     &    SR_sig, SR_r)
!
      if (iflag_debug.gt.0) write(*,*) 'overwrt_grad_of_vectors_sph'
      call overwrt_grad_of_vectors_sph(sph, r_2nd, sph_MHD_bc,          &
     &    trans_p%leg, ipol_LES%diff_fil_vect, rj_fld)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &      'sph_back_trans_snapshot_MHD for diff of vector'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p, rj_fld, &
     &    trns_fil_difv%backward, WK_leg, WK_FFTs, SR_sig, SR_r)
!
      end subroutine grad_of_filter_vectors_sph
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_SPH_SGS_MHD                               &
     &         (ltr_crust, ltr_lowpass, mtr_lowpass, SGS_param,         &
     &          sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc,            &
     &          trans_p, ipol, ipol_LES, trns_MHD, trns_SGS,            &
     &          trns_fil_MHD, trns_fil_snap, trns_snap, trns_difv,      &
     &          trns_eflux, trns_SGS_snap, WK_leg, WK_FFTs,             &
     &          rj_fld, SR_sig, SR_r)
!
      use sph_transforms_snapshot
      use lead_fields_4_sph_mhd
      use cal_SGS_terms_sph_MHD
      use cal_SGS_buo_flux_sph_MHD
      use cal_energy_flux_w_SGS_rtp
      use cal_force_with_SGS_rj
      use cal_geomagnetic_data
!
      integer(kind = kint), intent(in) :: ltr_crust
      integer(kind = kint), intent(in) :: ltr_lowpass, mtr_lowpass
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(SGS_address_sph_trans), intent(in) :: trns_fil_MHD
      type(SGS_address_sph_trans), intent(in) :: trns_fil_snap
      type(SGS_address_sph_trans), intent(in) :: trns_SGS
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(address_4_sph_trans), intent(in) :: trns_difv
      type(address_4_sph_trans), intent(inout) :: trns_eflux
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS_snap
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_geomagnetic_rtp                                          &
     &   (sph%sph_rtp, sph%sph_rj, sph_MHD_bc%sph_bc_B,                 &
     &    trns_MHD%b_trns%base, trns_eflux%f_trns%prod_fld,             &
     &    trns_MHD%backward%ncomp, trns_MHD%backward%fld_rtp,           &
     &    trns_eflux%forward%ncomp, trns_eflux%forward%fld_rtp)
      call cal_sph_enegy_fluxes                                         &
     &   (ltr_crust, ltr_lowpass, mtr_lowpass, sph, comms_sph,          &
     &    r_2nd, MHD_prop, sph_MHD_bc,                                  &
     &    trans_p, ipol, trns_MHD, trns_snap, trns_difv, trns_eflux,    &
     &    WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
      call compatible_magnetic_terms_SPH                               &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol, &
     &    trns_snap, trns_eflux, WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
      call cal_axial_dipole_magnetic_work                              &
     &   (sph%sph_rtp, trns_snap%b_trns%prod_fld,                      &
     &    trns_eflux%f_trns%forces, trns_eflux%f_trns%ene_flux,       &
     &    trns_snap%backward, trns_eflux%forward)
      call compatible_magnetic_terms_by_sym_SPH                        &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol, &
     &    trns_snap, trns_eflux, WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      if (iflag_debug.eq.1) write(*,*) 's_cal_force_with_SGS_rj'
      call s_cal_force_with_SGS_rj                                      &
     &   (ipol%forces, ipol_LES%SGS_term, ipol_LES%frc_w_SGS, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'backward transform for SGS snapshot'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p, rj_fld, &
     &    trns_SGS_snap%backward, WK_leg, WK_FFTs, SR_sig, SR_r)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_filtered_energy_flux_rtp'
      call cal_filtered_energy_flux_rtp(sph%sph_rtp, MHD_prop%fl_prop,  &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C, trns_snap%b_trns, &
     &    trns_fil_MHD%f_trns_LES, trns_fil_snap%b_trns_LES,            &
     &    trns_SGS_snap%b_trns_LES, trns_SGS_snap%f_trns_LES,           &
     &    trns_snap%backward, trns_fil_MHD%forward,                     &
     &    trns_fil_snap%backward, trns_SGS_snap%backward,               &
     &    trns_SGS_snap%forward)
!
!      Work of SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.eq.1) write(*,*) 'SGS_fluxes_for_snapshot'
        call SGS_fluxes_for_snapshot                                    &
     &     (sph%sph_rtp, MHD_prop%fl_prop, trns_snap%b_trns%base,       &
     &      trns_SGS%f_trns_LES%SGS_term,                               &
     &      trns_SGS_snap%b_trns_LES%SGS_term,                          &
     &      trns_SGS_snap%f_trns_LES%SGS_ene_flux,                      &
     &      trns_snap%backward, trns_SGS%forward,                       &
     &      trns_SGS_snap%backward, trns_SGS_snap%forward)
      end if
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &      'forward transform for energy flux snapshot'
      call sph_forward_trans_snapshot_MHD(sph, comms_sph, trans_p,      &
     &    trns_eflux%forward, WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
      if (iflag_debug.eq.1) write(*,*)                                  &
     &      'forward transform for SGS snapshot'
      call sph_forward_trans_snapshot_MHD(sph, comms_sph, trans_p,      &
     &    trns_SGS_snap%forward, WK_leg, WK_FFTs,                       &
     &    rj_fld, SR_sig, SR_r)
!
      end subroutine enegy_fluxes_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine compatible_magnetic_terms_SPH                         &
     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, &
     &          ipol, trns_snap, trns_eflux, WK_leg, WK_FFTs,         &
     &          rj_fld, SR_sig, SR_r)
!
      use const_sph_radial_grad
      use sph_transforms_snapshot
      use poynting_flux_smp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(address_4_sph_trans), intent(inout) :: trns_eflux
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind=kint) :: i
      real(kind=kreal), allocatable :: wk_scalar(:)
      real(kind=kreal), allocatable :: save_ind_rj(:,:)
      real(kind=kreal), allocatable :: save_ind_rtp(:,:)
      real(kind=kreal), allocatable :: save_ind_pole(:,:)
      real(kind=kreal), allocatable :: save_ujb_rtp(:)
      real(kind=kreal), allocatable :: save_ujb_pole(:)
!
      if(trns_eflux%f_trns%forces%i_mag_stretch.le.0) return
      if(trns_eflux%f_trns%forces%i_mag_advection.le.0) return
      if(trns_eflux%f_trns%ene_flux%i_ujb.le.0) return
      if(trns_eflux%b_trns%forces%i_induction.le.0) return
!
      allocate(wk_scalar(rj_fld%n_point))
      allocate(save_ind_rj(rj_fld%n_point,3))
      allocate(save_ind_rtp(sph%sph_rtp%nnod_rtp,3))
      allocate(save_ind_pole(sph%sph_rtp%nnod_pole,3))
      allocate(save_ujb_rtp(sph%sph_rtp%nnod_rtp))
      allocate(save_ujb_pole(sph%sph_rtp%nnod_pole))
!
      save_ind_rj(1:rj_fld%n_point,1:3)                                &
     & =rj_fld%d_fld(1:rj_fld%n_point,ipol%forces%i_induction:        &
     &                                      ipol%forces%i_induction+2)
      save_ind_rtp=trns_eflux%backward%fld_rtp(:,                     &
     & trns_eflux%b_trns%forces%i_induction:                           &
     & trns_eflux%b_trns%forces%i_induction+2)
      if(sph%sph_rtp%nnod_pole.gt.0) save_ind_pole                    &
     & =trns_eflux%backward%fld_pole(:,                               &
     & trns_eflux%b_trns%forces%i_induction:                           &
     & trns_eflux%b_trns%forces%i_induction+2)
      save_ujb_rtp=trns_eflux%forward%fld_rtp(:,                      &
     &                         trns_eflux%f_trns%ene_flux%i_ujb)
      if(sph%sph_rtp%nnod_pole.gt.0) save_ujb_pole                    &
     & =trns_eflux%forward%fld_pole(:,                                &
     &                         trns_eflux%f_trns%ene_flux%i_ujb)
!
!     Use the existing Lorentz-work scalar transform slot temporarily
!     for the true scalar h = u dot B.
!$omp parallel do private(i)
      do i=1,sph%sph_rtp%nnod_rtp
        trns_eflux%forward%fld_rtp(i,                                 &
     &        trns_eflux%f_trns%ene_flux%i_ujb)                       &
     &   =sum(trns_snap%backward%fld_rtp(i,                           &
     &        trns_snap%b_trns%base%i_velo:                           &
     &        trns_snap%b_trns%base%i_velo+2)                         &
     &       *trns_snap%backward%fld_rtp(i,                           &
     &        trns_snap%b_trns%base%i_magne:                          &
     &        trns_snap%b_trns%base%i_magne+2))
      end do
!$omp end parallel do
!$omp parallel do private(i)
      do i=1,sph%sph_rtp%nnod_pole
        trns_eflux%forward%fld_pole(i,                                &
     &        trns_eflux%f_trns%ene_flux%i_ujb)                       &
     &   =sum(trns_snap%backward%fld_pole(i,                          &
     &        trns_snap%b_trns%base%i_velo:                           &
     &        trns_snap%b_trns%base%i_velo+2)                         &
     &       *trns_snap%backward%fld_pole(i,                          &
     &        trns_snap%b_trns%base%i_magne:                          &
     &        trns_snap%b_trns%base%i_magne+2))
      end do
!$omp end parallel do
!
      call sph_forward_trans_snapshot_MHD(sph, comms_sph, trans_p,    &
     &    trns_eflux%forward, WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
      call const_sph_gradient_no_bc(sph%sph_rj, r_2nd,                &
     &    sph_MHD_bc%sph_bc_B, trans_p%leg%g_sph_rj,                  &
     &    ipol%ene_flux%i_ujb, ipol%forces%i_induction,               &
     &    wk_scalar, rj_fld)
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,       &
     &    rj_fld, trns_eflux%backward, WK_leg, WK_FFTs, SR_sig, SR_r)
!
      call cal_compatible_magnetic_terms                              &
     &   (sph%sph_rtp%nnod_rtp, MHD_prop%cd_prop%coef_induct,         &
     &    save_ind_rtp, trns_eflux%backward%fld_rtp(1,                &
     &                  trns_eflux%b_trns%forces%i_induction),        &
     &    trns_snap%backward%fld_rtp(1,trns_snap%b_trns%base%i_velo), &
     &    trns_snap%backward%fld_rtp(1,trns_snap%b_trns%base%i_magne),&
     &    trns_snap%backward%fld_rtp(1,trns_snap%b_trns%base%i_current),&
     &    trns_snap%backward%fld_rtp(1,trns_snap%b_trns%base%i_vort), &
     &    trns_eflux%forward%fld_rtp(1,                               &
     &                  trns_eflux%f_trns%forces%i_mag_advection),    &
     &    trns_eflux%forward%fld_rtp(1,                               &
     &                  trns_eflux%f_trns%forces%i_mag_stretch))
      if(sph%sph_rtp%nnod_pole.gt.0)                                  &
     & call cal_compatible_magnetic_terms                             &
     &   (sph%sph_rtp%nnod_pole, MHD_prop%cd_prop%coef_induct,        &
     &    save_ind_pole, trns_eflux%backward%fld_pole(1,              &
     &                  trns_eflux%b_trns%forces%i_induction),        &
     &    trns_snap%backward%fld_pole(1,trns_snap%b_trns%base%i_velo),&
     &    trns_snap%backward%fld_pole(1,trns_snap%b_trns%base%i_magne),&
     &    trns_snap%backward%fld_pole(1,trns_snap%b_trns%base%i_current),&
     &    trns_snap%backward%fld_pole(1,trns_snap%b_trns%base%i_vort),&
     &    trns_eflux%forward%fld_pole(1,                              &
     &                  trns_eflux%f_trns%forces%i_mag_advection),    &
     &    trns_eflux%forward%fld_pole(1,                              &
     &                  trns_eflux%f_trns%forces%i_mag_stretch))
!
!     Restore the native induction spectrum and the real Lorentz work.
      rj_fld%d_fld(1:rj_fld%n_point,ipol%forces%i_induction:          &
     & ipol%forces%i_induction+2)=save_ind_rj(1:rj_fld%n_point,1:3)
      trns_eflux%forward%fld_rtp(:,                                   &
     & trns_eflux%f_trns%ene_flux%i_ujb)=save_ujb_rtp
      if(sph%sph_rtp%nnod_pole.gt.0)                                  &
     & trns_eflux%forward%fld_pole(:,                                 &
     & trns_eflux%f_trns%ene_flux%i_ujb)=save_ujb_pole
!
      deallocate(wk_scalar,save_ind_rj,save_ind_rtp,save_ind_pole)
      deallocate(save_ujb_rtp,save_ujb_pole)
!
      end subroutine compatible_magnetic_terms_SPH
!
! ----------------------------------------------------------------------
!
      subroutine cal_axial_dipole_magnetic_work                       &
     &         (sph_rtp, bs_trns_prod, fs_trns_frc, fs_trns_eflux,    &
     &          trns_b_snap, trns_f_eflux)
!
      use cal_vector_products
      use t_spheric_rtp_data
      use t_field_product_labels
      use t_base_force_labels
      use t_energy_flux_labels
      use t_addresses_sph_transform
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_products_address), intent(in) :: bs_trns_prod
      type(base_force_address), intent(in) :: fs_trns_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(inout) :: trns_f_eflux
!
!
      if(bs_trns_prod%i_dipole_B .le. 0) return
!
      if(      fs_trns_eflux%i_axial_dipole_mag_advect .gt. 0         &
     &   .and. fs_trns_frc%i_mag_advection .gt. 0) then
        call cal_dot_product_no_coef(sph_rtp%nnod_rtp,                 &
     &      trns_b_snap%fld_rtp(1,bs_trns_prod%i_dipole_B),            &
     &      trns_f_eflux%fld_rtp(1,fs_trns_frc%i_mag_advection),       &
     &      trns_f_eflux%fld_rtp(1,                                   &
     &                   fs_trns_eflux%i_axial_dipole_mag_advect))
        call cal_dot_product_no_coef(sph_rtp%nnod_pole,                &
     &      trns_b_snap%fld_pole(1,bs_trns_prod%i_dipole_B),           &
     &      trns_f_eflux%fld_pole(1,fs_trns_frc%i_mag_advection),      &
     &      trns_f_eflux%fld_pole(1,                                  &
     &                   fs_trns_eflux%i_axial_dipole_mag_advect))
      end if
!
      if(      fs_trns_eflux%i_axial_dipole_mag_stretch .gt. 0        &
     &   .and. fs_trns_frc%i_mag_stretch .gt. 0) then
        call cal_dot_product_no_coef(sph_rtp%nnod_rtp,                 &
     &      trns_b_snap%fld_rtp(1,bs_trns_prod%i_dipole_B),            &
     &      trns_f_eflux%fld_rtp(1,fs_trns_frc%i_mag_stretch),         &
     &      trns_f_eflux%fld_rtp(1,                                   &
     &                   fs_trns_eflux%i_axial_dipole_mag_stretch))
        call cal_dot_product_no_coef(sph_rtp%nnod_pole,                &
     &      trns_b_snap%fld_pole(1,bs_trns_prod%i_dipole_B),           &
     &      trns_f_eflux%fld_pole(1,fs_trns_frc%i_mag_stretch),        &
     &      trns_f_eflux%fld_pole(1,                                  &
     &                   fs_trns_eflux%i_axial_dipole_mag_stretch))
      end if
!
      end subroutine cal_axial_dipole_magnetic_work
!
! ----------------------------------------------------------------------
!
      subroutine compatible_magnetic_terms_by_sym_SPH                 &
     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, &
     &          ipol, trns_snap, trns_eflux, WK_leg, WK_FFTs,         &
     &          rj_fld, SR_sig, SR_r)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(address_4_sph_trans), intent(inout) :: trns_eflux
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call compatible_one_magnetic_interaction                        &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol, &
     &    ipol%sym_fld, ipol%sym_fld, ipol%forces_by_sym_sym,         &
     &    trns_snap%b_trns%sym_fld, trns_snap%b_trns%sym_fld,         &
     &    trns_eflux%b_trns%forces_by_sym_sym,                        &
     &    trns_eflux%f_trns%forces_by_sym_sym, trns_snap, trns_eflux,&
     &    WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
      call compatible_one_magnetic_interaction                        &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol, &
     &    ipol%asym_fld, ipol%asym_fld, ipol%forces_by_asym_asym,     &
     &    trns_snap%b_trns%asym_fld, trns_snap%b_trns%asym_fld,       &
     &    trns_eflux%b_trns%forces_by_asym_asym,                      &
     &    trns_eflux%f_trns%forces_by_asym_asym, trns_snap,trns_eflux,&
     &    WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
      call compatible_one_magnetic_interaction                        &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol, &
     &    ipol%sym_fld, ipol%asym_fld, ipol%forces_by_sym_asym,       &
     &    trns_snap%b_trns%sym_fld, trns_snap%b_trns%asym_fld,        &
     &    trns_eflux%b_trns%forces_by_sym_asym,                       &
     &    trns_eflux%f_trns%forces_by_sym_asym, trns_snap, trns_eflux,&
     &    WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
      call compatible_one_magnetic_interaction                        &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol, &
     &    ipol%asym_fld, ipol%sym_fld, ipol%forces_by_asym_sym,       &
     &    trns_snap%b_trns%asym_fld, trns_snap%b_trns%sym_fld,        &
     &    trns_eflux%b_trns%forces_by_asym_sym,                       &
     &    trns_eflux%f_trns%forces_by_asym_sym, trns_snap, trns_eflux,&
     &    WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      end subroutine compatible_magnetic_terms_by_sym_SPH
!
! ----------------------------------------------------------------------
!
      subroutine compatible_one_magnetic_interaction                  &
     &         (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p, &
     &          ipol, ipol_u, ipol_b, ipol_frc, bs_u, bs_b, be_frc,   &
     &          fe_frc, trns_snap, trns_eflux, WK_leg, WK_FFTs,       &
     &          rj_fld, SR_sig, SR_r)
!
      use const_sph_radial_grad
      use sph_transforms_snapshot
      use poynting_flux_smp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(base_field_address), intent(in) :: ipol_u, ipol_b
      type(base_force_address), intent(in) :: ipol_frc
      type(base_field_address), intent(in) :: bs_u, bs_b
      type(base_force_address), intent(in) :: be_frc, fe_frc
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(address_4_sph_trans), intent(inout) :: trns_eflux
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind=kint) :: i
      real(kind=kreal), allocatable :: wk_scalar(:), save_ind_rj(:,:)
      real(kind=kreal), allocatable :: save_ind_rtp(:,:), save_ind_pole(:,:)
      real(kind=kreal), allocatable :: save_ujb_rtp(:), save_ujb_pole(:)
!
      if(ipol_frc%i_mag_stretch.le.0) return
      if(ipol_frc%i_mag_advection.le.0) return
      if(ipol_frc%i_induction.le.0 .or. be_frc%i_induction.le.0) return
      if(fe_frc%i_mag_stretch.le.0 .or. fe_frc%i_mag_advection.le.0) return
      if(bs_u%i_velo.le.0 .or. bs_u%i_vort.le.0) return
      if(bs_b%i_magne.le.0 .or. bs_b%i_current.le.0) return
!
      allocate(wk_scalar(rj_fld%n_point),save_ind_rj(rj_fld%n_point,3))
      allocate(save_ind_rtp(sph%sph_rtp%nnod_rtp,3))
      allocate(save_ind_pole(sph%sph_rtp%nnod_pole,3))
      allocate(save_ujb_rtp(sph%sph_rtp%nnod_rtp))
      allocate(save_ujb_pole(sph%sph_rtp%nnod_pole))
      save_ind_rj=rj_fld%d_fld(:,ipol_frc%i_induction:                &
     &                              ipol_frc%i_induction+2)
      save_ind_rtp=trns_eflux%backward%fld_rtp(:,be_frc%i_induction:  &
     &                                             be_frc%i_induction+2)
      if(sph%sph_rtp%nnod_pole.gt.0) save_ind_pole=                   &
     & trns_eflux%backward%fld_pole(:,be_frc%i_induction:             &
     &                                             be_frc%i_induction+2)
      save_ujb_rtp=trns_eflux%forward%fld_rtp(:,                      &
     &                         trns_eflux%f_trns%ene_flux%i_ujb)
      if(sph%sph_rtp%nnod_pole.gt.0) save_ujb_pole=                   &
     & trns_eflux%forward%fld_pole(:,trns_eflux%f_trns%ene_flux%i_ujb)
!
!$omp parallel do private(i)
      do i=1,sph%sph_rtp%nnod_rtp
        trns_eflux%forward%fld_rtp(i,trns_eflux%f_trns%ene_flux%i_ujb)&
     &   =sum(trns_snap%backward%fld_rtp(i,bs_u%i_velo:bs_u%i_velo+2)&
     &       *trns_snap%backward%fld_rtp(i,bs_b%i_magne:bs_b%i_magne+2))
      end do
!$omp end parallel do
!$omp parallel do private(i)
      do i=1,sph%sph_rtp%nnod_pole
        trns_eflux%forward%fld_pole(i,trns_eflux%f_trns%ene_flux%i_ujb)&
     &   =sum(trns_snap%backward%fld_pole(i,bs_u%i_velo:bs_u%i_velo+2)&
     &       *trns_snap%backward%fld_pole(i,bs_b%i_magne:bs_b%i_magne+2))
      end do
!$omp end parallel do
      call sph_forward_trans_snapshot_MHD(sph, comms_sph, trans_p,    &
     &    trns_eflux%forward, WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
      call const_sph_gradient_no_bc(sph%sph_rj, r_2nd,                &
     &    sph_MHD_bc%sph_bc_B, trans_p%leg%g_sph_rj,                  &
     &    ipol%ene_flux%i_ujb, ipol_frc%i_induction, wk_scalar,rj_fld)
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,       &
     &    rj_fld, trns_eflux%backward, WK_leg, WK_FFTs, SR_sig, SR_r)
      call cal_compatible_magnetic_terms                              &
     &   (sph%sph_rtp%nnod_rtp,MHD_prop%cd_prop%coef_induct,save_ind_rtp,&
     &    trns_eflux%backward%fld_rtp(1,be_frc%i_induction),          &
     &    trns_snap%backward%fld_rtp(1,bs_u%i_velo),                  &
     &    trns_snap%backward%fld_rtp(1,bs_b%i_magne),                 &
     &    trns_snap%backward%fld_rtp(1,bs_b%i_current),               &
     &    trns_snap%backward%fld_rtp(1,bs_u%i_vort),                  &
     &    trns_eflux%forward%fld_rtp(1,fe_frc%i_mag_advection),       &
     &    trns_eflux%forward%fld_rtp(1,fe_frc%i_mag_stretch))
      if(sph%sph_rtp%nnod_pole.gt.0) call cal_compatible_magnetic_terms&
     &   (sph%sph_rtp%nnod_pole,MHD_prop%cd_prop%coef_induct,save_ind_pole,&
     &    trns_eflux%backward%fld_pole(1,be_frc%i_induction),          &
     &    trns_snap%backward%fld_pole(1,bs_u%i_velo),                  &
     &    trns_snap%backward%fld_pole(1,bs_b%i_magne),                 &
     &    trns_snap%backward%fld_pole(1,bs_b%i_current),               &
     &    trns_snap%backward%fld_pole(1,bs_u%i_vort),                  &
     &    trns_eflux%forward%fld_pole(1,fe_frc%i_mag_advection),       &
     &    trns_eflux%forward%fld_pole(1,fe_frc%i_mag_stretch))
      rj_fld%d_fld(:,ipol_frc%i_induction:ipol_frc%i_induction+2)    &
     &     =save_ind_rj
      trns_eflux%forward%fld_rtp(:,trns_eflux%f_trns%ene_flux%i_ujb) &
     &     =save_ujb_rtp
      if(sph%sph_rtp%nnod_pole.gt.0) trns_eflux%forward%fld_pole(:,  &
     & trns_eflux%f_trns%ene_flux%i_ujb)=save_ujb_pole
      deallocate(wk_scalar,save_ind_rj,save_ind_rtp,save_ind_pole)
      deallocate(save_ujb_rtp,save_ujb_pole)
!
      end subroutine compatible_one_magnetic_interaction
!
! ----------------------------------------------------------------------
!
      subroutine lead_SGS_terms_4_SPH                                   &
     &         (SGS_param, sph, comms_sph, trans_p,                     &
     &          trns_Csim, trns_SGS, trns_SGS_snap,                     &
     &          dynamic_SPH, rj_fld, SR_sig, SR_r)
!
      use sph_transforms_4_SGS
      use copy_Csim_4_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SGS_address_sph_trans), intent(in) :: trns_Csim
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS_snap
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(SGS_param%iflag_SGS .eq. id_SGS_none) return
      if (iflag_debug.eq.1) write(*,*) 'sph_pole_trans_SGS_MHD'
      call sph_pole_trans_SGS_MHD(sph, comms_sph, trans_p, rj_fld,      &
     &                            trns_SGS%backward, SR_sig, SR_r)
!
      if(SGS_param%iflag_dynamic .gt. id_SGS_none) then
        if(iflag_debug.eq.1) write(*,*) 'copy_model_coefs_4_sph_snap'
        call copy_model_coefs_4_sph_snap                                &
     &     (sph%sph_rtp, dynamic_SPH%sph_d_grp,                         &
     &      dynamic_SPH%iak_sgs_term, trns_Csim%f_trns_LES%Csim,        &
     &      dynamic_SPH%wk_sph_sgs, trns_SGS_snap%forward)
      end if
!
      end subroutine lead_SGS_terms_4_SPH
!
! ----------------------------------------------------------------------
!
      end module lead_fields_SPH_SGS_MHD
