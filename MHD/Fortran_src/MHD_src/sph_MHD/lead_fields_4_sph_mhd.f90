!>@file   lead_fields_4_sph_mhd.f90
!!@brief  module lead_fields_4_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine s_lead_fields_4_sph_mhd(SGS_param, sph, comms_sph,   &
!!     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol,           &
!!     &          sph_MHD_mat, rj_fld, WK)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module lead_fields_4_sph_mhd
!
      use m_precision
      use m_machine_parameter
!
      use t_control_parameter
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_addresses_sph_transform
      use t_sph_trans_arrays_MHD
      use t_sph_matrices
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_transforms
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
      use sph_filtering
!
      implicit none
!
      private :: pressure_4_sph_mhd
      private :: gradients_of_vectors_sph, enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_lead_fields_4_sph_mhd(SGS_param, sph, comms_sph,     &
     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol,             &
     &          sph_MHD_mat, rj_fld, WK)
!
      use sph_transforms_4_MHD
      use sph_transforms_4_SGS
      use copy_MHD_4_sph_trans
      use cal_energy_flux_rtp
      use swap_phi_4_sph_trans
      use dynamic_model_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call pressure_4_sph_mhd                                         &
     &     (SGS_param, sph%sph_rj, MHD_prop, sph_MHD_bc, r_2nd,         &
     &      trans_p%leg, sph_MHD_mat%band_p_poisson, ipol, rj_fld)
      end if
!
!
      call swap_phi_from_trans(WK%trns_MHD%ncomp_rj_2_rtp,              &
     &    sph%sph_rtp%nnod_rtp, sph%sph_rtp%nidx_rtp,                   &
     &    WK%trns_MHD%fld_rtp)
      call swap_phi_from_trans(WK%trns_MHD%ncomp_rtp_2_rj,              &
     &    sph%sph_rtp%nnod_rtp, sph%sph_rtp%nidx_rtp,                   &
     &    WK%trns_MHD%frc_rtp)
!
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
        if (iflag_debug.eq.1) write(*,*) 'sph_pole_trans_4_MHD'
        call sph_pole_trans_4_MHD                                       &
     &     (sph, comms_sph, trans_p, ipol, rj_fld, WK%trns_MHD)
!
        if (iflag_debug.eq.1) write(*,*) 'cal_nonlinear_pole_MHD'
        call cal_nonlinear_pole_MHD                                     &
     &     (sph%sph_rtp, MHD_prop%fl_prop, MHD_prop%cd_prop,            &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop,                         &
     &      WK%trns_MHD%f_trns, WK%trns_MHD%b_trns,                     &
     &      WK%trns_MHD%ncomp_rj_2_rtp, WK%trns_MHD%ncomp_rtp_2_rj,     &
     &      WK%trns_MHD%fld_pole, WK%trns_MHD%frc_pole)
      end if
!
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.eq.1) write(*,*) 'swap_phi_from_trans'
        call swap_phi_from_trans(WK%trns_SGS%ncomp_rj_2_rtp,            &
     &      sph%sph_rtp%nnod_rtp, sph%sph_rtp%nidx_rtp,                 &
     &      WK%trns_SGS%fld_rtp)
        call swap_phi_from_trans(WK%trns_SGS%ncomp_rtp_2_rj,            &
     &      sph%sph_rtp%nnod_rtp, sph%sph_rtp%nidx_rtp,                 &
     &      WK%trns_SGS%frc_rtp)
!
        if (iflag_debug.eq.1) write(*,*) 'sph_pole_trans_SGS_MHD'
        call sph_pole_trans_SGS_MHD                                     &
     &     (sph, comms_sph, trans_p, ipol, rj_fld, WK%trns_SGS)
!
        if(SGS_param%iflag_dynamic .gt. id_SGS_none) then
          if(iflag_debug.eq.1) write(*,*) 'copy_model_coefs_4_sph_snap'
          call copy_model_coefs_4_sph_snap(sph%sph_rtp,                 &
     &        WK%dynamic_SPH%ifld_sgs, WK%dynamic_SPH%wk_sgs,           &
     &        WK%trns_snap)
        end if
      end if
!
      call gradients_of_vectors_sph                                     &
     &   (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,                   &
     &    ipol, WK%trns_MHD, WK%trns_tmp, WK%WK_sph, rj_fld)
      call enegy_fluxes_4_sph_mhd(SGS_param, sph, comms_sph,            &
     &    r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol,                   &
     &    WK%trns_MHD, WK%trns_SGS, WK%trns_snap, WK%WK_sph, rj_fld)
!
      end subroutine s_lead_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine pressure_4_sph_mhd(SGS_param, sph_rj, MHD_prop,        &
     &          sph_MHD_bc, r_2nd, leg, band_p_poisson, ipol, rj_fld)
!
      use cal_sol_sph_fluid_crank
!
      use cal_sph_field_by_rotation
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use const_sph_radial_grad
      use cal_sph_rotation_of_SGS
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(band_matrices_type), intent(in) :: band_p_poisson
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2(sph_rj, r_2nd, MHD_prop, sph_MHD_bc, &
     &    leg%g_sph_rj, ipol, rj_fld)
!
!   ----  Lead SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        call cal_div_of_SGS_forces_sph_2                                &
     &     (sph_rj, r_2nd, sph_MHD_bc, leg%g_sph_rj, ipol, rj_fld)
      end if
!
      call s_const_radial_forces_on_bc                                  &
     &   (sph_rj, leg%g_sph_rj,MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,   &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C, ipol, rj_fld)
!
      call sum_div_of_forces(MHD_prop%fl_prop, ipol, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v                                    &
     &   (sph_rj, sph_MHD_bc%sph_bc_U, band_p_poisson, ipol, rj_fld)
!
      if(ipol%i_press_grad .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient                                    &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U,                         &
     &      leg%g_sph_rj, MHD_prop%fl_prop%coef_press,                  &
     &      ipol%i_press, ipol%i_press_grad, rj_fld)
      end if
!
      end subroutine pressure_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_4_sph_mhd                                 &
     &         (SGS_param, sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, &
     &          trans_p, ipol, trns_MHD, trns_SGS, trns_snap,           &
     &          WK_sph, rj_fld)
!
      use sph_transforms_snapshot
      use cal_energy_flux_rtp
      use cal_energy_flux_rj
      use cal_SGS_terms_sph_MHD
      use cal_SGS_buo_flux_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(address_4_sph_trans), intent(in) :: trns_SGS
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
!      Evaluate fields for output in spectrum space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rj'
      call s_cal_energy_flux_rj                                         &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc, ipol, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,         &
     &    ipol, rj_fld, trns_snap, WK_sph)
!
!      Evaluate fields for output in grid space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rtp'
      call s_cal_energy_flux_rtp                                        &
     &   (sph%sph_rtp, MHD_prop%fl_prop, MHD_prop%cd_prop,              &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    trns_MHD%f_trns, trns_snap%b_trns, trns_snap%f_trns,          &
     &    trns_MHD%ncomp_rtp_2_rj, trns_snap%ncomp_rj_2_rtp,            &
     &    trns_snap%ncomp_rtp_2_rj, trns_MHD%frc_rtp,                   &
     &    trns_snap%fld_rtp, trns_snap%frc_rtp)
!
!      Work of SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.eq.1) write(*,*) 'SGS_fluxes_for_snapshot'
        call SGS_fluxes_for_snapshot                                    &
     &     (sph%sph_rtp, MHD_prop%fl_prop, trns_MHD%b_trns,             &
     &      trns_SGS%f_trns, trns_snap%b_trns, trns_snap%f_trns,        &
     &      trns_MHD%ncomp_rj_2_rtp, trns_SGS%ncomp_rtp_2_rj,           &
     &      trns_snap%ncomp_rj_2_rtp, trns_snap%ncomp_rtp_2_rj,         &
     &      trns_MHD%fld_rtp, trns_SGS%frc_rtp,                         &
     &      trns_snap%fld_rtp, trns_snap%frc_rtp)
      end if
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'sph_forward_trans_snapshot_MHD'
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p, trns_snap, ipol, WK_sph, rj_fld)
      call calypso_mpi_barrier
!
      end subroutine enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine gradients_of_vectors_sph                               &
     &         (sph, comms_sph, r_2nd, sph_MHD_bc, trans_p,             &
     &          ipol, trns_MHD, trns_tmp, WK_sph, rj_fld)
!
      use sph_transforms_snapshot
      use sph_poynting_flux_smp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_velo_to_grad_v_rtp'
      call copy_velo_to_grad_v_rtp                                      &
     &   (sph%sph_rtp, trns_MHD%b_trns, trns_tmp%f_trns,                &
     &    trns_MHD%ncomp_rj_2_rtp, trns_tmp%ncomp_rtp_2_rj,             &
     &    trns_MHD%fld_rtp, trns_tmp%frc_rtp)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_tmp_snap_MHD'
      call sph_forward_trans_tmp_snap_MHD                               &
     &   (sph, comms_sph, trans_p, trns_tmp, ipol, WK_sph, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_grad_of_velocities_sph'
      call cal_grad_of_velocities_sph(sph%sph_rj, r_2nd,                &
     &    sph_MHD_bc%sph_bc_U, trans_p%leg%g_sph_rj, ipol, rj_fld)
!
      end subroutine gradients_of_vectors_sph
!
! ----------------------------------------------------------------------
!
      end module lead_fields_4_sph_mhd
