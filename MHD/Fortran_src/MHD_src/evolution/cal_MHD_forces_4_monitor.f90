!
!     module cal_MHD_forces_4_monitor
!
!     Written by H. Matsui
!
!!      subroutine cal_fluxes_4_monitor                                 &
!!     &         (node, fl_prop, cd_prop, iphys, nod_fld)
!!      subroutine cal_forces_4_monitor(dt, FEM_prm, SGS_par,           &
!!     &          nod_comm, node, ele, surf, fluid, conduct,            &
!!     &          sf_grp, fl_prop, cd_prop, ht_prop, cp_prop,           &
!!     &          nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD, fem_int, &
!!     &          FEM_elens, ifld_diff, diff_coefs, mhd_fem_wk,         &
!!     &          rhs_mat, nod_fld, ele_fld)
!!      subroutine cal_work_4_forces(FEM_prm,                           &
!!     &           nod_comm, node, ele, fl_prop, cd_prop, iphys,        &
!!     &          jacobians, rhs_tbl, mhd_fem_wk, fem_wk, f_nl, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_MHD_forces_4_monitor
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_labels
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
      use t_bc_data_MHD
      use t_MHD_boundary_data
      use t_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_fluxes_4_monitor                                   &
     &         (node, fl_prop, cd_prop, iphys, nod_fld)
!
      use cal_fluxes
      use products_nodal_fields_smp
      use int_vol_coriolis_term
!
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iphys%i_h_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_h_flux)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%i_velo, iphys%i_temp, iphys%i_h_flux, nod_fld)
!$omp end parallel
!
      else if (iphys%i_ph_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_ph_flux)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%i_velo, iphys%i_par_temp, iphys%i_ph_flux, nod_fld)
!$omp end parallel
!
      else if (iphys%i_c_flux .gt.  izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_c_flux)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%i_velo, iphys%i_light, iphys%i_c_flux, nod_fld)
!$omp end parallel
!
      else if (iphys%i_pc_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_pc_flux)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%i_velo, iphys%i_par_light, iphys%i_pc_flux, nod_fld)
!$omp end parallel
!
      else if (iphys%i_m_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_mom_flux)
        call cal_flux_tensor                                            &
     &     (iphys%i_velo, iphys%i_velo, iphys%i_m_flux, nod_fld)
      else if (iphys%i_maxwell .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_maxwell_t)
        call cal_maxwell_tensor                                         &
     &     (cd_prop%ex_magne, iphys%i_magne, iphys%i_maxwell, nod_fld)
      else if (iphys%i_induct_t .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_induct_t)
        call cal_induction_tensor                                       &
     &     (iphys%i_magne, iphys%i_velo, iphys%i_induct_t, nod_fld)
      else if (iphys%i_density .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_density)
        call set_boussinesq_density_at_node                             &
     &     (node, fl_prop, iphys, nod_fld)
      end if
!
      end subroutine cal_fluxes_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_forces_4_monitor(dt, FEM_prm, SGS_par,             &
     &          nod_comm, node, ele, surf, fluid, conduct,              &
     &          sf_grp, fl_prop, cd_prop, ht_prop, cp_prop,             &
     &          nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD, fem_int,   &
     &          FEM_elens, ifld_diff, diff_coefs, mhd_fem_wk,           &
     &          rhs_mat, nod_fld, ele_fld)
!
      use cal_terms_for_heat
      use cal_momentum_terms
      use cal_magnetic_terms
      use cal_induction_terms
      use cal_gradient
!
      real(kind = kreal), intent(in) :: dt
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(surface_group_data), intent(in) :: sf_grp
      type(fluid_property),  intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
      integer(kind = kint) :: i, i_fld, i_src
!
!
      if(iphys%i_h_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_heat_advect)
        call cal_terms_4_advect                                         &
     &     (iphys%i_h_advect, iphys%i_temp,                             &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid, ht_prop,               &
     &      nod_bcs%Tnod_bcs, iphys_ele, ele_fld, fem_int,              &
     &      mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
      if(iphys%i_ph_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_part_h_advect)
        call cal_terms_4_advect                                         &
     &     (iphys%i_ph_advect, iphys%i_par_temp,                        &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid, ht_prop,               &
     &      nod_bcs%Tnod_bcs, iphys_ele, ele_fld, fem_int,              &
     &      mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
      if(iphys%i_h_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_h_flux)
        call cal_div_of_scalar_flux                                     &
     &     (iphys%i_h_flux_div, iphys%i_h_flux,                         &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid, ht_prop,               &
     &      nod_bcs%Tnod_bcs, iphys_ele, ele_fld, fem_int,              &
     &      mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
      if(iphys%i_ph_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_ph_flux)
        call cal_div_of_scalar_flux                                     &
     &     (iphys%i_ph_flux_div, iphys%i_ph_flux,                       &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid, ht_prop,               &
     &      nod_bcs%Tnod_bcs, iphys_ele, ele_fld, fem_int,              &
     &      mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
!
      if(iphys%i_c_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_composit_advect)
        call cal_terms_4_advect                                         &
     &     (iphys%i_c_advect, iphys%i_light,                            &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid, cp_prop,               &
     &      nod_bcs%Tnod_bcs, iphys_ele, ele_fld, fem_int,              &
     &      mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
      if(iphys%i_pc_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_part_c_advect)
        call cal_terms_4_advect                                         &
     &     (iphys%i_pc_advect, iphys%i_par_light,                       &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid, cp_prop,               &
     &      nod_bcs%Tnod_bcs, iphys_ele, ele_fld, fem_int,              &
     &      mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
      if(iphys%i_c_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_c_flux)
        call cal_div_of_scalar_flux                                     &
     &     (iphys%i_c_flux_div, iphys%i_c_flux,                         &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid, cp_prop,               &
     &      nod_bcs%Tnod_bcs, iphys_ele, ele_fld, fem_int,              &
     &      mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
      if(iphys%i_pc_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_pc_flux)
        call cal_div_of_scalar_flux                                     &
     &     (iphys%i_pc_flux_div, iphys%i_pc_flux,                       &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid, cp_prop,               &
     &      nod_bcs%Tnod_bcs, iphys_ele, ele_fld, fem_int,              &
     &      mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%i_m_advect                             &
     &     .or. i_fld .eq. iphys%i_m_flux_div                           &
     &     .or. i_fld .eq. iphys%i_maxwell_div                          &
     &     .or. i_fld .eq. iphys%i_m_tension                            &
     &     .or. i_fld .eq. iphys%i_lorentz                              &
     &     .or. i_fld .eq. iphys%i_buoyancy                             &
     &     .or. i_fld .eq. iphys%i_comp_buo                             &
     &     .or. i_fld .eq. iphys%i_filter_buo                           &
     &     .or. i_fld .eq. iphys%i_coriolis) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call cal_terms_4_momentum                                     &
     &       (i_fld, ifld_diff%i_velo, ifld_diff%i_lorentz, dt,         &
     &        FEM_prm, SGS_par%model_p, SGS_par%commute_p, nod_comm,    &
     &        node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,         &
     &        surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs, iphys, iphys_ele,     &
     &        ak_MHD, fem_int, FEM_elens, diff_coefs,                   &
     &        mhd_fem_wk%mlump_fl, mhd_fem_wk, rhs_mat,                 &
     &        nod_fld, ele_fld)
        end if
      end do
!
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%i_induct_div                           &
     &     .or. (i_fld.eq.iphys%i_induction                             &
     &      .and. cd_prop%iflag_Bevo_scheme.gt.id_no_evolution)) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call cal_terms_4_magnetic                                     &
     &       (i_fld, ifld_diff%i_induction, ak_MHD%ak_d_magne, dt,      &
     &        FEM_prm, SGS_par%model_p, SGS_par%commute_p,              &
     &        nod_comm, node, ele, surf, conduct, sf_grp, cd_prop,      &
     &        nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,     &
     &        iphys, iphys_ele, ele_fld, fem_int, FEM_elens,            &
     &        diff_coefs, mhd_fem_wk%mlump_cd, mhd_fem_wk,              &
     &        rhs_mat, nod_fld)
        end if
      end do
!
!
      if (iphys%i_vp_induct .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_vp_induct)
        call cal_vecp_induction                                         &
     &     (dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,         &
     &      nod_bcs%Bnod_bcs, iphys, iphys_ele, ele_fld, fem_int,       &
     &      mhd_fem_wk%mlump_cd, mhd_fem_wk, rhs_mat, nod_fld)
      end if
!
!
      if (iphys%i_t_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_thermal_diffusion)
        call cal_thermal_diffusion                                      &
          (iphys%i_t_diffuse, iphys%i_temp, ifld_diff%i_temp,           &
     &     ak_MHD%ak_d_temp, FEM_prm%npoint_t_evo_int,                  &
     &     SGS_par%model_p, nod_comm, node, ele, surf, fluid, sf_grp,   &
     &     nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, fem_int, FEM_elens,      &
     &     diff_coefs, mhd_fem_wk%mlump_fl, rhs_mat, nod_fld)
      end if
!
      if (iphys%i_c_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_c_diffuse)
        call cal_thermal_diffusion                                      &
     &     (iphys%i_c_diffuse, iphys%i_light, ifld_diff%i_light,        &
     &      ak_MHD%ak_d_composit, FEM_prm%npoint_t_evo_int,             &
     &      SGS_par%model_p, nod_comm, node, ele, surf, fluid, sf_grp,  &
     &      nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, fem_int, FEM_elens,     &
     &      diff_coefs, mhd_fem_wk%mlump_fl, rhs_mat, nod_fld)
      end if
!
      if (iphys%i_v_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_viscous)
        call cal_viscous_diffusion                                      &
     &     (ifld_diff%i_velo, ifld_diff%i_velo, ifld_diff%i_lorentz,    &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      nod_comm, node, ele, surf, sf_grp, fluid, fl_prop,          &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,       &
     &      iphys, ak_MHD, fem_int, FEM_elens, diff_coefs,              &
     &      mhd_fem_wk%mlump_fl, rhs_mat, nod_fld)
      end if
!
      if (iphys%i_vp_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_vecp_diffuse)
        call cal_vecp_diffusion(ifld_diff%i_magne, ak_MHD%ak_d_magne,   &
     &      FEM_prm, SGS_par%model_p, nod_comm, node, ele, surf,        &
     &      sf_grp, nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, iphys, fem_int, &
     &      FEM_elens, diff_coefs, mhd_fem_wk%mlump_cd,                 &
     &      rhs_mat, nod_fld)
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_diffuse)
        call cal_magnetic_diffusion(ifld_diff%i_magne,                  &
     &      ifld_diff%i_induction, ak_MHD%ak_d_magne,                   &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      nod_comm, node, ele, surf, conduct, sf_grp,                 &
     &      nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,       &
     &      iphys, fem_int, FEM_elens, diff_coefs, rhs_mat, nod_fld)
      end if
!
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%i_grad_vx                              &
     &     .or. i_fld .eq. iphys%i_grad_vy                              &
     &     .or. i_fld .eq. iphys%i_grad_vz) then
          if(i_fld .eq. iphys%i_grad_vx) i_src = iphys%i_velo
          if(i_fld .eq. iphys%i_grad_vy) i_src = iphys%i_velo + 1
          if(i_fld .eq. iphys%i_grad_vz) i_src = iphys%i_velo + 2
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call choose_cal_gradient                                      &
     &       (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &        i_src, i_fld, fluid%istack_ele_fld_smp,                   &
     &        mhd_fem_wk%mlump_fl, nod_comm, node, ele, iphys_ele,      &
     &        ele_fld, fem_int%jcs%jac_3d, fem_int%rhs_tbl,             &
     &        rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
        end if
      end do
!
      end subroutine cal_forces_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_4_forces(FEM_prm,                             &
     &          nod_comm, node, ele, fl_prop, cd_prop, iphys,           &
     &          jacobians, rhs_tbl, mhd_fem_wk, fem_wk, f_nl, nod_fld)
!
      use buoyancy_flux
      use products_nodal_fields_smp
      use copy_nodal_fields
      use int_magne_diffusion
      use int_magne_induction
      use nodal_poynting_flux_smp
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iphys%i_induction .gt. izero                                  &
     &      .and. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_induct)
        call s_int_magne_induction(FEM_prm%npoint_poisson_int,          &
     &      nod_comm, node, ele, iphys, jacobians%jac_3d, rhs_tbl,      &
     &      mhd_fem_wk%mlump_cd, mhd_fem_wk, fem_wk, f_nl, nod_fld)
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_diffuse)
        call s_int_magne_diffusion(FEM_prm%npoint_poisson_int,          &
     &      nod_comm, node, ele, iphys, jacobians%jac_3d, rhs_tbl,      &
     &      mhd_fem_wk%mlump_cd, mhd_fem_wk, fem_wk, f_nl, nod_fld)
      end if
!
!$omp parallel
      if (iphys%i_electric .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_e_field)
        call cal_nod_electric_field_smp(node, cd_prop%coef_diffuse,     &
     &      nod_fld%ntot_phys, iphys%i_current, iphys%i_vp_induct,      &
     &      iphys%i_electric, nod_fld%d_fld)
      end if
!
      if (iphys%i_ujb .gt. izero) then
        call cal_tri_product_4_scalar                                   &
     &     (iphys%i_velo, iphys%i_current, iphys%i_magne, iphys%i_ujb,  &
     &      fl_prop%coef_lor, nod_fld)
      end if
!
      if (iphys%i_nega_ujb .gt. izero) then
        call cal_tri_product_4_scalar                                   &
     &     (iphys%i_velo, iphys%i_magne, iphys%i_current,               &
     &      iphys%i_nega_ujb, fl_prop%coef_lor, nod_fld)
      end if
!
      if (iphys%i_me_gen .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_induction, iphys%i_magne, iphys%i_me_gen, nod_fld)
      end if
!$omp end parallel
!
!
!
!
      if (iphys%i_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_buoyancy_flux)
        call cal_gravity_flux(node,                                     &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,             &
     &      iphys%i_velo,  iphys%i_temp, iphys%i_buo_gen, nod_fld)
      end if
!
      if (iphys%i_c_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_comp_buo_flux)
        call cal_gravity_flux(node,                                     &
     &      fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,        &
     &      iphys%i_velo, iphys%i_light,  iphys%i_c_buo_gen, nod_fld)
      end if
!
      if (iphys%i_f_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_filter_buo_flux)
        call cal_gravity_flux(node,                                     &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,             &
     &      iphys%i_velo, iphys%i_filter_temp, iphys%i_f_buo_gen,       &
     &      nod_fld)
      end if
!
!
!$omp parallel
      if (iphys%i_temp_gen .gt. izero) then
        call cal_phys_product_4_scalar                                  &
     &     (iphys%i_h_advect, iphys%i_temp, iphys%i_temp_gen, nod_fld)
      end if
!
      if (iphys%i_par_t_gen .gt. izero) then
        call cal_phys_product_4_scalar                                  &
     &     (iphys%i_ph_advect, iphys%i_par_temp, iphys%i_par_t_gen,     &
     &      nod_fld)
      end if
!
      if (iphys%i_par_c_gen .gt. izero) then
        call cal_phys_product_4_scalar                                  &
     &     (iphys%i_pc_advect, iphys%i_par_light, iphys%i_par_c_gen,    &
     &      nod_fld)
      end if
!
      if (iphys%i_vis_e_diffuse .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_velo, iphys%i_v_diffuse, iphys%i_vis_e_diffuse,     &
     &      nod_fld)
      end if
!
      if (iphys%i_mag_e_diffuse .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_magne, iphys%i_b_diffuse, iphys%i_mag_e_diffuse,    &
     &      nod_fld)
      end if
!
      if (iphys%i_m_tension_wk .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_electric, iphys%i_magne, iphys%i_m_tension_wk,      &
     &      nod_fld)
      end if
!
      if (iphys%i_mag_stretch .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_grad_vx, iphys%i_magne, iphys%i_mag_stretch,        &
     &      nod_fld)
        call cal_phys_dot_product                                       &
     &     (iphys%i_grad_vy, iphys%i_magne, (iphys%i_mag_stretch+1),    &
     &      nod_fld)
        call cal_phys_dot_product                                       &
     &     (iphys%i_grad_vz, iphys%i_magne, (iphys%i_mag_stretch+2),    &
     &      nod_fld)
      end if
!
      if (iphys%i_poynting .gt. izero) then
        call cal_nod_poynting_flux_smp(node, cd_prop%coef_diffuse,      &
     &      nod_fld%ntot_phys, iphys%i_current, iphys%i_vp_induct,      &
     &      iphys%i_magne, iphys%i_poynting, nod_fld%d_fld)
      end if
!$omp end parallel
!
!
      end subroutine cal_work_4_forces
!
!-----------------------------------------------------------------------
!
      end module cal_MHD_forces_4_monitor
