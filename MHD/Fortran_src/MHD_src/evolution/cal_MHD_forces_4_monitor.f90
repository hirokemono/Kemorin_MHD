!
!     module cal_MHD_forces_4_monitor
!
!     Written by H. Matsui
!
!!      subroutine cal_fluxes_4_monitor(node, iphys, nod_fld)
!!      subroutine cal_forces_4_monitor                                 &
!!     &         (nod_comm, node, ele, surf, fluid, conduct, sf_grp,    &
!!     &          nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,          &
!!     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,               &
!!     &          ifld_diff, diff_coefs, m_lump, mhd_fem_wk, fem_wk,    &
!!     &          surf_wk, f_l, f_nl, nod_fld, ele_fld)
!!      subroutine cal_work_4_forces                                    &
!!     &         (nod_comm, node, ele, iphys, jac_3d, rhs_tbl,          &
!!     &          mhd_fem_wk, fem_wk, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
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
      use m_control_parameter
      use m_phys_labels
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
      use t_bc_data_MHD
      use t_MHD_boundary_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_fluxes_4_monitor(node, iphys, nod_fld)
!
      use m_physical_property
!
      use cal_fluxes
      use products_nodal_fields_smp
      use int_vol_coriolis_term
!
      type(node_data), intent(in) :: node
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
      else if (iphys%i_ph_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_ph_flux)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%i_velo, iphys%i_par_temp, iphys%i_ph_flux, nod_fld)
!$omp end parallel
      else if (iphys%i_c_flux .gt.  izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_c_flux)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%i_velo, iphys%i_light, iphys%i_c_flux, nod_fld)
!$omp end parallel
      else if (iphys%i_m_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_mom_flux)
        call cal_flux_tensor                                            &
     &     (iphys%i_velo, iphys%i_velo, iphys%i_m_flux, nod_fld)
      else if (iphys%i_maxwell .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_maxwell_t)
        call cal_maxwell_tensor                                         &
     &     (cd_prop1%ex_magne, iphys%i_magne, iphys%i_maxwell, nod_fld)
      else if (iphys%i_induct_t .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_induct_t)
        call cal_induction_tensor                                       &
     &     (iphys%i_magne, iphys%i_velo, iphys%i_induct_t, nod_fld)
      else if (iphys%i_density .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_density)
        call set_boussinesq_density_at_node(node, iphys, nod_fld)
      end if
!
      end subroutine cal_fluxes_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_forces_4_monitor                                   &
     &         (nod_comm, node, ele, surf, fluid, conduct, sf_grp,      &
     &          nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,            &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                 &
     &          ifld_diff, diff_coefs, m_lump, mhd_fem_wk, fem_wk,      &
     &          surf_wk, f_l, f_nl, nod_fld, ele_fld)
!
      use cal_terms_for_heat
      use cal_momentum_terms
      use cal_magnetic_terms
      use cal_induction_terms
      use cal_gradient
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
      integer(kind = kint) :: i, i_fld, i_src
!
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%i_h_advect                             &
     &     .or. i_fld .eq. iphys%i_ph_advect                            &
     &     .or. i_fld .eq. iphys%i_h_flux_div                           &
     &     .or. i_fld .eq. iphys%i_ph_flux_div) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call cal_terms_4_heat                                         &
     &       (i_fld, ifld_diff%i_heat_flux, ak_MHD%ak_d_temp,           &
     &        nod_comm, node, ele, surf, fluid, sf_grp,                 &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys,                &
     &        iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,          &
     &        FEM_elens, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,       &
     &        f_l, f_nl, nod_fld)
        end if
      end do
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
     &       (i_fld, ifld_diff%i_velo, ifld_diff%i_lorentz,             &
     &        nod_comm, node, ele, surf, fluid, sf_grp,                 &
     &        surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs, iphys, iphys_ele,     &
     &        ak_MHD, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,           &
     &        diff_coefs, mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,       &
     &        nod_fld, ele_fld)
        end if
      end do
!
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%i_induct_div                           &
     &     .or. (i_fld.eq.iphys%i_induction                             &
     &           .and. evo_magne%iflag_scheme.gt.id_no_evolution)) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call cal_terms_4_magnetic                                     &
     &       (i_fld, ifld_diff%i_induction, ak_MHD%ak_d_magne,          &
     &        nod_comm, node, ele, surf, conduct, sf_grp,               &
     &        nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,     &
     &        iphys, iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,   &
     &        FEM_elens, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,       &
     &        f_l, f_nl, nod_fld)
        end if
      end do
!
!
      if (iphys%i_vp_induct .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_vp_induct)
        call cal_vecp_induction(nod_comm, node, ele, conduct,           &
     &       nod_bcs%Bnod_bcs, iphys, iphys_ele, ele_fld, jac_3d,       &
     &       rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
!
      if (iphys%i_t_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_thermal_diffusion)
        call cal_thermal_diffusion                                      &
     &     (ifld_diff%i_heat_flux, ifld_diff%i_temp, ak_MHD%ak_d_temp,  &
     &      nod_comm, node, ele, surf, fluid, sf_grp,                   &
     &      nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys,                  &
     &      jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,         &
     &      mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
      end if
!
!      if (iphys%i_t_diffuse .gt. izero) then
!        if(iflag_debug .ge. iflag_routine_msg)                         &
!     &             write(*,*) 'lead  ', trim(fhd_thermal_diffusion)
!        call cal_thermal_diffusion                                     &
!     &     (ifld_diff%i_heat_flux, ifld_diff%i_temp, ak_MHD%ak_d_temp, &
!     &      nod_comm, node, ele, surf, fluid, sf_grp,                  &
!     &      nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys,                 &
!     &      jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,        &
!     &      mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!      end if
!
      if (iphys%i_v_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_viscous)
        call cal_viscous_diffusion                                      &
     &     (ifld_diff%i_velo, ifld_diff%i_velo, ifld_diff%i_lorentz,    &
     &      nod_comm, node, ele, surf, fluid, sf_grp,                   &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,       &
     &      iphys, ak_MHD, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,      &
     &      diff_coefs, mhd_fem_wk, fem_wk, surf_wk,                    &
     &      f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_vp_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_vecp_diffuse)
        call cal_vecp_diffusion(ifld_diff%i_magne, ak_MHD%ak_d_magne,   &
     &      nod_comm, node, ele, surf, sf_grp,                          &
     &      nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, iphys,                  &
     &      jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,         &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. evo_magne%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_diffuse)
        call cal_magnetic_diffusion(ifld_diff%i_magne,                  &
     &      ifld_diff%i_induction, ak_MHD%ak_d_magne,                   &
     &      nod_comm, node, ele, surf, conduct, sf_grp,                 &
     &      nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,       &
     &      iphys, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,  &
     &      m_lump, fem_wk, surf_wk, f_l, f_nl, nod_fld)
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
          call choose_cal_gradient(iflag_velo_supg, i_src, i_fld,       &
     &        fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,            &
     &        nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,          &
     &        rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
        end if
      end do
!
      end subroutine cal_forces_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_4_forces                                      &
     &         (nod_comm, node, ele, iphys, jac_3d, rhs_tbl,            &
     &          mhd_fem_wk, fem_wk, f_nl, nod_fld)
!
      use m_physical_property
!
      use buoyancy_flux
      use products_nodal_fields_smp
      use copy_nodal_fields
      use int_magne_diffusion
      use int_magne_induction
      use nodal_poynting_flux_smp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iphys%i_induction .gt. izero                                  &
     &      .and. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_induct)
          call s_int_magne_induction                                    &
     &       (nod_comm, node, ele, iphys, jac_3d, rhs_tbl,              &
     &        mhd_fem_wk, fem_wk, f_nl, nod_fld)
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_diffuse)
        call s_int_magne_diffusion                                      &
     &     (nod_comm, node, ele, iphys, jac_3d, rhs_tbl,                &
     &      mhd_fem_wk, fem_wk, f_nl, nod_fld)
      end if
!
!$omp parallel
      if (iphys%i_electric .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_e_field)
        call cal_nod_electric_field_smp(node, cd_prop1%coef_diffuse,    &
     &      nod_fld%ntot_phys, iphys%i_current, iphys%i_vp_induct,      &
     &      iphys%i_electric, nod_fld%d_fld)
      end if
!
      if (iphys%i_ujb .gt. izero) then
        call cal_tri_product_4_scalar                                   &
     &     (iphys%i_velo, iphys%i_current, iphys%i_magne, iphys%i_ujb,  &
     &      fl_prop1%coef_lor, nod_fld)
      end if
!
      if (iphys%i_nega_ujb .gt. izero) then
        call cal_tri_product_4_scalar                                   &
     &     (iphys%i_velo, iphys%i_magne, iphys%i_current,               &
     &      iphys%i_nega_ujb, fl_prop1%coef_lor, nod_fld)
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
     &      fl_prop1%i_grav, fl_prop1%coef_buo, fl_prop1%grav,          &
     &      iphys%i_velo,  iphys%i_temp, iphys%i_buo_gen, nod_fld)
      end if
!
      if (iphys%i_c_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_comp_buo_flux)
        call cal_gravity_flux(node,                                     &
     &      fl_prop1%i_grav, fl_prop1%coef_comp_buo, fl_prop1%grav,     &
     &      iphys%i_velo, iphys%i_light,  iphys%i_c_buo_gen, nod_fld)
      end if
!
      if (iphys%i_f_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_filter_buo_flux)
        call cal_gravity_flux(node,                                     &
     &      fl_prop1%i_grav, fl_prop1%coef_buo, fl_prop1%grav,          &
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
        call cal_nod_poynting_flux_smp(node, cd_prop1%coef_diffuse,     &
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
