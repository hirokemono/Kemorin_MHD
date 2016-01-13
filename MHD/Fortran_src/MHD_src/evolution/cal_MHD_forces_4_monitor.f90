!
!     module cal_MHD_forces_4_monitor
!
!     Written by H. Matsui
!
!      subroutine cal_fluxes_4_monitor
!      subroutine cal_forces_4_monitor
!      subroutine cal_work_4_forces
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
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_fluxes_4_monitor
!
      use m_geometry_data
      use m_node_phys_data
      use m_physical_property
!
      use cal_fluxes
      use int_vol_coriolis_term
!
!
      if (iphys%i_h_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_h_flux)
        call cal_flux_vector(node1, nod_fld1%ntot_phys,                 &
     &      iphys%i_velo, iphys%i_temp, iphys%i_h_flux, nod_fld1%d_fld)
      else if (iphys%i_ph_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_ph_flux)
        call cal_flux_vector(node1, nod_fld1%ntot_phys, iphys%i_velo,   &
     &      iphys%i_par_temp, iphys%i_ph_flux, nod_fld1%d_fld)
      else if (iphys%i_c_flux .gt.  izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_c_flux)
        call cal_flux_vector(node1, nod_fld1%ntot_phys, iphys%i_velo,   &
     &      iphys%i_light, iphys%i_c_flux, nod_fld1%d_fld)
      else if (iphys%i_m_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_mom_flux)
        call cal_flux_tensor(node1, nod_fld1%ntot_phys,                 &
     &      iphys%i_velo, iphys%i_velo, iphys%i_m_flux, nod_fld1%d_fld)
      else if (iphys%i_maxwell .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_maxwell_t)
        call cal_maxwell_tensor(node1, ex_magne, nod_fld1%ntot_phys,    &
     &      iphys%i_magne, iphys%i_maxwell, nod_fld1%d_fld)
      else if (iphys%i_induct_t .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_induct_t)
        call cal_induction_tensor(node1, nod_fld1%ntot_phys,            &
     &      iphys%i_magne, iphys%i_velo, iphys%i_induct_t,              &
     &      nod_fld1%d_fld)
      else if (iphys%i_density .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_density)
        call set_boussinesq_density_at_node(node1, iphys, nod_fld1)
      end if
!
      end subroutine cal_fluxes_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_forces_4_monitor
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_filter_elength
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_terms_for_heat
      use cal_momentum_terms
      use cal_magnetic_terms
      use cal_induction_terms
      use cal_gradient
!
!
      if (iphys%i_h_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_heat_advect)
        call cal_terms_4_heat                                           &
     &     (iphys%i_h_advect, nod_comm, node1, ele1, surf1, fluid1,     &
     &      sf_grp1, iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,   &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_ph_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_part_h_advect)
        call cal_terms_4_heat                                           &
     &     (iphys%i_ph_advect, nod_comm, node1, ele1, surf1, fluid1,    &
     &      sf_grp1, iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,   &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_h_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_h_flux)
        call cal_terms_4_heat                                           &
     &     (iphys%i_h_flux_div, nod_comm, node1, ele1, surf1, fluid1,   &
     &      sf_grp1, iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,   &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_ph_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_ph_flux)
        call cal_terms_4_heat                                           &
     &     (iphys%i_ph_flux_div, nod_comm, node1, ele1, surf1, fluid1,  &
     &      sf_grp1, iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,   &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
!
      if (iphys%i_m_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_inertia)
        call cal_terms_4_momentum(iphys%i_m_advect,                     &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_m_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_m_flux)
        call cal_terms_4_momentum(iphys%i_m_flux_div,                   &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_maxwell_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_maxwell_t)
        call cal_terms_4_momentum(iphys%i_maxwell_div,                  &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_m_tension .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_tension)
        call cal_terms_4_momentum(iphys%i_m_tension,                    &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_lorentz .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_Lorentz)
        call cal_terms_4_momentum(iphys%i_lorentz,                      &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_buoyancy .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_buoyancy)
        call cal_terms_4_momentum(iphys%i_buoyancy,                     &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_comp_buo .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_comp_buo)
        call cal_terms_4_momentum(iphys%i_comp_buo,                     &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_filter_buo .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_filter_buo)
        call cal_terms_4_momentum(iphys%i_filter_buo,                   &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_coriolis .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_Coriolis)
        call cal_terms_4_momentum(iphys%i_coriolis,                     &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
!
      if (iphys%i_induct_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_induct_t)
        call cal_terms_4_magnetic(iphys%i_induct_div)
      end if
!
      if (iphys%i_induction .gt. izero                                  &
     &      .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_induct)
          call cal_terms_4_magnetic(iphys%i_induction)
      end if
!
      if (iphys%i_vp_induct .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_vp_induct)
        call cal_vecp_induction
      end if
!
!
      if (iphys%i_t_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_thermal_diffusion)
        call cal_thermal_diffusion(nod_comm, node1, ele1, surf1,        &
     &      fluid1, sf_grp1, iphys, jac1_3d_q, rhs_tbl1, FEM1_elen,     &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
!      if (iphys%i_t_diffuse .gt. izero) then
!        if(iflag_debug .ge. iflag_routine_msg)                         &
!     &             write(*,*) 'lead  ', trim(fhd_thermal_diffusion)
!        call cal_thermal_diffusion(nod_comm, node1, ele1, surf1,       &
!     &      fluid1, sf_grp1, iphys, jac1_3d_q, rhs_tbl1, FEM1_elen,    &
!     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
!      end if
!
      if (iphys%i_v_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_viscous)
        call cal_viscous_diffusion(nod_comm, node1, ele1, surf1,        &
     &      fluid1, sf_grp1, iphys, jac1_3d_q, rhs_tbl1, FEM1_elen,     &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if (iphys%i_vp_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_vecp_diffuse)
        call cal_vecp_diffusion
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_diffuse)
        call cal_magnetic_diffusion
      end if
!
!
      if(iphys%i_grad_vx .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead gradient of V_x'
        call choose_cal_gradient                                        &
     &     (iflag_velo_supg, (iphys%i_velo  ), iphys%i_grad_vx,         &
     &      fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,            &
     &      nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,      &
     &      rhs_tbl1, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if(iphys%i_grad_vy .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead gradient of V_y'
        call choose_cal_gradient                                        &
     &     (iflag_velo_supg, (iphys%i_velo+1), iphys%i_grad_vy,         &
     &      fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,            &
     &      nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,      &
     &      rhs_tbl1, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if(iphys%i_grad_vz .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead gradient of V_z'
        call choose_cal_gradient                                        &
     &     (iflag_velo_supg, (iphys%i_velo+2), iphys%i_grad_vz,         &
     &      fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,            &
     &      nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,      &
     &      rhs_tbl1, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
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
     &      .and. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_induct)
          call s_int_magne_induction                                    &
     &       (nod_comm, node, ele, iphys, jac_3d, rhs_tbl,              &
     &        mhd_fem_wk, fem_wk, f_nl, nod_fld)
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
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
        call cal_nod_electric_field_smp                                 &
     &     (node, coef_d_magne, nod_fld%ntot_phys, iphys%i_current,     &
     &      iphys%i_vp_induct, iphys%i_electric, nod_fld%d_fld)
      end if
!
      if (iphys%i_ujb .gt. izero) then
        call cal_tri_product_4_scalar(node, nod_fld, coef_lor,          &
     &      iphys%i_velo, iphys%i_current, iphys%i_magne, iphys%i_ujb)
      end if
!
      if (iphys%i_nega_ujb .gt. izero) then
        call cal_tri_product_4_scalar(node, nod_fld, coef_lor,          &
     &      iphys%i_velo, iphys%i_magne, iphys%i_current,               &
     &      iphys%i_nega_ujb)
      end if
!
      if (iphys%i_me_gen .gt. izero) then
        call cal_phys_dot_product(node, nod_fld,                       &
     &      iphys%i_induction, iphys%i_magne, iphys%i_me_gen)
      end if
!$omp end parallel
!
!
!
!
      if (iphys%i_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_buoyancy_flux)
        call cal_gravity_flux(node, coef_buo, iphys%i_velo,             &
     &      iphys%i_temp, iphys%i_buo_gen, nod_fld)
      end if
!
      if (iphys%i_c_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_comp_buo_flux)
        call cal_gravity_flux(node, coef_comp_buo, iphys%i_velo,        &
     &      iphys%i_light,  iphys%i_c_buo_gen, nod_fld)
      end if
!
      if (iphys%i_f_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_filter_buo_flux)
        call cal_gravity_flux(node, coef_buo, iphys%i_velo,             &
     &      iphys%i_filter_temp, iphys%i_f_buo_gen, nod_fld)
      end if
!
!
!$omp parallel
      if (iphys%i_temp_gen .gt. izero) then
        call cal_phys_product_4_scalar(node, nod_fld,                   &
     &      iphys%i_h_advect, iphys%i_temp, iphys%i_temp_gen)
      end if
!
      if (iphys%i_par_t_gen .gt. izero) then
        call cal_phys_product_4_scalar(node, nod_fld,                   &
     &      iphys%i_ph_advect, iphys%i_par_temp, iphys%i_par_t_gen)
      end if
!
!
      if (iphys%i_vis_e_diffuse .gt. izero) then
        call cal_phys_dot_product(node, nod_fld,                        &
     &      iphys%i_velo, iphys%i_v_diffuse, iphys%i_vis_e_diffuse)
      end if
!
      if (iphys%i_mag_e_diffuse .gt. izero) then
        call cal_phys_dot_product(node, nod_fld,                        &
     &      iphys%i_magne, iphys%i_b_diffuse, iphys%i_mag_e_diffuse)
      end if
!
      if (iphys%i_m_tension_wk .gt. izero) then
        call cal_phys_dot_product(node, nod_fld,                        &
     &      iphys%i_electric, iphys%i_magne, iphys%i_m_tension_wk)
      end if
!
      if (iphys%i_mag_stretch .gt. izero) then
        call cal_phys_dot_product(node, nod_fld,                        &
     &      iphys%i_grad_vx, iphys%i_magne, iphys%i_mag_stretch    )
        call cal_phys_dot_product(node, nod_fld,                        &
     &      iphys%i_grad_vy, iphys%i_magne, (iphys%i_mag_stretch+1))
        call cal_phys_dot_product(node, nod_fld,                        &
     &      iphys%i_grad_vz, iphys%i_magne, (iphys%i_mag_stretch+2))
      end if
!
      if (iphys%i_poynting .gt. izero) then
        call cal_nod_poynting_flux_smp                                  &
     &     (node, coef_d_magne, nod_fld%ntot_phys, iphys%i_current,     &
     &      iphys%i_vp_induct, iphys%i_magne, iphys%i_poynting,         &
     &      nod_fld%d_fld)
      end if
!$omp end parallel
!
!
      end subroutine cal_work_4_forces
!
!-----------------------------------------------------------------------
!
      end module cal_MHD_forces_4_monitor
