!
!     module cal_MHD_forces_4_monitor
!
!     Written by H. Matsui
!
!!      subroutine cal_fluxes_4_monitor                                 &
!!     &         (node, fl_prop, cd_prop, iphys, nod_fld)
!!      subroutine cal_forces_4_monitor                                 &
!!     &         (dt, FEM_prm, SGS_par, mesh, fluid, conduct, sf_grp,   &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop, nod_bcs, surf_bcs,&
!!     &          iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int,    &
!!     &          FEM_elens, iak_diff_base, iak_diff_sgs, diff_coefs,   &
!!     &          mk_MHD, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,        &
!!     &          v_sol, SR_sig, SR_r)
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
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(base_field_address), intent(in) :: iak_diff_base
!!        type(SGS_term_address), intent(in) :: iak_diff_sgs
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_MHD_forces_4_monitor
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
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
      use t_SGS_model_addresses
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
      use t_bc_data_MHD
      use t_surface_bc_data_MHD
      use t_physical_property
      use t_vector_for_solver
      use t_solver_SR
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
      use m_base_field_labels
      use m_base_force_labels
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
      if (iphys%forces%i_h_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(heat_flux%name)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%base%i_velo, iphys%base%i_temp,                       &
     &      iphys%forces%i_h_flux, nod_fld)
!$omp end parallel
!
      else if (iphys%forces%i_ph_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ',                       &
     &                      trim(pert_heat_flux%name)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%base%i_velo, iphys%base%i_per_temp,                   &
     &      iphys%forces%i_ph_flux, nod_fld)
!$omp end parallel
!
      else if (iphys%forces%i_c_flux .gt.  izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ',                       &
     &                       trim(composite_flux%name)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%base%i_velo, iphys%base%i_light,                      &
     &      iphys%forces%i_c_flux, nod_fld)
!$omp end parallel
!
      else if (iphys%forces%i_pc_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ',                       &
     &                       trim(pert_comp_flux%name)
!$omp parallel
        call cal_phys_scalar_product_vector                             &
     &     (iphys%base%i_velo, iphys%base%i_per_light,                  &
     &      iphys%forces%i_pc_flux, nod_fld)
!$omp end parallel
!
      else if (iphys%forces%i_m_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ',                       &
     &                                 trim(momentum_flux%name)
        call cal_flux_tensor                                            &
     &     (iphys%base%i_velo, iphys%base%i_velo,                       &
     &      iphys%forces%i_m_flux, nod_fld)
      else if (iphys%forces%i_maxwell .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ',                       &
     &                                 trim(maxwell_tensor%name)
        call cal_maxwell_tensor                                         &
     &     (cd_prop%ex_magne, iphys%base%i_magne,                       &
     &      iphys%forces%i_maxwell, nod_fld)
      else if (iphys%forces%i_induct_t .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ',                       &
     &                      trim(induction_tensor%name)
        call cal_induction_tensor                                       &
     &     (iphys%base%i_magne, iphys%base%i_velo,                      &
     &      iphys%forces%i_induct_t, nod_fld)
      else if (iphys%base%i_density .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(density%name)
        call set_boussinesq_density_at_node                             &
     &     (node, fl_prop, iphys, nod_fld)
      end if
!
      end subroutine cal_fluxes_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_forces_4_monitor                                   &
     &         (dt, FEM_prm, SGS_par, mesh, fluid, conduct, sf_grp,     &
     &          fl_prop, cd_prop, ht_prop, cp_prop, nod_bcs, surf_bcs,  &
     &          iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int,      &
     &          FEM_elens, iak_diff_base, iak_diff_sgs, diff_coefs,     &
     &          mk_MHD, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,          &
     &          v_sol, SR_sig, SR_r)
!
      use m_base_force_labels
      use m_diffusion_term_labels
      use m_div_force_labels
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
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(surface_group_data), intent(in) :: sf_grp
      type(fluid_property),  intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(base_field_address), intent(in) :: iak_diff_base
      type(SGS_term_address), intent(in) :: iak_diff_sgs
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: i, i_fld
!
!
      if(iphys%forces%i_h_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(heat_advect%name)
        call cal_terms_4_advect                                         &
     &     (iphys%forces%i_h_advect, iphys%base%i_temp,                 &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      ht_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,         &
     &      fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
      if(iphys%forces%i_ph_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(pert_heat_advect%name)
        call cal_terms_4_advect                                         &
     &     (iphys%forces%i_ph_advect, iphys%base%i_per_temp,            &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      ht_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,         &
     &      fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
      if(iphys%div_forces%i_h_flux .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(div_heat_flux%name)
        call cal_div_of_scalar_flux                                     &
     &     (iphys%div_forces%i_h_flux, iphys%forces%i_h_flux,           &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      ht_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,         &
     &      fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
      if(iphys%div_forces%i_ph_flux .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(div_pert_heat_flux%name)
        call cal_div_of_scalar_flux                                     &
     &     (iphys%div_forces%i_ph_flux, iphys%forces%i_ph_flux,         &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      ht_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,         &
     &      fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
!
      if(iphys%forces%i_c_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(composition_advect%name)
        call cal_terms_4_advect                                         &
     &     (iphys%forces%i_c_advect, iphys%base%i_light,                &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      cp_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,         &
     &      fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
      if(iphys%forces%i_pc_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(pert_comp_advect%name)
        call cal_terms_4_advect                                         &
     &     (iphys%forces%i_pc_advect, iphys%base%i_per_light,           &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      cp_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,         &
     &      fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
      if(iphys%div_forces%i_c_flux .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(div_composition_flux%name)
        call cal_div_of_scalar_flux                                     &
     &     (iphys%div_forces%i_c_flux, iphys%forces%i_c_flux,           &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      cp_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,         &
     &      fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
      if(iphys%div_forces%i_pc_flux .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg) write(*,*) 'lead  ',     &
     &              trim(div_pert_composition_flux%name)
        call cal_div_of_scalar_flux                                     &
     &     (iphys%div_forces%i_pc_flux, iphys%forces%i_pc_flux,         &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      cp_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,         &
     &      fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%forces%i_m_advect                      &
     &     .or. i_fld .eq. iphys%div_forces%i_m_flux                    &
     &     .or. i_fld .eq. iphys%div_forces%i_maxwell                   &
     &     .or. i_fld .eq. iphys%forces%i_m_tension                     &
     &     .or. i_fld .eq. iphys%forces%i_lorentz                       &
     &     .or. i_fld .eq. iphys%forces%i_buoyancy                      &
     &     .or. i_fld .eq. iphys%forces%i_comp_buo                      &
     &     .or. i_fld .eq. iphys_LES%force_by_filter%i_buoyancy         &
     &     .or. i_fld .eq. iphys_LES%force_by_filter%i_comp_buo         &
     &     .or. i_fld .eq. iphys%forces%i_coriolis) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call cal_terms_4_momentum                                     &
     &       (i_fld, dt, FEM_prm, SGS_par%model_p, SGS_par%commute_p,   &
     &        mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,            &
     &        sf_grp, fluid, fl_prop, cd_prop,                          &
     &        surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,                       &
     &        iphys%base, iphys%forces, iphys%div_forces,               &
     &        iphys%diffusion, iphys_LES%filter_fld,                    &
     &        iphys_LES%force_by_filter, iphys_LES%SGS_term,            &
     &        iphys_LES%div_SGS, iphys_ele_base, ak_MHD,                &
     &        fem_int, FEM_elens, iak_diff_sgs, diff_coefs,             &
     &        mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat,                     &
     &        nod_fld, ele_fld, v_sol, SR_sig, SR_r)
        end if
      end do
!
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%div_forces%i_induct_t                  &
     &     .or. (i_fld.eq.iphys%forces%i_induction                      &
     &      .and. cd_prop%iflag_Bevo_scheme.gt.id_no_evolution)) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call cal_terms_4_magnetic(i_fld, ak_MHD%ak_d_magne, dt,       &
     &        FEM_prm, SGS_par%model_p, SGS_par%commute_p,              &
     &        mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,            &
     &        conduct, sf_grp, cd_prop,                                 &
     &        nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,     &
     &        iphys%base, iphys%forces, iphys%div_forces,               &
     &        iphys%diffusion, iphys_LES%SGS_term,                      &
     &        iphys_ele_base, ele_fld, fem_int, FEM_elens,              &
     &        diff_coefs%ak(1,iak_diff_sgs%i_SGS_induction),            &
     &        mk_MHD%mlump_cd, mhd_fem_wk, rhs_mat, nod_fld,            &
     &        v_sol, SR_sig, SR_r)
        end if
      end do
!
!
      if (iphys%forces%i_vp_induct .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(vecp_induction%name)
        call cal_vecp_induction                                         &
     &     (dt, FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, conduct,   &
     &      cd_prop, nod_bcs%Bnod_bcs, iphys, iphys_ele_base, ele_fld,  &
     &      fem_int, mk_MHD%mlump_cd, mhd_fem_wk, rhs_mat, nod_fld,     &
     &      v_sol, SR_sig, SR_r)
      end if
!
!
      if (iphys%diffusion%i_t_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(thermal_diffusion%name)
        call cal_thermal_diffusion(iphys%diffusion%i_t_diffuse,         &
     &      iphys%base%i_temp, iak_diff_base%i_temp, ak_MHD%ak_d_temp,  &
     &      FEM_prm%npoint_t_evo_int, SGS_par%model_p,                  &
     &      mesh%nod_comm, mesh%node, mesh%ele, mesh%surf, fluid,       &
     &      sf_grp, nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, fem_int,        &
     &      FEM_elens, diff_coefs%ak(1,iak_diff_base%i_temp),           &
     &      mk_MHD%mlump_fl, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      if (iphys%diffusion%i_c_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg) write(*,*) 'lead  ',     &
     &                     trim(composition_diffusion%name)
        call cal_thermal_diffusion(iphys%diffusion%i_c_diffuse,         &
     &      iphys%base%i_light, iak_diff_base%i_light,                  &
     &      ak_MHD%ak_d_composit, FEM_prm%npoint_t_evo_int,             &
     &      SGS_par%model_p, mesh%nod_comm, mesh%node, mesh%ele,        &
     &      mesh%surf, fluid, sf_grp,                                   &
     &      nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, fem_int,                &
     &      FEM_elens, diff_coefs%ak(1,iak_diff_base%i_light),          &
     &      mk_MHD%mlump_fl, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      if (iphys%diffusion%i_v_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(viscous_diffusion%name)
        call cal_viscous_diffusion                                      &
     &     (FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,              &
     &      sf_grp, fluid, fl_prop,                                     &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,       &
     &      iphys%base, iphys%diffusion, iphys_LES%SGS_term,            &
     &      iphys_LES%div_SGS, ak_MHD, fem_int, FEM_elens,              &
     &      iak_diff_base, iak_diff_sgs, diff_coefs,                    &
     &      mk_MHD%mlump_fl, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      if (iphys%diffusion%i_vp_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg) write(*,*) 'lead  ',     &
     &                     trim(vector_potential_diffusion%name)
        call cal_vecp_diffusion                                         &
     &     (ak_MHD%ak_d_magne, FEM_prm, SGS_par%model_p,                &
     &      mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,              &
     &      sf_grp, nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, iphys, fem_int, &
     &      FEM_elens, diff_coefs%Cdiff_magne%num_comp, diff_coefs%Cdiff_magne%coef, &
     &      mk_MHD%mlump_cd, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      if (iphys%diffusion%i_b_diffuse .gt. izero                        &
     &      .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(magnetic_diffusion%name)
        call cal_magnetic_diffusion(ak_MHD%ak_d_magne,                  &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      mesh%nod_comm, mesh%node, mesh%ele, mesh%surf, conduct,     &
     &      sf_grp, nod_bcs%Bnod_bcs,                                   &
     &      surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,                         &
     &      iphys%base, iphys%diffusion, iphys_LES%SGS_term,            &
     &      fem_int, FEM_elens, iak_diff_base, iak_diff_sgs,            &
     &      diff_coefs, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      end subroutine cal_forces_4_monitor
!
!-----------------------------------------------------------------------
!
      end module cal_MHD_forces_4_monitor
