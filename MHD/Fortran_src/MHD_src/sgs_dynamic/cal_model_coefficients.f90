!
!      module cal_model_coefficients
!
!      Written by H. Matsui
!
!!      subroutine s_cal_model_coefficients                             &
!!     &         (evo_V, evo_B, evo_A, evo_T, evo_C, FEM_prm, SGS_par,  &
!!     &          mesh, group, ele_mesh, MHD_mesh, layer_tbl,           &
!!     &          nod_bcs, surf_bcs, iphys, iphys_ele,                  &
!!     &          ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,            &
!!     &          rhs_tbl, FEM_elens, ifld_sgs, icomp_sgs, ifld_diff,   &
!!     &          icomp_diff, iphys_elediff, filtering, wide_filtering, &
!!     &          m_lump, wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter,   &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,               &
!!     &          nod_fld, sgs_coefs, sgs_coefs_nod, diff_coefs)
!!        type(time_evolution_params), intent(in) :: evo_V, evo_B, evo_A
!!        type(time_evolution_params), intent(in) :: evo_T, evo_C
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!         type(SGS_terms_address), intent(in) :: ifld_sgs
!!         type(SGS_terms_address), intent(in) :: icomp_sgs
!!         type(SGS_terms_address), intent(in) :: ifld_diff
!!         type(SGS_terms_address), intent(in) :: icomp_diff
!!         type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!
!!        type(dynamis_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      module cal_model_coefficients
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_physical_property
!
      use t_time_stepping_parameter
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_2d
      use t_jacobian_3d
      use t_table_FEM_const
      use t_layering_ele_list
      use t_MHD_finite_element_mat
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
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
      subroutine s_cal_model_coefficients                               &
     &         (evo_V, evo_B, evo_A, evo_T, evo_C, FEM_prm, SGS_par,    &
     &          mesh, group, ele_mesh, MHD_mesh, layer_tbl,             &
     &          nod_bcs, surf_bcs, iphys, iphys_ele,                    &
     &          ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,              &
     &          rhs_tbl, FEM_elens, ifld_sgs, icomp_sgs, ifld_diff,     &
     &          icomp_diff, iphys_elediff, filtering, wide_filtering,   &
     &          m_lump, wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter,     &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                 &
     &          nod_fld, sgs_coefs, sgs_coefs_nod, diff_coefs)
!
      use m_t_step_parameter
!
      use cal_sgs_heat_flux_dynamic
      use cal_sgs_h_flux_dynamic_simi
      use cal_diff_coef_sgs_hf
      use cal_sgs_mom_flux_dynamic
      use cal_sgs_m_flux_dynamic_simi
      use cal_diff_coef_sgs_mf
      use cal_sgs_maxwell_dynamic
      use cal_diff_coef_sgs_mxwl
      use cal_sgs_induction_dynamic
      use cal_diff_coef_sgs_induct
      use cal_sgs_uxb_dynamic_simi
!
      type(time_evolution_params), intent(in) :: evo_V, evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: icomp_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(mod(i_step_MHD, i_step_sgs_coefs) .ne. 0) return
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &            'set Csim', i_step_MHD, i_step_sgs_coefs
!
      if(evo_T%iflag_scheme .ne. id_no_evolution) then
        if(SGS_par%model_p%iflag_SGS_h_flux .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_sf_dynamic temp'
          call cal_sgs_sf_dynamic                                       &
     &       (FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,        &
     &        SGS_par%model_p%itype_Csym_h_flux,                        &
     &        SGS_par%model_p%SGS_hf_factor,                            &
     &        iphys%i_sgs_temp, iphys%i_filter_temp,                    &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_h_flux,    &
     &        ifld_sgs%i_heat_flux, icomp_sgs%i_heat_flux,              &
     &        iphys_elediff%i_velo, iphys_elediff%i_filter_velo,        &
     &        SGS_par, mesh%nod_comm, mesh%node, mesh%ele, iphys,       &
     &        iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,            &
     &        jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,        &
     &        sgs_coefs_nod, wk_filter, wk_cor, wk_lsq, wk_sgs,         &
     &        mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
!
        else if(SGS_par%model_p%iflag_SGS_h_flux                        &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &          write(*,*) 's_cal_sgs_s_flux_dynamic_simi temp'
          call s_cal_sgs_s_flux_dynamic_simi(FEM_prm%npoint_t_evo_int,  &
     &        SGS_par%model_p%itype_Csym_h_flux, iphys%i_sgs_temp,      &
     &        iphys%i_filter_temp, iphys%i_wide_fil_temp,               &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_h_flux,    &
     &        ifld_sgs%i_heat_flux, icomp_sgs%i_heat_flux, SGS_par,     &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, rhs_tbl, filtering, wide_filtering,   &
     &        m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,        &
     &        f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_hf .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_sf'
          call s_cal_diff_coef_sgs_sf                                   &
     &       (SGS_par%model_p%itype_Csym_h_flux,                        &
     &        FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,        &
     &        iphys%i_sgs_temp, iphys%i_filter_temp,                    &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_h_flux,    &
     &        ifld_diff%i_heat_flux, icomp_sgs%i_heat_flux,             &
     &        icomp_diff%i_heat_flux, iphys_elediff%i_filter_velo,      &
     &        SGS_par, mesh%nod_comm, mesh%node, mesh%ele,              &
     &        ele_mesh%surf, group%surf_grp,                            &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,                       &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,                &
     &        FEM_elens, filtering, sgs_coefs, wk_filter,               &
     &        wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,     &
     &        f_l, f_nl, nod_fld, diff_coefs)
        end if
      end if
!
!
      if(evo_C%iflag_scheme .ne. id_no_evolution) then
        if(SGS_par%model_p%iflag_SGS_c_flux .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_sf_dynamic comp'
          call cal_sgs_sf_dynamic                                       &
     &       (FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,        &
     &        SGS_par%model_p%itype_Csym_c_flux,                        &
     &        SGS_par%model_p%SGS_cf_factor,                            &
     &        iphys%i_sgs_composit, iphys%i_filter_comp,                &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_c_flux,    &
     &        ifld_sgs%i_comp_flux, icomp_sgs%i_comp_flux,              &
     &        iphys_elediff%i_velo, iphys_elediff%i_filter_velo,        &
     &        SGS_par, mesh%nod_comm, mesh%node, mesh%ele, iphys,       &
     &        iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,            &
     &        jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,        &
     &        sgs_coefs_nod, wk_filter, wk_cor, wk_lsq, wk_sgs,         &
     &        mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
!
        else if(SGS_par%model_p%iflag_SGS_c_flux                        &
     &                       .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &          write(*,*) 's_cal_sgs_s_flux_dynamic_simi comp'
          call s_cal_sgs_s_flux_dynamic_simi(FEM_prm%npoint_t_evo_int,  &
     &        SGS_par%model_p%itype_Csym_c_flux, iphys%i_sgs_composit,  &
     &        iphys%i_filter_comp, iphys%i_wide_fil_comp,               &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_c_flux,    &
     &        ifld_sgs%i_comp_flux, icomp_sgs%i_comp_flux, SGS_par,     &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, rhs_tbl, filtering, wide_filtering,   &
     &        m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,        &
     &        f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_hf .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_sf'
          call s_cal_diff_coef_sgs_sf                                   &
     &       (SGS_par%model_p%itype_Csym_c_flux,                        &
     &        FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,        &
     &        iphys%i_sgs_composit, iphys%i_filter_comp,                &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_c_flux,    &
     &        ifld_diff%i_comp_flux, icomp_sgs%i_comp_flux,             &
     &        icomp_diff%i_comp_flux, iphys_elediff%i_filter_velo,      &
     &        SGS_par, mesh%nod_comm, mesh%node, mesh%ele,              &
     &        ele_mesh%surf, group%surf_grp,                            &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,                       &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,                &
     &        FEM_elens, filtering, sgs_coefs, wk_filter,               &
     &        wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,     &
     &        f_l, f_nl, nod_fld, diff_coefs)
        end if
      end if
!
      if(evo_V%iflag_scheme .ne. id_no_evolution) then
        if (SGS_par%model_p%iflag_SGS_m_flux .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_m_flux_dynamic'
          call cal_sgs_m_flux_dynamic                                   &
     &       (ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                &
     &        iphys_elediff%i_velo, iphys_elediff%i_filter_velo,        &
     &        FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,     &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,        &
     &        sgs_coefs_nod, wk_filter, wk_cor, wk_lsq, wk_sgs,         &
     &        mhd_fem_wk, fem_wk, nod_fld, sgs_coefs)
        else if(SGS_par%model_p%iflag_SGS_m_flux                        &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 's_cal_sgs_m_flux_dynamic_simi'
          call s_cal_sgs_m_flux_dynamic_simi                            &
     &       (ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                &
     &        FEM_prm, SGS_par, mesh%nod_comm, mesh%node,               &
     &        mesh%ele, iphys, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,  &
     &        filtering, wide_filtering, m_lump, wk_filter,             &
     &        wk_cor, wk_lsq, wk_sgs, fem_wk, f_l, nod_fld,             &
     &        sgs_coefs, sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_mf .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_mf'
          call s_cal_diff_coef_sgs_mf                                   &
     &     (ifld_diff%i_mom_flux, icomp_sgs%i_mom_flux,                 &
     &      icomp_diff%i_mom_flux, iphys_elediff%i_filter_velo,         &
     &      FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,       &
     &      ele_mesh%surf, group%surf_grp,                              &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs,                         &
     &      iphys, iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,       &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,                  &
     &      FEM_elens, filtering, sgs_coefs, wk_filter,                 &
     &      wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,       &
     &      f_l, f_nl, nod_fld, diff_coefs)
        end if
      end if
!
!
      if (fl_prop1%iflag_4_lorentz .ne. id_turn_OFF) then
!
        if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_t_dynamic'
          call cal_sgs_maxwell_t_dynamic                                &
     &       (ifld_sgs%i_lorentz, icomp_sgs%i_lorentz,                  &
     &        iphys_elediff%i_magne, iphys_elediff%i_filter_magne,      &
     &        FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,     &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,        &
     &        sgs_coefs_nod, wk_filter, wk_cor, wk_lsq, wk_sgs,         &
     &        mhd_fem_wk, fem_wk, nod_fld, sgs_coefs)
        else if(SGS_par%model_p%iflag_SGS_lorentz                       &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_dynamic_simi'
          call cal_sgs_maxwell_dynamic_simi                             &
     &      (ifld_sgs%i_lorentz, icomp_sgs%i_lorentz, FEM_prm, SGS_par, &
     &       mesh%nod_comm, mesh%node, mesh%ele, iphys, layer_tbl,      &
     &       jac_3d_q, jac_3d_l, rhs_tbl, filtering, wide_filtering,    &
     &       m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,         &
     &       f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_lorentz .eq. id_SGS_commute_ON)    &
     &   then
          if (iflag_debug.eq.1) write(*,*) 's_cal_diff_coef_sgs_mxwl'
          call s_cal_diff_coef_sgs_mxwl                                 &
     &     (ifld_diff%i_lorentz, icomp_sgs%i_lorentz,                   &
     &      icomp_diff%i_lorentz, iphys_elediff%i_filter_magne,         &
     &      FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,       &
     &      ele_mesh%surf, MHD_mesh%fluid, layer_tbl, group%surf_grp,   &
     &      nod_bcs%Vnod_bcs, surf_bcs%Bsf_bcs, iphys,                  &
     &      iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,       &
     &      rhs_tbl, FEM_elens, filtering, sgs_coefs, wk_filter,        &
     &      wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,       &
     &      f_l, f_nl, nod_fld, diff_coefs)
        end if
      end if
!
!
!
      if(evo_B%iflag_scheme .gt. id_no_evolution) then
        if(SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic'
          call cal_sgs_induct_t_dynamic                                 &
     &      (ifld_sgs%i_induction, icomp_sgs%i_induction,               &
     &       iphys_elediff%i_velo, iphys_elediff%i_magne,               &
     &       iphys_elediff%i_filter_velo, iphys_elediff%i_filter_magne, &
     &       FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,      &
     &       iphys, iphys_ele, ele_fld, MHD_mesh%conduct, cd_prop1,     &
     &       layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens,         &
     &       filtering, sgs_coefs_nod, wk_filter, wk_cor, wk_lsq,       &
     &       wk_sgs, mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
        else if(SGS_par%model_p%iflag_SGS_uxb                           &
     &                            .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic_simi'
          call cal_sgs_induct_t_dynamic_simi                            &
     &       (ifld_sgs%i_induction, icomp_sgs%i_induction,              &
     &        FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,     &
     &        iphys, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,            &
     &        filtering, wide_filtering, m_lump, wk_filter,             &
     &        wk_cor, wk_lsq, wk_sgs, fem_wk, f_l, nod_fld,             &
     &        sgs_coefs, sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_uxb .eq. id_SGS_commute_ON) then
          if(iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_induct'
          call s_cal_diff_coef_sgs_induct(ifld_diff%i_induction,        &
     &       icomp_sgs%i_induction, icomp_diff%i_induction,             &
     &       iphys_elediff%i_filter_velo, iphys_elediff%i_filter_magne, &
     &       FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,      &
     &       ele_mesh%surf, MHD_mesh%fluid, MHD_mesh%conduct, cd_prop1, &
     &       layer_tbl, group%surf_grp, surf_bcs%Bsf_bcs, iphys,        &
     &       iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,      &
     &       rhs_tbl, FEM_elens, sgs_coefs, filtering, wk_filter,       &
     &       wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,      &
     &       f_l, f_nl, nod_fld, diff_coefs)
        end if
!
      else if(evo_A%iflag_scheme .gt. id_no_evolution) then
!
        if(SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_uxb_dynamic'
          call cal_sgs_uxb_dynamic                                      &
     &       (ifld_sgs%i_induction, icomp_sgs%i_induction,              &
     &        iphys_elediff%i_velo, iphys_elediff%i_filter_velo,        &
     &        FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,     &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%conduct, cd_prop1,    &
     &        layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens,        &
     &        filtering, wk_filter, wk_cor, wk_lsq, wk_sgs, mhd_fem_wk, &
     &        fem_wk, f_l, nod_fld, sgs_coefs)
        else if(SGS_par%model_p%iflag_SGS_uxb                           &
     &                         .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)  write(*,*)                             &
     &                          's_cal_sgs_uxb_dynamic_simi'
          call s_cal_sgs_uxb_dynamic_simi                               &
     &       (ifld_sgs%i_induction, icomp_sgs%i_induction,              &
     &        FEM_prm, SGS_par, mesh%nod_comm, mesh%node, mesh%ele,     &
     &        iphys, layer_tbl, jac_3d_q, jac_3d_l,                     &
     &        filtering, wide_filtering, wk_filter,                     &
     &        wk_cor, wk_lsq, wk_sgs, nod_fld, sgs_coefs)
        end if
      end if
!
      end subroutine s_cal_model_coefficients
!
!-----------------------------------------------------------------------
!
      end module cal_model_coefficients
