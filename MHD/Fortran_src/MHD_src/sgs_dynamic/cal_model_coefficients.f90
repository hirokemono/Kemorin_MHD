!
!      module cal_model_coefficients
!
!      Written by H. Matsui
!
!!      subroutine s_cal_model_coefficients(mesh, group, ele_mesh,      &
!!     &          MHD_mesh, layer_tbl, nod_bcs, surf_bcs, iphys,        &
!!     &          iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q, &
!!     &          rhs_tbl, FEM_elens, ifld_sgs, icomp_sgs, ifld_diff,   &
!!     &          icomp_diff, iphys_elediff, filtering, wide_filtering, &
!!     &          m_lump, wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter,   &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,               &
!!     &          nod_fld, sgs_coefs, sgs_coefs_nod, diff_coefs)
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
      use m_control_parameter
!
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
      subroutine s_cal_model_coefficients(mesh, group, ele_mesh,        &
     &          MHD_mesh, layer_tbl, nod_bcs, surf_bcs, iphys,          &
     &          iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,   &
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
      if(iflag_dynamic_SGS .eq. id_SGS_DYNAMIC_OFF) return
      if(mod(i_step_MHD, i_step_sgs_coefs) .ne. 0) return
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &            'set Csim', i_step_MHD, i_step_sgs_coefs
!
      if(iflag_t_evo_4_temp .ne. id_no_evolution) then
        if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_hf_dynamic'
          call cal_sgs_hf_dynamic                                       &
     &       (ifld_sgs%i_heat_flux, icomp_sgs%i_heat_flux,              &
     &        iphys_elediff%i_velo, iphys_elediff%i_filter_velo,        &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys,                &
     &        iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,            &
     &        jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,        &
     &        sgs_coefs_nod, wk_filter, wk_cor, wk_lsq, wk_sgs,         &
     &        mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
!
        else if (iflag_SGS_heat .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &          write(*,*) 's_cal_sgs_h_flux_dynamic_simi'
          call s_cal_sgs_h_flux_dynamic_simi                            &
     &       (ifld_sgs%i_heat_flux, icomp_sgs%i_heat_flux,              &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, rhs_tbl, filtering, wide_filtering,   &
     &        m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,        &
     &        f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
        end if
!
        if ( iflag_commute_heat .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_hf'
          call s_cal_diff_coef_sgs_hf                                   &
     &       (ifld_diff%i_heat_flux, icomp_sgs%i_heat_flux,             &
     &        icomp_diff%i_heat_flux, iphys_elediff%i_filter_velo,      &
     &        mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,        &
     &        group%surf_grp, nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,       &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,                &
     &        FEM_elens, filtering, sgs_coefs, wk_filter,               &
     &        wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,     &
     &        f_l, f_nl, nod_fld, diff_coefs)
        end if
      end if
!
!
      if(iflag_t_evo_4_velo .ne. id_no_evolution) then
!
        if (iflag_SGS_inertia .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_m_flux_dynamic'
          call cal_sgs_m_flux_dynamic                                   &
     &       (ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                &
     &        iphys_elediff%i_velo, iphys_elediff%i_filter_velo,        &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys,                &
     &        iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,            &
     &        jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,        &
     &        sgs_coefs_nod, wk_filter, wk_cor, wk_lsq, wk_sgs,         &
     &        mhd_fem_wk, fem_wk, nod_fld, sgs_coefs)
        else if (iflag_SGS_inertia .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 's_cal_sgs_m_flux_dynamic_simi'
          call s_cal_sgs_m_flux_dynamic_simi                            &
     &       (ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, rhs_tbl, filtering, wide_filtering,   &
     &        m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,        &
     &        f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
        end if
!
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_mf'
          call s_cal_diff_coef_sgs_mf                                   &
     &       (ifld_diff%i_mom_flux, icomp_sgs%i_mom_flux,               &
     &        icomp_diff%i_mom_flux, iphys_elediff%i_filter_velo,       &
     &        mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,        &
     &        group%surf_grp, nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs,       &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,                &
     &        FEM_elens, filtering, sgs_coefs, wk_filter,               &
     &        wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,     &
     &        f_l, f_nl, nod_fld, diff_coefs)
        end if
      end if
!
!
      if (iflag_4_lorentz .ne. id_turn_OFF) then
!
        if (iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_t_dynamic'
          call cal_sgs_maxwell_t_dynamic                                &
     &       (ifld_sgs%i_lorentz, icomp_sgs%i_lorentz,                  &
     &        iphys_elediff%i_magne, iphys_elediff%i_filter_magne,      &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys,                &
     &        iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,            &
     &        jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,        &
     &        sgs_coefs_nod, wk_filter, wk_cor, wk_lsq, wk_sgs,         &
     &        mhd_fem_wk, fem_wk, nod_fld, sgs_coefs)
        else if (iflag_SGS_lorentz .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_dynamic_simi'
          call cal_sgs_maxwell_dynamic_simi                             &
     &       (ifld_sgs%i_lorentz, icomp_sgs%i_lorentz,                  &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, rhs_tbl, filtering, wide_filtering,   &
     &        m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,        &
     &        f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
        end if
!
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1) write(*,*) 's_cal_diff_coef_sgs_mxwl'
          call s_cal_diff_coef_sgs_mxwl                                 &
     &       (ifld_diff%i_lorentz, icomp_sgs%i_lorentz,                 &
     &        icomp_diff%i_lorentz, iphys_elediff%i_filter_magne,       &
     &        mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,        &
     &        MHD_mesh%fluid, layer_tbl, group%surf_grp,                &
     &        nod_bcs%Vnod_bcs, surf_bcs%Bsf_bcs, iphys,                &
     &        iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,     &
     &        rhs_tbl, FEM_elens, filtering, sgs_coefs, wk_filter,      &
     &        wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,     &
     &        f_l, f_nl, nod_fld, diff_coefs)
        end if
      end if
!
!
!
      if(iflag_t_evo_4_magne .gt. id_no_evolution) then
        if(iflag_SGS_induction .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic'
          call cal_sgs_induct_t_dynamic                                 &
     &      (ifld_sgs%i_induction, icomp_sgs%i_induction,               &
     &       iphys_elediff%i_velo, iphys_elediff%i_magne,               &
     &       iphys_elediff%i_filter_velo, iphys_elediff%i_filter_magne, &
     &       mesh%nod_comm, mesh%node, mesh%ele, iphys,                 &
     &       iphys_ele, ele_fld, MHD_mesh%conduct, layer_tbl,           &
     &       jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,         &
     &       sgs_coefs_nod, wk_filter, wk_cor, wk_lsq, wk_sgs,          &
     &       mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
        else if(iflag_SGS_induction .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic_simi'
          call cal_sgs_induct_t_dynamic_simi                            &
     &       (ifld_sgs%i_induction, icomp_sgs%i_induction,              &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, rhs_tbl, filtering, wide_filtering,   &
     &         m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,       &
     &        f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
        end if
!
        if (iflag_commute_induction .eq. id_SGS_commute_ON) then
          if(iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_induct'
          call s_cal_diff_coef_sgs_induct(ifld_diff%i_induction,        &
     &       icomp_sgs%i_induction, icomp_diff%i_induction,             &
     &       iphys_elediff%i_filter_velo, iphys_elediff%i_filter_magne, &
     &       mesh%nod_comm, mesh%node, mesh%ele,                        &
     &       ele_mesh%surf, MHD_mesh%fluid, MHD_mesh%conduct,           &
     &       layer_tbl, group%surf_grp, surf_bcs%Bsf_bcs, iphys,        &
     &       iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,      &
     &       rhs_tbl, FEM_elens, sgs_coefs, filtering, wk_filter,       &
     &       wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,      &
     &       f_l, f_nl, nod_fld, diff_coefs)
        end if
!
      else if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
!
        if(iflag_SGS_induction .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_uxb_dynamic'
          call cal_sgs_uxb_dynamic                                      &
     &       (ifld_sgs%i_induction, icomp_sgs%i_induction,              &
     &        iphys_elediff%i_velo, iphys_elediff%i_filter_velo,        &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys,                &
     &        iphys_ele, ele_fld, MHD_mesh%conduct, layer_tbl,          &
     &        jac_3d_q, jac_3d_l, rhs_tbl, FEM_elens, filtering,        &
     &        wk_filter, wk_cor, wk_lsq, wk_sgs, mhd_fem_wk, fem_wk,    &
     &        f_l, nod_fld, sgs_coefs)
        else if(iflag_SGS_induction .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)  write(*,*)                             &
     &                          's_cal_sgs_uxb_dynamic_simi'
          call s_cal_sgs_uxb_dynamic_simi                               &
     &       (ifld_sgs%i_induction, icomp_sgs%i_induction,              &
     &        mesh%nod_comm, mesh%node, mesh%ele, iphys, layer_tbl,     &
     &        jac_3d_q, jac_3d_l, filtering, wide_filtering, wk_filter, &
     &        wk_cor, wk_lsq, wk_sgs, nod_fld, sgs_coefs)
        end if
!
      end if
!
      end subroutine s_cal_model_coefficients
!
!-----------------------------------------------------------------------
!
      end module cal_model_coefficients
