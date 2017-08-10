!>@file   cal_model_coefficients.f90
!!        module cal_model_coefficients
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate m odel coefficients
!!
!!@verbatim
!!      subroutine s_cal_model_coefficients(time_d, FEM_prm, SGS_par,   &
!!     &        femmesh, ele_mesh, MHD_mesh, MHD_prop, layer_tbl,       &
!!     &        nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld, fem_int,  &
!!     &        FEM_elens, filtering, wide_filtering, mk_MHD,           &
!!     &        wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter, mhd_fem_wk, &
!!     &        rhs_mat, nod_fld, Csims_FEM_MHD)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) ::   femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!
!!        type(dynamic_correlation_data), intent(inout) :: wk_cor
!!        type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!@endverbatim
!
      module cal_model_coefficients
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_layering_ele_list
      use t_MHD_finite_element_mat
      use t_MHD_mass_matricxes
      use t_finite_element_mat
      use t_work_FEM_integration
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_bc_data_MHD
      use t_MHD_boundary_data
      use t_FEM_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_model_coefficients(time_d, FEM_prm, SGS_par,     &
     &        femmesh, ele_mesh, MHD_mesh, MHD_prop, layer_tbl,         &
     &        nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld, fem_int,    &
     &        FEM_elens, filtering, wide_filtering, mk_MHD,             &
     &        wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter, mhd_fem_wk,   &
     &        rhs_mat, nod_fld, Csims_FEM_MHD)
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
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) ::   femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(layering_tbl), intent(in) :: layer_tbl
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(dynamic_SGS_flag(time_d%i_time_step, SGS_par) .ne. 0) return
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &        'set Csim', time_d%i_time_step, SGS_par%i_step_sgs_coefs
!
      if(MHD_prop%ht_prop%iflag_scheme .ne. id_no_evolution) then
        if(SGS_par%model_p%iflag_SGS_h_flux .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_sf_dynamic temp'
          call cal_sgs_sf_dynamic                                       &
     &       (FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,        &
     &        time_d%dt, SGS_par%model_p%itype_Csym_h_flux,             &
     &        SGS_par%model_p%SGS_hf_factor,                            &
     &        iphys%i_sgs_temp, iphys%i_filter_temp,                    &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_h_flux,    &
     &        Csims_FEM_MHD%ifld_sgs%i_heat_flux,                       &
     &        Csims_FEM_MHD%icomp_sgs%i_heat_flux,                      &
     &        Csims_FEM_MHD%iphys_elediff%i_velo,                       &
     &        Csims_FEM_MHD%iphys_elediff%i_filter_velo,                &
     &        SGS_par, femmesh%mesh, iphys, iphys_ele, ele_fld,         &
     &        MHD_mesh%fluid, layer_tbl, fem_int%jcs, fem_int%rhs_tbl,  &
     &        FEM_elens, filtering, Csims_FEM_MHD%sgs_coefs_nod,        &
     &        mk_MHD%mlump_fl, wk_filter, wk_cor, wk_lsq, wk_sgs,       &
     &        mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l, nod_fld,         &
     &        Csims_FEM_MHD%sgs_coefs)
!
        else if(SGS_par%model_p%iflag_SGS_h_flux                        &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &          write(*,*) 's_cal_sgs_s_flux_dynamic_simi temp'
          call s_cal_sgs_s_flux_dynamic_simi(FEM_prm%npoint_t_evo_int,  &
     &        SGS_par%model_p%itype_Csym_h_flux, iphys%i_sgs_temp,      &
     &        iphys%i_filter_temp, iphys%i_wide_fil_temp,               &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_h_flux,    &
     &        Csims_FEM_MHD%ifld_sgs%i_heat_flux,                       &
     &        Csims_FEM_MHD%icomp_sgs%i_heat_flux, SGS_par,             &
     &        femmesh%mesh, iphys, layer_tbl, fem_int%jcs,              &
     &        fem_int%rhs_tbl, filtering, wide_filtering,               &
     &        fem_int%m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs,        &
     &        rhs_mat%fem_wk, rhs_mat%f_l, nod_fld,                     &
     &        Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_hf .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_sf'
          call s_cal_diff_coef_sgs_sf                                   &
     &       (SGS_par%model_p%itype_Csym_h_flux,                        &
     &        FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,        &
     &        time_d%dt, iphys%i_sgs_temp, iphys%i_filter_temp,         &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_h_flux,    &
     &        Csims_FEM_MHD%ifld_diff%i_heat_flux,                      &
     &        Csims_FEM_MHD%icomp_sgs%i_heat_flux,                      &
     &        Csims_FEM_MHD%icomp_diff%i_heat_flux,                     &
     &        Csims_FEM_MHD%iphys_elediff%i_filter_velo,                &
     &        SGS_par, femmesh%mesh, femmesh%group, ele_mesh%surf,      &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys, iphys_ele,     &
     &        ele_fld, MHD_mesh%fluid, layer_tbl,                       &
     &        fem_int%jcs, fem_int%rhs_tbl, FEM_elens, filtering,       &
     &        Csims_FEM_MHD%sgs_coefs, mk_MHD%mlump_fl,                 &
     &        wk_filter, wk_cor, wk_lsq, wk_diff, mhd_fem_wk,           &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                       &
     &        Csims_FEM_MHD%diff_coefs)
        end if
      end if
!
!
      if(MHD_prop%cp_prop%iflag_scheme .ne. id_no_evolution) then
        if(SGS_par%model_p%iflag_SGS_c_flux .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_sf_dynamic comp'
          call cal_sgs_sf_dynamic                                       &
     &       (FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,        &
     &        time_d%dt, SGS_par%model_p%itype_Csym_c_flux,             &
     &        SGS_par%model_p%SGS_cf_factor,                            &
     &        iphys%i_sgs_composit, iphys%i_filter_comp,                &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_c_flux,    &
     &        Csims_FEM_MHD%ifld_sgs%i_comp_flux,                       &
     &        Csims_FEM_MHD%icomp_sgs%i_comp_flux,                      &
     &        Csims_FEM_MHD%iphys_elediff%i_velo,                       &
     &        Csims_FEM_MHD%iphys_elediff%i_filter_velo,                &
     &        SGS_par, femmesh%mesh, iphys,iphys_ele, ele_fld,          &
     &        MHD_mesh%fluid, layer_tbl, fem_int%jcs, fem_int%rhs_tbl,  &
     &        FEM_elens, filtering, Csims_FEM_MHD%sgs_coefs_nod,        &
     &        mk_MHD%mlump_fl, wk_filter, wk_cor, wk_lsq, wk_sgs,       &
     &        mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l, nod_fld,         &
     &        Csims_FEM_MHD%sgs_coefs)
!
        else if(SGS_par%model_p%iflag_SGS_c_flux                        &
     &                       .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &          write(*,*) 's_cal_sgs_s_flux_dynamic_simi comp'
          call s_cal_sgs_s_flux_dynamic_simi(FEM_prm%npoint_t_evo_int,  &
     &        SGS_par%model_p%itype_Csym_c_flux, iphys%i_sgs_composit,  &
     &        iphys%i_filter_comp, iphys%i_wide_fil_comp,               &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_c_flux,    &
     &        Csims_FEM_MHD%ifld_sgs%i_comp_flux,                       &
     &        Csims_FEM_MHD%icomp_sgs%i_comp_flux, SGS_par,             &
     &        femmesh%mesh, iphys, layer_tbl, fem_int%jcs,              &
     &        fem_int%rhs_tbl, filtering, wide_filtering,               &
     &        fem_int%m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs,        &
     &        rhs_mat%fem_wk, rhs_mat%f_l, nod_fld,                     &
     &        Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_hf .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_sf'
          call s_cal_diff_coef_sgs_sf                                   &
     &       (SGS_par%model_p%itype_Csym_c_flux,                        &
     &        FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,        &
     &        time_d%dt, iphys%i_sgs_composit, iphys%i_filter_comp,     &
     &        iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_c_flux,    &
     &        Csims_FEM_MHD%ifld_diff%i_comp_flux,                      &
     &        Csims_FEM_MHD%icomp_sgs%i_comp_flux,                      &
     &        Csims_FEM_MHD%icomp_diff%i_comp_flux,                     &
     &        Csims_FEM_MHD%iphys_elediff%i_filter_velo,                &
     &        SGS_par, femmesh%mesh, femmesh%group, ele_mesh%surf,      &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys, iphys_ele,     &
     &        ele_fld, MHD_mesh%fluid, layer_tbl,                       &
     &        fem_int%jcs, fem_int%rhs_tbl, FEM_elens, filtering,       &
     &        Csims_FEM_MHD%sgs_coefs, mk_MHD%mlump_fl,                 &
     &        wk_filter, wk_cor, wk_lsq, wk_diff, mhd_fem_wk,           &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                       &
     &        Csims_FEM_MHD%diff_coefs)
        end if
      end if
!
      if(MHD_prop%fl_prop%iflag_scheme .ne. id_no_evolution) then
        if (SGS_par%model_p%iflag_SGS_m_flux .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_m_flux_dynamic'
          call cal_sgs_m_flux_dynamic                                   &
     &       (Csims_FEM_MHD%ifld_sgs%i_mom_flux,                        &
     &        Csims_FEM_MHD%icomp_sgs%i_mom_flux,                       &
     &        Csims_FEM_MHD%iphys_elediff%i_velo,                       &
     &        Csims_FEM_MHD%iphys_elediff%i_filter_velo,                &
     &        time_d%dt, FEM_prm, SGS_par, femmesh%mesh,                &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%fluid,                &
     &        layer_tbl, fem_int%jcs, fem_int%rhs_tbl,                  &
     &        FEM_elens, filtering, Csims_FEM_MHD%sgs_coefs_nod,        &
     &        mk_MHD%mlump_fl, wk_filter, wk_cor, wk_lsq, wk_sgs,       &
     &        mhd_fem_wk, rhs_mat%fem_wk, nod_fld,                      &
     &        Csims_FEM_MHD%sgs_coefs)
        else if(SGS_par%model_p%iflag_SGS_m_flux                        &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 's_cal_sgs_m_flux_dynamic_simi'
          call s_cal_sgs_m_flux_dynamic_simi                            &
     &       (Csims_FEM_MHD%ifld_sgs%i_mom_flux,                        &
     &        Csims_FEM_MHD%icomp_sgs%i_mom_flux,                       &
     &        FEM_prm, SGS_par, femmesh%mesh, iphys, layer_tbl,         &
     &        fem_int%jcs, fem_int%rhs_tbl, filtering, wide_filtering,  &
     &        fem_int%m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs,        &
     &        rhs_mat%fem_wk, rhs_mat%f_l, nod_fld,                     &
     &        Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_mf .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_mf'
          call s_cal_diff_coef_sgs_mf                                   &
     &     (Csims_FEM_MHD%ifld_diff%i_mom_flux,                         &
     &      Csims_FEM_MHD%icomp_sgs%i_mom_flux,                         &
     &      Csims_FEM_MHD%icomp_diff%i_mom_flux,                        &
     &      Csims_FEM_MHD%iphys_elediff%i_filter_velo,                  &
     &      time_d%dt, FEM_prm, SGS_par, femmesh%mesh, femmesh%group,   &
     &      ele_mesh%surf, nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs, iphys,   &
     &      iphys_ele, ele_fld, MHD_mesh%fluid, layer_tbl,              &
     &      fem_int%jcs, fem_int%rhs_tbl, FEM_elens, filtering,         &
     &      Csims_FEM_MHD%sgs_coefs, mk_MHD%mlump_fl,                   &
     &      wk_filter, wk_cor, wk_lsq, wk_diff, mhd_fem_wk,             &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, &
     &      nod_fld, Csims_FEM_MHD%diff_coefs)
        end if
      end if
!
!
      if (MHD_prop%fl_prop%iflag_4_lorentz .ne. id_turn_OFF) then
!
        if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_t_dynamic'
          call cal_sgs_maxwell_t_dynamic                                &
     &       (Csims_FEM_MHD%ifld_sgs%i_lorentz,                         &
     &        Csims_FEM_MHD%icomp_sgs%i_lorentz,                        &
     &        Csims_FEM_MHD%iphys_elediff%i_magne,                      &
     &        Csims_FEM_MHD%iphys_elediff%i_filter_magne,               &
     &        time_d%dt, FEM_prm, SGS_par, femmesh%mesh,                &
     &        iphys, iphys_ele, ele_fld, MHD_mesh%fluid,                &
     &        layer_tbl, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,       &
     &        filtering, Csims_FEM_MHD%sgs_coefs_nod,                   &
     &        mk_MHD%mlump_fl, wk_filter, wk_cor, wk_lsq,               &
     &        wk_sgs, mhd_fem_wk, rhs_mat%fem_wk, nod_fld,              &
     &        Csims_FEM_MHD%sgs_coefs)
        else if(SGS_par%model_p%iflag_SGS_lorentz                       &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_dynamic_simi'
          call cal_sgs_maxwell_dynamic_simi                             &
     &      (Csims_FEM_MHD%ifld_sgs%i_lorentz,                          &
     &       Csims_FEM_MHD%icomp_sgs%i_lorentz,                         &
     &       FEM_prm, SGS_par, femmesh%mesh, iphys, layer_tbl,          &
     &       fem_int%jcs, fem_int%rhs_tbl, filtering, wide_filtering,   &
     &       fem_int%m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs,         &
     &       rhs_mat%fem_wk, rhs_mat%f_l, nod_fld,                      &
     &       Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_lorentz .eq. id_SGS_commute_ON)    &
     &   then
          if (iflag_debug.eq.1) write(*,*) 's_cal_diff_coef_sgs_mxwl'
          call s_cal_diff_coef_sgs_mxwl                                 &
     &     (Csims_FEM_MHD%ifld_diff%i_lorentz,                          &
     &      Csims_FEM_MHD%icomp_sgs%i_lorentz,                          &
     &      Csims_FEM_MHD%icomp_diff%i_lorentz,                         &
     &      Csims_FEM_MHD%iphys_elediff%i_filter_magne,                 &
     &      time_d%dt, FEM_prm, SGS_par, femmesh%mesh, femmesh%group,   &
     &      ele_mesh%surf, MHD_mesh%fluid, layer_tbl,                   &
     &      nod_bcs%Vnod_bcs, surf_bcs%Bsf_bcs, iphys,                  &
     &      iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,           &
     &      FEM_elens, filtering, Csims_FEM_MHD%sgs_coefs,              &
     &      mk_MHD%mlump_fl, wk_filter, wk_cor, wk_lsq, wk_diff,        &
     &      mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%surf_wk,                &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                         &
     &      Csims_FEM_MHD%diff_coefs)
        end if
      end if
!
!
!
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic'
          call cal_sgs_induct_t_dynamic                                 &
     &      (Csims_FEM_MHD%ifld_sgs%i_induction,                        &
     &       Csims_FEM_MHD%icomp_sgs%i_induction,                       &
     &       Csims_FEM_MHD%iphys_elediff%i_velo,                        &
     &       Csims_FEM_MHD%iphys_elediff%i_magne,                       &
     &       Csims_FEM_MHD%iphys_elediff%i_filter_velo,                 &
     &       Csims_FEM_MHD%iphys_elediff%i_filter_magne,                &
     &       time_d%dt, FEM_prm, SGS_par, femmesh%mesh, iphys,          &
     &       iphys_ele, ele_fld, MHD_mesh%conduct, MHD_prop%cd_prop,    &
     &       layer_tbl, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,        &
     &       filtering, Csims_FEM_MHD%sgs_coefs_nod,                    &
     &       mk_MHD%mlump_cd, wk_filter, wk_cor, wk_lsq, wk_sgs,        &
     &       mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l,                   &
     &       nod_fld, Csims_FEM_MHD%sgs_coefs)
        else if(SGS_par%model_p%iflag_SGS_uxb                           &
     &                            .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic_simi'
          call cal_sgs_induct_t_dynamic_simi                            &
     &       (Csims_FEM_MHD%ifld_sgs%i_induction,                       &
     &        Csims_FEM_MHD%icomp_sgs%i_induction,                      &
     &        FEM_prm, SGS_par, femmesh%mesh, iphys, layer_tbl,         &
     &        fem_int%jcs, fem_int%rhs_tbl, filtering, wide_filtering,  &
     &        fem_int%m_lump, wk_filter, wk_cor, wk_lsq, wk_sgs,        &
     &        rhs_mat%fem_wk, rhs_mat%f_l, nod_fld,                     &
     &        Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod)
        end if
!
        if(SGS_par%commute_p%iflag_c_uxb .eq. id_SGS_commute_ON) then
          if(iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_induct'
          call s_cal_diff_coef_sgs_induct                               &
     &      (Csims_FEM_MHD%ifld_diff%i_induction,                       &
     &       Csims_FEM_MHD%icomp_sgs%i_induction,                       &
     &       Csims_FEM_MHD%icomp_diff%i_induction,                      &
     &       Csims_FEM_MHD%iphys_elediff%i_filter_velo,                 &
     &       Csims_FEM_MHD%iphys_elediff%i_filter_magne,                &
     &       time_d%dt, FEM_prm, SGS_par, femmesh%mesh, femmesh%group,  &
     &       ele_mesh%surf, MHD_mesh%fluid, MHD_mesh%conduct,           &
     &       MHD_prop%cd_prop, layer_tbl, surf_bcs%Bsf_bcs, iphys,      &
     &       iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,          &
     &       FEM_elens, Csims_FEM_MHD%sgs_coefs, filtering,             &
     &       mk_MHD%mlump_cd, wk_filter, wk_cor, wk_lsq, wk_diff,       &
     &       mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%surf_wk,               &
     &       rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                        &
     &       Csims_FEM_MHD%diff_coefs)
        end if
!
      else if(MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution)  &
     &  then
!
        if(SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_uxb_dynamic'
          call cal_sgs_uxb_dynamic                                      &
     &       (Csims_FEM_MHD%ifld_sgs%i_induction,                       &
     &        Csims_FEM_MHD%icomp_sgs%i_induction,                      &
     &        Csims_FEM_MHD%iphys_elediff%i_velo,                       &
     &        Csims_FEM_MHD%iphys_elediff%i_filter_velo, time_d%dt,     &
     &        FEM_prm, SGS_par, femmesh%mesh, iphys, iphys_ele,         &
     &        ele_fld, MHD_mesh%conduct, MHD_prop%cd_prop, layer_tbl,   &
     &        fem_int%jcs, fem_int%rhs_tbl, FEM_elens, filtering,       &
     &        mk_MHD%mlump_cd, wk_filter, wk_cor, wk_lsq, wk_sgs,       &
     &        mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l,                  &
     &        nod_fld, Csims_FEM_MHD%sgs_coefs)
        else if(SGS_par%model_p%iflag_SGS_uxb                           &
     &                         .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)  write(*,*)                             &
     &                          's_cal_sgs_uxb_dynamic_simi'
          call s_cal_sgs_uxb_dynamic_simi                               &
     &       (Csims_FEM_MHD%ifld_sgs%i_induction,                       &
     &        Csims_FEM_MHD%icomp_sgs%i_induction,                      &
     &        FEM_prm, SGS_par, femmesh%mesh, iphys, layer_tbl,         &
     &        fem_int%jcs,filtering, wide_filtering, wk_filter, wk_cor, &
     &        wk_lsq, wk_sgs, nod_fld, Csims_FEM_MHD%sgs_coefs)
        end if
      end if
!
      end subroutine s_cal_model_coefficients
!
!-----------------------------------------------------------------------
!
      end module cal_model_coefficients
