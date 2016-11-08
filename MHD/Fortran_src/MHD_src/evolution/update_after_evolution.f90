!>@file   update_after_evolution.f90
!!        module update_after_evolution
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine fields_evolution(mesh, group, ele_mesh, MHD_mesh,    &
!!     &        nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,            &
!!     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
!!     &        rhs_tbl, FEM_elens, ifld_sgs, icomp_sgs, ifld_diff,     &
!!     &        icomp_diff, iphys_elediff, sgs_coefs_nod,               &
!!     &        filtering, wide_filtering, layer_tbl, m_lump, s_package,&
!!     &        wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter,             &
!!     &        mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                 &
!!     &        nod_fld, ele_fld, sgs_coefs, diff_coefs)
!!      subroutine update_fields(mesh, group, ele_mesh, MHD_mesh,       &
!!     &         nod_bcs, surf_bcs, iphys, iphys_ele,                   &
!!     &         jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,  &
!!     &         ifld_diff, icomp_diff, iphys_elediff,                  &
!!     &         filtering, wide_filtering, layer_tbl, m_lump,          &
!!     &         wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,&
!!     &         surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
!!      subroutine reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!!
!!      subroutine fields_evolution_4_FEM_SPH(mesh, group, ele_mesh,    &
!!     &        fluid, nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,     &
!!     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
!!     &        rhs_tbl, FEM_elens, ifld_sgs, icomp_sgs, ifld_diff,     &
!!     &        icomp_diff, iphys_elediff, sgs_coefs_nod,               &
!!     &        filtering, wide_filtering, layer_tbl, s_package,        &
!!     &        wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter,             &
!!     &        mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                 &
!!     &        nod_fld, ele_fld, sgs_coefs, diff_coefs)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: icomp_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(MHD_matrices_pack), intent(in) :: s_package
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
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!@endverbatim
!
      module update_after_evolution
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_material_property
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_layering_ele_list
      use t_MHD_boundary_data
      use t_bc_data_MHD
      use t_MHD_matrices_pack
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fields_evolution(mesh, group, ele_mesh, MHD_mesh,      &
     &        nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,              &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,           &
     &        rhs_tbl, FEM_elens, ifld_sgs, icomp_sgs, ifld_diff,       &
     &        icomp_diff, iphys_elediff, sgs_coefs_nod,                 &
     &        filtering, wide_filtering, layer_tbl, m_lump, s_package,  &
     &        wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter,               &
     &        mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                   &
     &        nod_fld, ele_fld, sgs_coefs, diff_coefs)
!
      use cal_temperature
      use cal_velocity
      use cal_magnetic_field
      use cal_light_element
!
      use update_with_scalars
      use update_with_velo
      use update_with_vector_p
      use update_with_magne
!
!      use check_surface_groups
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: icomp_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_matrices_pack), intent(in) :: s_package
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
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!
!     ---- magnetic field update
!
      if ( iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'cal_magne_vector_potential'
        call cal_vector_potential(mesh%nod_comm, mesh%node, mesh%ele,   &
     &     ele_mesh%surf, MHD_mesh%conduct, group%surf_grp,             &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs,        &
     &     iphys, iphys_ele, ele_fld,                                   &
     &     jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,     &
     &     FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,              &
     &     sgs_coefs, diff_coefs, filtering, m_lump,                    &
     &     s_package%Bmatrix, s_package%Fmatrix, ak_MHD%ak_d_magne,     &
     &     wk_filter, mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
        call update_with_vector_potential                               &
     &    (ifld_diff%i_magne, icomp_diff%i_magne,                       &
     &     iphys_elediff%i_magne, iphys_elediff%i_filter_magne,         &
     &     mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,           &
     &     MHD_mesh%fluid, MHD_mesh%conduct, layer_tbl, group%surf_grp, &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs,        &
     &     iphys, iphys_ele, jac_3d_q, jac_3d_l, jac_sf_grp_q,          &
     &     rhs_tbl, FEM_elens, filtering, wide_filtering, m_lump,       &
     &     wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,      &
     &     surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
!
      else if ( iflag_t_evo_4_magne .gt. id_no_evolution) then
!
!        call check_surface_param_smp('cal_magnetic_field_pre start',   &
!     &      my_rank, sf_grp, group%surf_nod_grp)
        if (iflag_debug.eq.1) write(*,*) 's_cal_magnetic_field'
        call s_cal_magnetic_field(mesh%nod_comm, mesh%node, mesh%ele,   &
     &     ele_mesh%surf, MHD_mesh%conduct, group%surf_grp,             &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,        &
     &     surf_bcs%Fsf_bcs, iphys, iphys_ele, ele_fld,                 &
     &     jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,     &
     &     FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,              &
     &     sgs_coefs, sgs_coefs_nod, diff_coefs, filtering, m_lump,     &
     &     s_package%Bmatrix, s_package%Fmatrix, ak_MHD%ak_d_magne,     &
     &     wk_filter, mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
        call update_with_magnetic_field                                 &
     &    (ifld_diff%i_magne, icomp_diff%i_magne,                       &
     &     iphys_elediff%i_magne, iphys_elediff%i_filter_magne,         &
     &     mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,           &
     &     MHD_mesh%fluid, MHD_mesh%conduct, layer_tbl, group%surf_grp, &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs, iphys,                   &
     &     iphys_ele, jac_3d_q, jac_3d_l, jac_sf_grp_q,                 &
     &     rhs_tbl, FEM_elens, filtering, wide_filtering, m_lump,       &
     &     wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,      &
     &     surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
      end if
!
!     ---- temperature update
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if( iflag_4_ref_temp .ne. id_no_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_parturbation_temp'
          call cal_parturbation_temp                                    &
     &      (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,         &
     &       MHD_mesh%fluid, group%surf_grp, nod_bcs%Tnod_bcs,          &
     &       surf_bcs%Tsf_bcs, iphys, iphys_ele, ele_fld,               &
     &       jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,                &
     &       icomp_sgs, ifld_diff, iphys_elediff,                       &
     &       sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,           &
     &       s_package%Tmatrix, ak_MHD%ak_d_temp, wk_filter,            &
     &       mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
        else
!          call check_surface_param_smp('cal_temperature_field start',  &
!     &        my_rank, sf_grp, group%surf_nod_grp)
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field                                    &
     &      (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,         &
     &       MHD_mesh%fluid, group%surf_grp, nod_bcs%Tnod_bcs,          &
     &       surf_bcs%Tsf_bcs, iphys, iphys_ele, ele_fld,               &
     &       jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,                &
     &       icomp_sgs, ifld_diff, iphys_elediff,                       &
     &       sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,           &
     &       s_package%Tmatrix, ak_MHD%ak_d_temp, wk_filter,            &
     &       mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
        end if
!
        call update_with_temperature                                    &
     &     (ifld_diff%i_temp, icomp_diff%i_temp,                        &
     &      mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      MHD_mesh%fluid, group%surf_grp, surf_bcs%Tsf_bcs, iphys,    &
     &      iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,       &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, diff_coefs)
      end if
!
!     ----- composition update
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_light_element'
        call s_cal_light_element(mesh%nod_comm, mesh%node, mesh%ele,    &
     &      ele_mesh%surf, MHD_mesh%fluid, group%surf_grp,              &
     &      nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, iphys,                  &
     &      iphys_ele, ele_fld, jac_3d_q, jac_sf_grp_q, rhs_tbl,        &
     &      FEM_elens, ifld_diff, diff_coefs, s_package%Cmatrix,        &
     &      ak_MHD%ak_d_composit, mhd_fem_wk, fem_wk,                   &
     &      f_l, f_nl, nod_fld)
        call update_with_dummy_scalar                                   &
     &     (ifld_diff%i_light, icomp_diff%i_light,                      &
     &      mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      MHD_mesh%fluid, group%surf_grp, surf_bcs%Csf_bcs, iphys,    &
     &      iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,       &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, diff_coefs)
      end if
!
!     ---- velocity update
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution                                         &
     &     (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      MHD_mesh%fluid, group%surf_grp, group%surf_nod_grp,         &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,       &
     &      surf_bcs%Psf_bcs, iphys, iphys_ele, ak_MHD,                 &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,   &
     &      sgs_coefs_nod, diff_coefs, filtering, layer_tbl,            &
     &      s_package%Vmatrix, s_package%Pmatrix, wk_lsq, wk_sgs,       &
     &      wk_filter, mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,          &
     &      nod_fld, ele_fld, sgs_coefs)
        call update_with_velocity                                       &
     &     (ifld_diff%i_velo, icomp_diff%i_velo,                        &
     &      iphys_elediff%i_velo, iphys_elediff%i_filter_velo,          &
     &      mesh%nod_comm, mesh%node, mesh%ele,                         &
     &      ele_mesh%surf, MHD_mesh%fluid, group%surf_grp,              &
     &      surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs, iphys,                  &
     &      iphys_ele, jac_3d_q, jac_3d_l, jac_sf_grp_q,                &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
      end if
!
      end subroutine fields_evolution
!
!-----------------------------------------------------------------------
!
      subroutine update_fields(mesh, group, ele_mesh, MHD_mesh,         &
     &        nod_bcs, surf_bcs, iphys, iphys_ele,                      &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,     &
     &        ifld_diff, icomp_diff, iphys_elediff,                     &
     &        filtering, wide_filtering, layer_tbl, m_lump,             &
     &        wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,   &
     &        surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
!
      use average_on_elements
      use update_with_scalars
      use update_with_velo
      use update_with_vector_p
      use update_with_magne
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: icomp_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (iphys%i_velo .ne. 0) then
        call update_with_velocity                                       &
     &     (ifld_diff%i_velo, icomp_diff%i_velo,                        &
     &      iphys_elediff%i_velo, iphys_elediff%i_filter_velo,          &
     &      mesh%nod_comm, mesh%node, mesh%ele,                         &
     &      ele_mesh%surf, MHD_mesh%fluid, group%surf_grp,              &
     &      surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs, iphys,                  &
     &      iphys_ele, jac_3d_q, jac_3d_l, jac_sf_grp_q,                &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
      end if
!
      if (iphys%i_temp .ne. 0) then
        call update_with_temperature                                    &
     &     (ifld_diff%i_temp, icomp_diff%i_temp,                        &
     &      mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      MHD_mesh%fluid, group%surf_grp, surf_bcs%Tsf_bcs, iphys,    &
     &      iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,       &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, diff_coefs)
      end if
!
      if (iphys%i_light .ne. 0) then
        call update_with_dummy_scalar                                   &
     &     (ifld_diff%i_light, icomp_diff%i_light,                      &
     &      mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      MHD_mesh%fluid, group%surf_grp, surf_bcs%Csf_bcs, iphys,    &
     &      iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,       &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, diff_coefs)
      end if
!
      if (iphys%i_vecp .ne. 0) then
        call update_with_vector_potential                               &
     &    (ifld_diff%i_magne, icomp_diff%i_magne,                       &
     &     iphys_elediff%i_magne, iphys_elediff%i_filter_magne,         &
     &     mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,           &
     &     MHD_mesh%fluid, MHD_mesh%conduct, layer_tbl, group%surf_grp, &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs,        &
     &     iphys, iphys_ele, jac_3d_q, jac_3d_l, jac_sf_grp_q,          &
     &     rhs_tbl, FEM_elens, filtering, wide_filtering, m_lump,       &
     &     wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,      &
     &     surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
      else if (iphys%i_magne.ne.0) then
        call update_with_magnetic_field                                 &
     &    (ifld_diff%i_magne, icomp_diff%i_magne,                       &
     &     iphys_elediff%i_magne, iphys_elediff%i_filter_magne,         &
     &     mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,           &
     &     MHD_mesh%fluid, MHD_mesh%conduct, layer_tbl, group%surf_grp, &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs, iphys,                   &
     &     iphys_ele, jac_3d_q, jac_3d_l, jac_sf_grp_q,                 &
     &     rhs_tbl, FEM_elens, filtering, wide_filtering, m_lump,       &
     &     wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,      &
     &     surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
      end if
!
      end subroutine update_fields
!
!-----------------------------------------------------------------------
!
      subroutine reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!     reset monitoring flag
!
      nod_fld%iflag_update(1:nod_fld%ntot_phys) = 0
      sgs_coefs%iflag_field(1:sgs_coefs%num_field) = 0
      diff_coefs%iflag_field(1:diff_coefs%num_field) = 0
!
      end subroutine reset_update_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fields_evolution_4_FEM_SPH(mesh, group, ele_mesh,      &
     &        fluid, nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,       &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,           &
     &        rhs_tbl, FEM_elens, ifld_sgs, icomp_sgs, ifld_diff,       &
     &        icomp_diff, iphys_elediff, sgs_coefs_nod,                 &
     &        filtering, wide_filtering, layer_tbl, s_package,          &
     &        wk_cor, wk_lsq, wk_sgs, wk_diff, wk_filter,               &
     &        mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                   &
     &        nod_fld, ele_fld, sgs_coefs, diff_coefs)
!
      use cal_temperature
      use cal_velocity
      use cal_light_element
!
      use update_with_scalars
      use update_with_velo
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: icomp_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_matrices_pack), intent(in) :: s_package
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
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!
!     ---- temperature update
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if( iflag_4_ref_temp .ne. id_no_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_parturbation_temp'
          call cal_parturbation_temp                                    &
     &       (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf, fluid, &
     &        group%surf_grp, nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,       &
     &        iphys, iphys_ele, ele_fld, jac_3d_q, jac_sf_grp_q,        &
     &        rhs_tbl, FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,  &
     &        sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,          &
     &        s_package%Tmatrix, ak_MHD%ak_d_temp, wk_filter,           &
     &        mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field                                    &
     &       (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf, fluid, &
     &        group%surf_grp, nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,       &
     &        iphys, iphys_ele, ele_fld, jac_3d_q, jac_sf_grp_q,        &
     &        rhs_tbl, FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,  &
     &        sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,          &
     &        s_package%Tmatrix, ak_MHD%ak_d_temp, wk_filter,           &
     &        mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
        end if
!
        call update_with_temperature                                    &
     &     (ifld_diff%i_temp, icomp_diff%i_temp,                        &
     &      mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      fluid, group%surf_grp, surf_bcs%Tsf_bcs, iphys,             &
     &      iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,       &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, diff_coefs)
      end if
!
!     ----- composition update
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_light_element'
        call s_cal_light_element(mesh%nod_comm, mesh%node, mesh%ele,    &
     &      ele_mesh%surf, fluid, group%surf_grp,                       &
     &      nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, iphys, iphys_ele,       &
     &      ele_fld, jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,        &
     &      ifld_diff, diff_coefs, s_package%Cmatrix,                   &
     &      ak_MHD%ak_d_composit, mhd_fem_wk, fem_wk,                   &
     &      f_l, f_nl, nod_fld)
        call update_with_dummy_scalar                                   &
     &     (ifld_diff%i_light, icomp_diff%i_light,                      &
     &      mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      fluid, group%surf_grp, surf_bcs%Csf_bcs, iphys,             &
     &      iphys_ele, ele_fld, jac_3d_q, jac_3d_l, jac_sf_grp_q,       &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, diff_coefs)
      end if
!
!     ---- velocity update
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution(mesh%nod_comm, mesh%node, mesh%ele,     &
     &      ele_mesh%surf, fluid, group%surf_grp, group%surf_nod_grp,   &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,       &
     &      surf_bcs%Psf_bcs, iphys, iphys_ele, ak_MHD,                 &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,   &
     &      sgs_coefs_nod, diff_coefs, filtering, layer_tbl,            &
     &      s_package%Vmatrix, s_package%Pmatrix, wk_lsq, wk_sgs,       &
     &       wk_filter, mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,         &
     &      nod_fld, ele_fld, sgs_coefs)
        call update_with_velocity                                       &
     &     (ifld_diff%i_velo, icomp_diff%i_velo,                        &
     &      iphys_elediff%i_velo, iphys_elediff%i_filter_velo,          &
     &      mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf, fluid,   &
     &      group%surf_grp, surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs,         &
     &      iphys, iphys_ele, jac_3d_q, jac_3d_l, jac_sf_grp_q,         &
     &      rhs_tbl, FEM_elens, filtering, wide_filtering, layer_tbl,   &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,     &
     &      surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
      end if
!
      end subroutine fields_evolution_4_FEM_SPH
!
!-----------------------------------------------------------------------
!
      end module update_after_evolution
