!
!     module construct_matrices
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine set_data_4_const_matrices(mesh, MHD_mesh, rhs_tbl,   &
!!     &          MHD_mat_tbls, MHD_matrices, s_package)
!!        type(MHD_matrices_pack), intent(inout) :: s_package
!!      subroutine update_matrices(time_d, FEM_prm, SGS_par,            &
!!     &          mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,   &
!!     &          ak_MHD, jacobians, FEM_elens, ifld_diff, diff_coefs,  &
!!     &          rhs_tbl, MHD_mat_tbls, surf_wk, flex_p,               &
!!     &          mhd_fem_wk, fem_wk, MHD_matrices)
!!      subroutine set_aiccg_matrices(dt, FEM_prm, SGS_param, cmt_param,&
!!     &          mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,   &
!!     &          ak_MHD, jacobians, FEM_elens, ifld_diff, diff_coefs,  &
!!     &          rhs_tbl, MHD_mat_tbls, surf_wk, mhd_fem_wk, fem_wk,   &
!!     &          MHD_matrices)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in)  :: surf_bcs
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_surface_element_mat), intent(in) :: surf_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(MHD_MG_matrices), intent(inout) :: MHD_matrices
!
      module construct_matrices
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
      use t_sorted_node_MHD
      use t_bc_data_MHD
      use t_MHD_boundary_data
      use t_solver_djds_MHD
      use t_MHD_matrices_pack
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine set_data_4_const_matrices(mesh, MHD_mesh, rhs_tbl,     &
     &          MHD_mat_tbls, MHD_matrices, s_package)
!
      use calypso_mpi
      use m_type_AMG_data
      use m_physical_property
!
      use t_solver_djds
!
      use set_MHD_idx_4_mat_type
!
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(tables_MHD_mat_const), intent(inout) :: MHD_mat_tbls
      type(MHD_MG_matrices), intent(inout) :: MHD_matrices
      type(MHD_matrices_pack), intent(inout) :: s_package
!
!
      call s_set_MHD_idx_4_mat_type(mesh, MHD_mesh,                     &
     &    fl_prop1, cd_prop1, ht_prop1, cp_prop1, rhs_tbl,              &
     &    MHD_matrices%MG_DJDS_table(0), MHD_matrices%MG_DJDS_fluid(0), &
     &    MHD_matrices%MG_DJDS_linear(0),                               &
     &    MHD_matrices%MG_DJDS_lin_fl(0),                               &
     &    MHD_mat_tbls%base, MHD_mat_tbls%fluid_q,                      &
     &    MHD_mat_tbls%full_conduct_q, MHD_mat_tbls%linear,             &
     &    MHD_mat_tbls%fluid_l)
!
      call link_MG_DJDS_MHD_structures                                  &
     &   (MGCG_WK1%num_MG_level, MHD_matrices, s_package)
!
      end subroutine set_data_4_const_matrices
!
! ----------------------------------------------------------------------
!
      subroutine update_matrices(time_d, FEM_prm, SGS_par,              &
     &          mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,     &
     &          ak_MHD, jacobians, FEM_elens, ifld_diff, diff_coefs,    &
     &          rhs_tbl, MHD_mat_tbls, surf_wk, flex_p,                 &
     &          mhd_fem_wk, fem_wk, MHD_matrices)
!
      use t_time_data
      use t_SGS_control_parameter
      use t_flex_delta_t_data
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in)  :: surf_bcs
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_type), intent(in) :: jacobians
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
      type(work_surface_element_mat), intent(in) :: surf_wk
!
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(MHD_MG_matrices), intent(inout) :: MHD_matrices
!
      integer (kind = kint) :: iflag
!
      if (   SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF      &
     & .and. SGS_par%commute_p%iflag_c_linear .gt. id_SGS_commute_OFF   &
     & .and. mod(time_d%i_time_step,SGS_par%i_step_sgs_coefs) .eq. 0)   &
     &   then
        iflag = id_turn_ON
      else
        iflag = id_turn_OFF
      end if
      iflag = iflag + flex_p%iflag_flex_step_changed
!
      if (iflag .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'matrix assemble again'
        call set_aiccg_matrices                                         &
     &     (time_d%dt, FEM_prm, SGS_par%model_p, SGS_par%commute_p,     &
     &      mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs, ak_MHD, &
     &      jacobians, FEM_elens, ifld_diff, diff_coefs, rhs_tbl,       &
     &      MHD_mat_tbls, surf_wk, mhd_fem_wk, fem_wk, MHD_matrices)
        flex_p%iflag_flex_step_changed = 0
      end if
!
      end subroutine update_matrices
!
!  ----------------------------------------------------------------------
!
      subroutine set_aiccg_matrices(dt, FEM_prm, SGS_param, cmt_param,  &
     &          mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,     &
     &          ak_MHD, jacobians, FEM_elens, ifld_diff, diff_coefs,    &
     &          rhs_tbl, MHD_mat_tbls, surf_wk, mhd_fem_wk, fem_wk,     &
     &          MHD_matrices)
!
      use m_physical_property
!
      use set_aiccg_matrices_type
      use precond_djds_MHD
      use initialize_4_MHD_AMG
      use skip_comment_f
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in)  :: surf_bcs
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
      type(work_surface_element_mat), intent(in) :: surf_wk
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(MHD_MG_matrices), intent(inout) :: MHD_matrices
!
!
      call s_set_aiccg_matrices                                         &
     &   (dt, FEM_prm, SGS_param, cmt_param,                            &
     &    mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,           &
     &    fl_prop1, cd_prop1, ht_prop1, cp_prop1, ak_MHD, jacobians,    &
     &    FEM_elens, ifld_diff, diff_coefs, rhs_tbl,                    &
     &    MHD_matrices%MG_DJDS_table(0), MHD_matrices%MG_DJDS_fluid(0), &
     &    MHD_matrices%MG_DJDS_linear(0),                               &
     &    MHD_matrices%MG_DJDS_lin_fl(0), MHD_mat_tbls%base,            &
     &    MHD_mat_tbls%fluid_q, MHD_mat_tbls%full_conduct_q,            &
     &    MHD_mat_tbls%linear, MHD_mat_tbls%fluid_l,                    &
     &    mhd_fem_wk%mlump_fl, mhd_fem_wk%mlump_cd, surf_wk, fem_wk,    &
     &    MHD_matrices%Vmat_MG_DJDS(0), MHD_matrices%Bmat_MG_DJDS(0),   &
     &    MHD_matrices%Tmat_MG_DJDS(0), MHD_matrices%Cmat_MG_DJDS(0),   &
     &    MHD_matrices%Pmat_MG_DJDS(0), MHD_matrices%Fmat_MG_DJDS(0))
!
!     set marrix for the Multigrid
!
      if(cmp_no_case(FEM_PRM%CG11_param%METHOD, 'MGCG')) then
        call const_MGCG_MHD_matrices                                    &
     &     (dt, FEM_prm, SGS_param, cmt_param, ifld_diff, MHD_matrices)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'preconditioning'
      call matrix_precondition                                          &
     &   (FEM_PRM%CG11_param%PRECOND, FEM_PRM%precond_33,               &
     &    FEM_prm%CG11_param%sigma_diag,                                &
     &    fl_prop1, cd_prop1, ht_prop1, cp_prop1,                       &
     &    MHD_matrices%MG_DJDS_table(0),                                &
     &    MHD_matrices%MG_DJDS_fluid(0),                                &
     &    MHD_matrices%MG_DJDS_linear(0),                               &
     &    MHD_matrices%MG_DJDS_lin_fl(0),                               &
     &    MHD_matrices%Vmat_MG_DJDS(0), MHD_matrices%Bmat_MG_DJDS(0),   &
     &    MHD_matrices%Tmat_MG_DJDS(0), MHD_matrices%Cmat_MG_DJDS(0),   &
     &    MHD_matrices%Pmat_MG_DJDS(0), MHD_matrices%Fmat_MG_DJDS(0))
!
      end subroutine set_aiccg_matrices
!
! ----------------------------------------------------------------------
!
      end module construct_matrices
