!
!     module construct_matrices
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine set_data_4_const_matrices                            &
!!     &         (mesh, MHD_mesh, rhs_tbl, mat_tbl_q, MHD_mat_tbls)
!!      subroutine update_matrices(mesh, group, ele_mesh, MHD_mesh,     &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, FEM_elens, rhs_tbl, &
!!     &          mat_tbl_q, MHD_mat_tbls, mhd_fem_wk, fem_wk)
!!      subroutine set_aiccg_matrices(mesh, group, ele_mesh, MHD_mesh,  &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, FEM_elens, rhs_tbl, &
!!     &          mat_tbl_q, MHD_mat_tbls, mhd_fem_wk, fem_wk)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: mat_tbl_q
!!        type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
      module construct_matrices
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_sorted_node_MHD
!
      implicit none
!
      private :: set_index_list_4_matrix
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine set_data_4_const_matrices                              &
     &         (mesh, MHD_mesh, rhs_tbl, mat_tbl_q, MHD_mat_tbls)
!
      use m_solver_djds_MHD
      use t_solver_djds
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(table_mat_const), intent(inout) :: mat_tbl_q
      type(tables_MHD_mat_const), intent(inout) :: MHD_mat_tbls
!
!
!   set off_diagonal information
!
      call set_index_list_4_matrix                                      &
     &   (mesh, MHD_mesh, rhs_tbl, mat_tbl_q, MHD_mat_tbls)
!
!   deallocate work arrays
!
      if (iflag_debug.eq.1) write(*,*) 'deallocation work arrays'
      call dealloc_type_4_djds_table(MHD1_matrices%MG_DJDS_table(0))
      call dealloc_type_4_djds_table(MHD1_matrices%MG_DJDS_fluid(0))
!
      if (mesh%ele%nnod_4_ele .ne. num_t_linear) then
        call dealloc_type_4_djds_table(MHD1_matrices%MG_DJDS_linear(0))
        call dealloc_type_4_djds_table(MHD1_matrices%MG_DJDS_lin_fl(0))
      end if
!
      end subroutine set_data_4_const_matrices
!
! ----------------------------------------------------------------------
!
      subroutine update_matrices(mesh, group, ele_mesh, MHD_mesh,       &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, FEM_elens, rhs_tbl,   &
     &          mat_tbl_q, MHD_mat_tbls, mhd_fem_wk, fem_wk)
!
      use m_control_parameter
      use m_t_step_parameter
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl_q
      type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer (kind = kint) :: iflag
!
      iflag = 0
      if (    iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                 &
     &  .and. iflag_commute_linear .gt. id_SGS_commute_OFF              &
     &  .and. mod(i_step_MHD,i_step_sgs_coefs) .eq. 0) iflag = 1
      iflag = iflag + iflag_flex_step_changed
!
      if (iflag .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'matrix assemble again'
        call set_aiccg_matrices(mesh, group, ele_mesh, MHD_mesh,        &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, FEM_elens, rhs_tbl,       &
     &      mat_tbl_q, MHD_mat_tbls, mhd_fem_wk, fem_wk)
        iflag_flex_step_changed = 0
      end if
!
      end subroutine update_matrices
!
!  ----------------------------------------------------------------------
!
      subroutine set_aiccg_matrices(mesh, group, ele_mesh, MHD_mesh,    &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, FEM_elens, rhs_tbl,   &
     &          mat_tbl_q, MHD_mat_tbls, mhd_fem_wk, fem_wk)
!
      use m_control_parameter
      use m_iccg_parameter
!
      use int_vol_lumped_mat_crank
      use int_vol_poisson_matrix
      use int_vol_consist_evo_mat
      use set_aiccg_bc_vectors
      use init_iccg_matrices
      use precond_djds_MHD
      use initialize_4_MHD_AMG
      use skip_comment_f
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl_q
      type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call reset_aiccg_matrices(mesh%node, mesh%ele, MHD_mesh%fluid)
!
!   set coefficients of matrix
!
      if (iflag_debug.eq.1) write(*,*) 'matrix assemble'
!
!   Poisson matrix
!
      call int_vol_poisson_matrices(mesh%ele, jac_3d_l, rhs_tbl,        &
     &    MHD_mat_tbls%linear, MHD_mat_tbls%fluid_l,                    &
     &    FEM_elens, fem_wk)
!
!   Diffusion matrix
!
      if (iflag_scheme .eq. id_Crank_nicolson) then
        if (iflag_debug.eq.1) write(*,*) 'int_vol_crank_mat_lump'
        call int_vol_crank_mat_lump                                     &
     &     (mesh%node, MHD_mesh%fluid, MHD_mesh%conduct, mhd_fem_wk)
        if (iflag_debug.eq.1) write(*,*) 'int_vol_crank_matrices'
        call int_vol_crank_matrices(mesh%ele, jac_3d_q, rhs_tbl,        &
     &      mat_tbl_q, MHD_mat_tbls%fluid_q,                            &
     &      MHD_mat_tbls%full_conduct_q, FEM_elens, fem_wk)
      else if (iflag_scheme .eq. id_Crank_nicolson_cmass) then
        call int_vol_crank_mat_consist                                  &
     &     (mesh%ele, jac_3d_q, rhs_tbl, MHD_mat_tbls, fem_wk)
        call int_vol_crank_matrices(mesh%ele, jac_3d_q, rhs_tbl,        &
     &      mat_tbl_q, MHD_mat_tbls%fluid_q,                            &
     &      MHD_mat_tbls%full_conduct_q, FEM_elens, fem_wk)
      end if
!
!     set boundary conditions
!
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_bc_phys'
      call set_aiccg_bc_phys(mesh%ele, ele_mesh%surf, group%surf_grp,   &
     &    jac_sf_grp_q, rhs_tbl, MHD_mat_tbls%fluid_q, fem_wk)
!
      if (iflag_debug.eq.1) write(*,*) 'preconditioning'
      call matrix_precondition
!
!
!     set marrix for the Multigrid
!
      if(cmp_no_case(method_4_solver, 'MGCG')) then
        call const_MGCG_MHD_matrices
      end if
!
      end subroutine set_aiccg_matrices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_index_list_4_matrix                                &
     &         (mesh, MHD_mesh, rhs_tbl, mat_tbl_q, MHD_mat_tbls)
!
      use calypso_mpi
      use m_control_parameter
      use m_solver_djds_MHD
!
      use set_index_list_4_djds
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(table_mat_const), intent(inout) :: mat_tbl_q
      type(tables_MHD_mat_const), intent(inout) :: MHD_mat_tbls
!
!
!      write(*,*) 'set_index_list_4_DJDS_mat_etr'
      call alloc_type_marix_list                                        &
     &   (mesh%ele%nnod_4_ele, rhs_tbl, mat_tbl_q)
      call set_index_list_4_DJDS_mat_etr(mesh%node, mesh%ele, rhs_tbl,  &
     &    MHD1_matrices%MG_DJDS_table(0), mat_tbl_q)
!
!      write(*,*) 'set_index_list_4_mat_etr_l'
      call set_index_list_4_mat_etr_l(mesh%node, mesh%ele,              &
     &    rhs_tbl, mat_tbl_q, MHD_mat_tbls%linear)
!
      if (iflag_t_evo_4_velo .ne. id_no_evolution                       &
     &  .or. iflag_t_evo_4_temp .ne. id_no_evolution                    &
     &  .or. iflag_t_evo_4_composit .ne. id_no_evolution) then
!        write(*,*) 'set_index_list_4_mat_fl'
        call set_index_list_4_mat_fl(mesh%node, mesh%ele,               &
     &      MHD_mesh%fluid, rhs_tbl, MHD_mat_tbls%fluid_q)
!        write(*,*) 'set_index_list_4_mat_fl_l'
        call set_index_list_4_mat_fl_l                                  &
     &     (mesh%node, mesh%ele, MHD_mesh%fluid, rhs_tbl,               &
     &    MHD_mat_tbls%fluid_q, MHD_mat_tbls%fluid_l)
      end if
!
      if (iflag_t_evo_4_magne .ne. id_no_evolution                      &
     &     .or. iflag_t_evo_4_vect_p .eq. id_Crank_nicolson_cmass) then
        write(*,*) 'set_index_list_4_mat_cd'
        call set_index_list_4_mat_cd                                    &
     &     (mesh%node, mesh%ele, MHD_mesh%conduct, rhs_tbl,             &
     &      MHD_mat_tbls%full_conduct_q)
      end if
!
!
      end subroutine set_index_list_4_matrix
!
! ----------------------------------------------------------------------
!
      end module construct_matrices
