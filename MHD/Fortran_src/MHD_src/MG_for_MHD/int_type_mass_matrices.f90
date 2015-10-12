!
!      module int_type_mass_matrices
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui on March 2009
!
!      subroutine s_int_type_mass_matrices(mesh, MHD_mesh, jacobians,   &
!     &          rhs_tbl, fem_mat, mk_MHD)
!        type(mesh_geometry), intent(in) :: mesh
!        type(mesh_data_MHD), intent(in) ::  MHD_mesh
!        type(jacobians_type), intent(in) :: jacobians
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(arrays_finite_element_mat), intent(inout) :: fem_mat
!        type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
      module int_type_mass_matrices
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use t_mesh_data
      use t_geometry_data_MHD
      use t_table_FEM_const
      use t_jacobians
!
      implicit none
!
      private :: int_mass_matrix_trilinear, int_mass_matrices_quad
      private :: int_mass_matrix_all_quad
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_type_mass_matrices(mesh, MHD_mesh, jacobians,    &
     &           rhs_tbl, fem_mat, mk_MHD)
!
      use m_machine_parameter
      use m_geometry_constants
      use t_finite_element_mat_MHD
!
      type(mesh_geometry), intent(in) ::          mesh
      type(mesh_data_MHD), intent(in) ::          MHD_mesh
      type(jacobians_type), intent(in) ::         jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(arrays_finite_element_mat), intent(inout) :: fem_mat
      type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
!
      if (mesh%ele%nnod_4_ele.eq.num_t_quad                             &
     &     .or. mesh%ele%nnod_4_ele.eq.num_t_lag) then
        call int_mass_matrices_quad(mesh, MHD_mesh, jacobians%jac_3d,   &
     &      rhs_tbl, fem_mat, mk_MHD)
      else
        call int_mass_matrix_trilinear(mesh, MHD_mesh,                  &
     &      jacobians%jac_3d, rhs_tbl, fem_mat, mk_MHD)
      end if
!
      end subroutine s_int_type_mass_matrices
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_trilinear(mesh, MHD_mesh, jac_3d,      &
     &          rhs_tbl, fem_mat, mk_MHD)
!
      use m_machine_parameter
      use t_finite_element_mat_MHD
      use int_mass_matrices_type
!
      type(mesh_geometry), intent(in) ::          mesh
      type(jacobians_3d), intent(in) ::           jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(mesh_data_MHD), intent(in) ::          MHD_mesh
!
      type(arrays_finite_element_mat), intent(inout) :: fem_mat
      type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag_type whole'
      call int_mass_matrix_diag_type                                    &
     &   (mesh%ele%istack_ele_smp, intg_point_t_evo,                    &
     &    mesh, jac_3d, rhs_tbl, fem_mat%fem_wk, fem_mat%fem_rhs%f_l,   &
     &    fem_mat%m_lump)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag_type fluid'
      call int_mass_matrix_diag_type                                    &
     &   (MHD_mesh%fluid%istack_ele_fld_smp, intg_point_t_evo,          &
     &    mesh, jac_3d, rhs_tbl, fem_mat%fem_wk, fem_mat%fem_rhs%f_l,   &
     &    mk_MHD%fluid)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag_type conduct'
      call int_mass_matrix_diag_type                                    &
     &   (MHD_mesh%conduct%istack_ele_fld_smp, intg_point_t_evo,        &
     &    mesh, jac_3d, rhs_tbl, fem_mat%fem_wk, fem_mat%fem_rhs%f_l,   &
     &    mk_MHD%conduct)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag_type insulator'
      call int_mass_matrix_diag_type                                    &
     &   (MHD_mesh%insulate%istack_ele_fld_smp, intg_point_t_evo,       &
     &    mesh, jac_3d, rhs_tbl, fem_mat%fem_wk, fem_mat%fem_rhs%f_l,   &
     &    mk_MHD%insulate)
!
      end subroutine int_mass_matrix_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrices_quad(mesh, MHD_mesh, jac_3d,         &
     &          rhs_tbl, fem_mat, mk_MHD)
!
      use m_machine_parameter
      use t_finite_element_mat_MHD
      use int_mass_matrices_type
!
      type(mesh_geometry), intent(in) ::          mesh
      type(jacobians_3d), intent(in) ::           jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(mesh_data_MHD), intent(in) ::          MHD_mesh
!
      type(arrays_finite_element_mat), intent(inout) :: fem_mat
      type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_mass_matrix_all_quad'
      call int_mass_matrix_all_quad(mesh, jac_3d, rhs_tbl,              &
     &    fem_mat%fem_wk, fem_mat%fem_rhs%f_l, fem_mat%m_lump)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ_type fluid'
       call int_mass_matrix_HRZ_type                                    &
     &    (MHD_mesh%fluid%istack_ele_fld_smp, intg_point_t_evo,         &
     &     mesh, jac_3d, rhs_tbl, fem_mat%fem_wk,                       &
     &     fem_mat%fem_rhs%f_l, mk_MHD%fluid)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ_type conduct'
       call int_mass_matrix_HRZ_type                                    &
     &    (MHD_mesh%conduct%istack_ele_fld_smp, intg_point_t_evo,       &
     &     mesh, jac_3d, rhs_tbl, fem_mat%fem_wk,                       &
     &     fem_mat%fem_rhs%f_l, mk_MHD%conduct)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ_type insulator'
       call int_mass_matrix_HRZ_type                                    &
     &    (MHD_mesh%insulate%istack_ele_fld_smp, intg_point_t_evo,      &
     &     mesh, jac_3d, rhs_tbl, fem_mat%fem_wk,                       &
     &     fem_mat%fem_rhs%f_l, mk_MHD%insulate)
!
      end subroutine int_mass_matrices_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_all_quad(mesh, jac_3d, rhs_tbl,        &
     &          fem_wk, rhs_l, m_lump)
!
      use t_finite_element_mat
      use fem_skv_mass_mat_type
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
!
      type(mesh_geometry), intent(in) ::          mesh
      type(jacobians_3d), intent(in) ::           jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) ::     rhs_l
      type(lumped_mass_matrices), intent(inout) ::   m_lump
!
!
      call reset_ff_smps_type(n_scalar,                                 &
     &    mesh%node%max_nod_smp, np_smp, rhs_l)
      call reset_sk6(n_scalar, mesh%ele, fem_wk%sk6)
!
      call fem_skv_mass_mat_diag_HRZ_type(mesh%ele%istack_ele_smp,      &
     &    intg_point_t_evo, mesh%ele, jac_3d, fem_wk%sk6)
!
      call sum_skv_diagonal_4_HRZ_type(mesh%ele%istack_ele_smp,         &
     &    mesh%ele, fem_wk%sk6, fem_wk%me_diag)
!
      call add1_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, rhs_l%ff_smp)
      call cal_ff_smp_2_ml_type                                         &
     &   (mesh%node, rhs_tbl, rhs_l%ff_smp, m_lump)
!
!      call check_mass_martix
!
      end subroutine int_mass_matrix_all_quad
!
!-----------------------------------------------------------------------
!
      end module int_type_mass_matrices
