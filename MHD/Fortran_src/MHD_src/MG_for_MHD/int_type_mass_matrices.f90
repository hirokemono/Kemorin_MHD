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
      use int_vol_mass_matrix
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
     &         write(*,*) 'int_mass_matrix_diag whole'
      call int_mass_matrix_diag(mesh%node, mesh%ele, jac_3d, rhs_tbl,   &
     &    mesh%ele%istack_ele_smp, intg_point_t_evo,                    &
     &    fem_mat%fem_wk, fem_mat%fem_rhs%f_l,   &
     &    fem_mat%m_lump)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag fluid'
      call int_mass_matrix_diag(mesh%node, mesh%ele, jac_3d, rhs_tbl,   &
     &    MHD_mesh%fluid%istack_ele_fld_smp, intg_point_t_evo,          &
     &    fem_mat%fem_wk, fem_mat%fem_rhs%f_l, mk_MHD%fluid)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag conduct'
      call int_mass_matrix_diag(mesh%node, mesh%ele, jac_3d, rhs_tbl,   &
     &    MHD_mesh%conduct%istack_ele_fld_smp, intg_point_t_evo,        &
     &    fem_mat%fem_wk, fem_mat%fem_rhs%f_l, mk_MHD%conduct)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag insulator'
      call int_mass_matrix_diag(mesh%node, mesh%ele, jac_3d, rhs_tbl,   &
     &    MHD_mesh%insulate%istack_ele_fld_smp, intg_point_t_evo,       &
     &    fem_mat%fem_wk, fem_mat%fem_rhs%f_l, mk_MHD%insulate)
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
      use int_vol_mass_matrix
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
      if (iflag_debug.eq.1) write(*,*) 'int_lump_mass_matrix_quad'
      call int_lump_mass_matrix_quad                                    &
     &   (mesh%node, mesh%ele, jac_3d, rhs_tbl, intg_point_t_evo,       &
     &    fem_mat%fem_wk, fem_mat%fem_rhs%f_l, fem_mat%m_lump)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ fluid'
       call int_mass_matrix_HRZ (mesh%node, mesh%ele, jac_3d, rhs_tbl,  &
     &     MHD_mesh%fluid%istack_ele_fld_smp, intg_point_t_evo,         &
     &     fem_mat%fem_wk, fem_mat%fem_rhs%f_l, mk_MHD%fluid)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ conduct'
       call int_mass_matrix_HRZ(mesh%node, mesh%ele, jac_3d, rhs_tbl,   &
     &    MHD_mesh%conduct%istack_ele_fld_smp, intg_point_t_evo,        &
     &     fem_mat%fem_wk, fem_mat%fem_rhs%f_l, mk_MHD%conduct)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ insulator'
       call int_mass_matrix_HRZ(mesh%node, mesh%ele, jac_3d, rhs_tbl,   &
     &     MHD_mesh%insulate%istack_ele_fld_smp, intg_point_t_evo,      &
     &     fem_mat%fem_wk, fem_mat%fem_rhs%f_l, mk_MHD%insulate)
!
      end subroutine int_mass_matrices_quad
!
!-----------------------------------------------------------------------
!
      end module int_type_mass_matrices
