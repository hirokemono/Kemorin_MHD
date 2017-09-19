!
!      module int_type_mass_matrices
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui on March 2009
!
!!      subroutine s_int_type_mass_matrices(FEM_prm, mesh, MHD_mesh,    &
!!     &          jacs, rhs_tbl, fem_mat, fem_int, mk_MHD)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) ::  MHD_mesh
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(arrays_finite_element_mat), intent(inout) :: fem_mat
!!        type(finite_element_integration), intent(inout) :: fem_int
!!        type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
      module int_type_mass_matrices
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_work_FEM_integration
      use t_table_FEM_const
      use t_fem_gauss_int_coefs
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
      subroutine s_int_type_mass_matrices(FEM_prm, mesh, MHD_mesh,      &
     &          jacs, rhs_tbl, fem_mat, fem_int, mk_MHD)
!
      use t_MHD_mass_matricxes
!
      type(FEM_MHD_paremeters), intent(in) ::     FEM_prm
      type(mesh_geometry), intent(in) ::          mesh
      type(mesh_data_MHD), intent(in) ::          MHD_mesh
      type(jacobians_type), intent(in) ::         jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(arrays_finite_element_mat), intent(inout) :: fem_mat
      type(finite_element_integration), intent(inout) :: fem_int
      type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
!
      if (mesh%ele%nnod_4_ele.eq.num_t_quad                             &
     &     .or. mesh%ele%nnod_4_ele.eq.num_t_lag) then
        call int_mass_matrices_quad(FEM_prm, mesh, MHD_mesh,            &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, fem_mat, fem_int, mk_MHD)
      else
        call int_mass_matrix_trilinear(FEM_prm, mesh, MHD_mesh, &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, fem_mat, fem_int, mk_MHD)
      end if
!
      end subroutine s_int_type_mass_matrices
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_trilinear(FEM_prm, mesh, MHD_mesh,     &
     &          g_FEM, jac_3d, rhs_tbl, fem_mat, fem_int, mk_MHD)
!
      use t_MHD_mass_matricxes
      use int_vol_mass_matrix
!
      type(FEM_MHD_paremeters), intent(in) ::     FEM_prm
      type(mesh_geometry), intent(in) ::          mesh
      type(FEM_gauss_int_coefs), intent(in) ::    g_FEM
      type(jacobians_3d), intent(in) ::           jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(mesh_data_MHD), intent(in) ::          MHD_mesh
!
      type(arrays_finite_element_mat), intent(inout) ::  fem_mat
      type(finite_element_integration), intent(inout) :: fem_int
      type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag whole'
      call int_mass_matrix_diag                                         &
     &   (mesh%node, mesh%ele, g_FEM, jac_3d, rhs_tbl,                  &
     &    mesh%ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,            &
     &    fem_mat%fem_wk, fem_mat%f_l, fem_int%m_lump)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag fluid'
      call int_mass_matrix_diag                                         &
     &   (mesh%node, mesh%ele, g_FEM, jac_3d, rhs_tbl,                  &
     &    MHD_mesh%fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,  &
     &    fem_mat%fem_wk, fem_mat%f_l, mk_MHD%mlump_fl)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag conduct'
      call int_mass_matrix_diag                                         &
     &  (mesh%node, mesh%ele, g_FEM, jac_3d, rhs_tbl,                   &
     &   MHD_mesh%conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, &
     &   fem_mat%fem_wk, fem_mat%f_l, mk_MHD%mlump_cd)
!
      if (iflag_debug.eq.1)                                             &
     &         write(*,*) 'int_mass_matrix_diag insulator'
      call int_mass_matrix_diag                                         &
     &   (mesh%node, mesh%ele, g_FEM, jac_3d, rhs_tbl,                  &
     &    MHD_mesh%insulate%istack_ele_fld_smp,                         &
     &    FEM_prm%npoint_t_evo_int, fem_mat%fem_wk, fem_mat%f_l,        &
     &    mk_MHD%mlump_ins)
!
      end subroutine int_mass_matrix_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrices_quad(FEM_prm, mesh, MHD_mesh,        &
     &          g_FEM, jac_3d, rhs_tbl, fem_mat, fem_int, mk_MHD)
!
      use t_MHD_mass_matricxes
      use int_vol_mass_matrix
!
      type(FEM_MHD_paremeters), intent(in) ::     FEM_prm
      type(mesh_geometry), intent(in) ::          mesh
      type(FEM_gauss_int_coefs), intent(in) ::    g_FEM
      type(jacobians_3d), intent(in) ::           jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(mesh_data_MHD), intent(in) ::          MHD_mesh
!
      type(arrays_finite_element_mat), intent(inout) :: fem_mat
      type(finite_element_integration), intent(inout) :: fem_int
      type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_lump_mass_matrix_quad'
      call int_lump_mass_matrix_quad(mesh%node, mesh%ele,               &
     &    g_FEM, jac_3d, rhs_tbl, FEM_prm%npoint_t_evo_int,             &
     &    fem_mat%fem_wk, fem_mat%f_l, fem_int%m_lump)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ fluid'
       call int_mass_matrix_HRZ                                         &
     &    (mesh%node, mesh%ele, g_FEM, jac_3d, rhs_tbl,                 &
     &     MHD_mesh%fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, &
     &     fem_mat%fem_wk, fem_mat%f_l, mk_MHD%mlump_fl)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ conduct'
       call int_mass_matrix_HRZ                                         &
     &  (mesh%node, mesh%ele, g_FEM, jac_3d, rhs_tbl,                   &
     &   MHD_mesh%conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, &
     &   fem_mat%fem_wk, fem_mat%f_l, mk_MHD%mlump_cd)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'int_mass_matrix_HRZ insulator'
       call int_mass_matrix_HRZ                                         &
     &    (mesh%node, mesh%ele, g_FEM, jac_3d, rhs_tbl,                 &
     &     MHD_mesh%insulate%istack_ele_fld_smp,                        &
     &     FEM_prm%npoint_t_evo_int, fem_mat%fem_wk, fem_mat%f_l,       &
     &     mk_MHD%mlump_ins)
!
      end subroutine int_mass_matrices_quad
!
!-----------------------------------------------------------------------
!
      end module int_type_mass_matrices
