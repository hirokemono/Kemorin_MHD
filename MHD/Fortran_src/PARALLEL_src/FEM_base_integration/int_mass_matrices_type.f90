!
!      module int_mass_matrices_type
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!      subroutine int_consist_mass_matrix_type(iele_fsmp_stack, num_int,&
!     &          mesh, jac_3d, rhs_tbl, mat_tbl, fem_wk, mat11)
!        integer (kind=kint), intent(in) :: num_int
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(in) :: mat_tbl
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(DJDS_MATRIX),  intent(inout) :: mat11
!
!      subroutine int_mass_matrix_type(iele_fsmp_stack, num_int,        &
!     &          mesh, jac_3d, rhs_tbl, fem_wk, rhs_l)
!        integer (kind=kint), intent(in) :: num_int
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(finite_ele_mat_node), intent(inout) :: rhs_l
!      subroutine int_mass_matrix_diag_type(iele_fsmp_stack, num_int,   &
!     &          mesh, jac_3d, rhs_tbl, fem_wk, rhs_l, m_lump)
!        integer (kind=kint), intent(in) :: num_int
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(finite_ele_mat_node), intent(inout) :: rhs_l
!        type(lumped_mass_matrices), intent(inout) :: m_lump
!      subroutine int_mass_matrix_HRZ_type(iele_fsmp_stack, num_int,    &
!     &         mesh, jac_3d, rhs_tbl, fem_wk, rhs_l, m_lump)
!        integer (kind=kint), intent(in) :: num_int
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(finite_ele_mat_node), intent(inout) :: rhs_l
!        type(lumped_mass_matrices), intent(inout) :: m_lump
!
      module int_mass_matrices_type
!
      use m_precision
!
      use m_phys_constants
      use t_mesh_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!!-----------------------------------------------------------------------
!
      subroutine int_consist_mass_matrix_type(iele_fsmp_stack, num_int, &
     &          mesh, jac_3d, rhs_tbl, mat_tbl, fem_wk, mat11)
!
      use t_solver_djds
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
      use add_skv1_2_matrix_type
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
      integer (kind = kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, mesh%ele%nnod_4_ele
        call reset_sk6(n_scalar, mesh%ele, fem_wk%sk6)
        call fem_skv_mass_matrix_type(iele_fsmp_stack, num_int, k2,     &
     &      mesh%ele, jac_3d, fem_wk%sk6)
        call add_skv1_2_matrix11_type(mesh%ele, rhs_tbl,                &
     &      mat_tbl%idx_4_mat, fem_wk%sk6, k2, mat11)
      end do
!
      end subroutine int_consist_mass_matrix_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_type(iele_fsmp_stack, num_int,         &
     &          mesh, jac_3d, rhs_tbl, fem_wk, rhs_l)
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
      use cal_ff_smp_to_ffs
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
!
      integer (kind = kint) :: k2
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps_type(n_scalar, mesh%node%max_nod_smp, rhs_l)
      call reset_sk6(n_scalar, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, mesh%ele%nnod_4_ele
        call fem_skv_mass_matrix_type(iele_fsmp_stack, num_int, k2,     &
     &      mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, rhs_l%ff_smp)
!
      end subroutine int_mass_matrix_type
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_diag_type(iele_fsmp_stack, num_int,    &
     &          mesh, jac_3d, rhs_tbl, fem_wk, rhs_l, m_lump)
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
      use cal_ff_smp_to_ffs
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps_type(n_scalar, mesh%node%max_nod_smp, rhs_l)
      call reset_sk6(n_scalar, mesh%ele, fem_wk%sk6)
!
      call fem_skv_mass_matrix_diag_type(iele_fsmp_stack, num_int,      &
     &    mesh%ele, jac_3d, fem_wk%sk6)
!
      call add1_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, rhs_l%ff_smp)
      call cal_ff_smp_2_ml_type                                         &
     &   (mesh%node, rhs_tbl, rhs_l%ff_smp, m_lump)
!
!      call check_mass_martix
!
      end subroutine int_mass_matrix_diag_type
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_HRZ_type(iele_fsmp_stack, num_int,     &
     &         mesh, jac_3d, rhs_tbl, fem_wk, rhs_l, m_lump)
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
      use cal_ff_smp_to_ffs
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      call reset_ff_smps_type(n_scalar, mesh%node%max_nod_smp, rhs_l)
      call reset_sk6(n_scalar, mesh%ele, fem_wk%sk6)
!
      call fem_skv_mass_mat_diag_HRZ_type(iele_fsmp_stack, num_int,     &
     &    mesh%ele, jac_3d, fem_wk%sk6)
      call vol_average_skv_HRZ_type(iele_fsmp_stack, mesh%ele,          &
     &    fem_wk%sk6, fem_wk%me_diag)
!
      call add1_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, rhs_l%ff_smp)
      call cal_ff_smp_2_ml_type                                         &
     &   (mesh%node, rhs_tbl, rhs_l%ff_smp, m_lump)
!
!      call check_mass_martix_part
!
      end subroutine int_mass_matrix_HRZ_type
!
!-----------------------------------------------------------------------
!
      end module int_mass_matrices_type
