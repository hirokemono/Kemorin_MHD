!
!      module int_MHD_mass_matrices
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Oct. 2006
!
!!      subroutine int_RHS_mass_matrices(n_int, node, ele, MHD_mesh,    &
!!     &           jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, m_lump)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(lumped_mass_matrices), intent(inout) :: m_lump
!
      module int_MHD_mass_matrices
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use int_vol_mass_matrix
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
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
      subroutine int_RHS_mass_matrices(n_int, node, ele, MHD_mesh,      &
     &           jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, m_lump)
!
      integer(kind = kint), intent(in) :: n_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      if     (ele%nnod_4_ele.eq.num_t_quad                              &
     &   .or. ele%nnod_4_ele.eq.num_t_lag) then
        call int_mass_matrices_quad(n_int, node, ele,                   &
     &      MHD_mesh%fluid, MHD_mesh%conduct, MHD_mesh%insulate,        &
     &      jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, m_lump)
      else
        call int_mass_matrix_trilinear(n_int, node, ele,                &
     &      MHD_mesh%fluid, MHD_mesh%conduct, MHD_mesh%insulate,        &
     &      jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, m_lump)
      end if 
!
      end subroutine int_RHS_mass_matrices
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_trilinear                              &
     &          (n_int, node, ele, fluid, conduct, insulate,            &
     &           jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, m_lump)
!
      integer(kind = kint), intent(in) :: n_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(field_geometry_data), intent(in) :: conduct, insulate
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_lump_mass_matrix_linear'
      call int_lump_mass_matrix_linear                                  &
     &   (node, ele, jac_3d, rhs_tbl, n_int, fem_wk, f_l, m_lump)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_diag fluid'
      call int_mass_matrix_diag(node, ele, jac_3d, rhs_tbl,             &
     &    fluid%istack_ele_fld_smp, n_int, fem_wk, f_l,                 &
     &    mhd_fem_wk%mlump_fl)
!      call check_mass_martix_fluid(my_rank, node%numnod, mhd_fem_wk)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_diag conductor'
      call int_mass_matrix_diag(node, ele, jac_3d, rhs_tbl,             &
     &    conduct%istack_ele_fld_smp, n_int, fem_wk, f_l,               &
     &    mhd_fem_wk%mlump_cd)
!      call check_mass_martix_conduct                                   &
!     &   (my_rank, node%numnod, mhd_fem_wk)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_diag insulator'
      call int_mass_matrix_diag(node, ele, jac_3d, rhs_tbl,             &
     &    insulate%istack_ele_fld_smp, n_int, fem_wk, f_l,              &
     &    mhd_fem_wk%mlump_ins)
!
      end subroutine int_mass_matrix_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrices_quad                                 &
     &          (n_int, node, ele, fluid, conduct, insulate,            &
     &           jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, m_lump)
!
      integer(kind = kint), intent(in) :: n_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(field_geometry_data), intent(in) :: conduct, insulate
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_lump_mass_matrix_quad'
      call int_lump_mass_matrix_quad(node, ele, jac_3d, rhs_tbl,        &
     &    n_int, fem_wk, f_l, m_lump)
!      call check_mass_martix(my_rank, node%numnod, m_lump)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_HRZ fluid'
      call int_mass_matrix_HRZ(node, ele, jac_3d, rhs_tbl,              &
     &    fluid%istack_ele_fld_smp, n_int,                              &
     &    fem_wk, f_l, mhd_fem_wk%mlump_fl)
!      call check_mass_martix_fluid(my_rank, node%numnod, mhd_fem_wk)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_HRZ conduct'
      call int_mass_matrix_HRZ(node, ele, jac_3d, rhs_tbl,              &
     &    conduct%istack_ele_fld_smp, n_int, fem_wk, f_l,               &
     &    mhd_fem_wk%mlump_cd)
!      call check_mass_martix_conduct                                   &
!     &   (my_rank, node%numnod, mhd_fem_wk)
!
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_HRZ insulator'
      call int_mass_matrix_HRZ(node, ele, jac_3d, rhs_tbl,              &
     &    insulate%istack_ele_fld_smp, n_int,                           &
     &    fem_wk, f_l, mhd_fem_wk%mlump_ins)
!
      end subroutine int_mass_matrices_quad
!
!-----------------------------------------------------------------------

      end module int_MHD_mass_matrices
