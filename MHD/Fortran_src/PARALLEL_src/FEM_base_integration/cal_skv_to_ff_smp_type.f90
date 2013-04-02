!cal_skv_to_ff_smp_type.f90
!     module cal_skv_to_ff_smp_type
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!     Modified by H. Matsui on March, 2012
!
!> @brief Assemble element integration data to nodal vector
!
!
!  -------- Assemble routines from skv to ff_smp for scalar
!      subroutine add1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_v)
!      subroutine add1_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,         &
!     &           coef, fem_wk, f_v)
!      subroutine sub1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_v)
!      subroutine sub1_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,         &
!     &           coef, fem_wk, f_v)
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(work_finite_element_mat), intent(in) :: fem_wk
!        type(finite_ele_mat_node), intent(inout) :: f_v
!
!  -------- Assemble routines from skv to ff_smp for vector
!      subroutine add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_v)
!      subroutine add3_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,         &
!     &           coef, fem_wk, f_v)
!      subroutine sub3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_v)
!      subroutine sub3_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,         &
!     &           coef, fem_wk, f_v)
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(work_finite_element_mat), intent(in) :: fem_wk
!        type(finite_ele_mat_node), intent(inout) :: f_v
!
!  -------- Assemble routines from skv to ff_smp for tensor
!      subroutine add6_skv_to_ff_t_smp_type(mesh, rhs_tbl, fem_wk, f_t)
!      subroutine add6_skv_coef_to_ff_t_smp_type(mesh, rhs_tbl,         &
!     &           coef, fem_wk, f_t)
!      subroutine sub6_skv_to_ff_t_smp_type(mesh, rhs_tbl, fem_wk, f_t)
!      subroutine sub6_skv_coef_to_ff_t_smp_type(mesh, rhs_tbl,         &
!     &           coef, fem_wk, f_t)
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(work_finite_element_mat), intent(in) :: fem_wk
!        type(finite_ele_mat_node), intent(inout) :: f_t
!
      module cal_skv_to_ff_smp_type
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine add1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_v)
!
      use cal_skv_to_ff_scalar_smp
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_v
!
!
      call add_skv_scalar_2_ff_smp                                      &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     f_v%ff_smp, fem_wk%sk6)
!
      end subroutine add1_skv_to_ff_v_smp_type
!
! ----------------------------------------------------------------------
!
      subroutine add1_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,          &
     &           coef, fem_wk, f_v)
!
      use cal_skv_to_ff_scalar_smp
!
      real (kind=kreal), intent(in)    :: coef
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_v
!
!
      call add_skv_scalar_coef_2_ff_smp                                 &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     coef, f_v%ff_smp, fem_wk%sk6)
!
      end subroutine add1_skv_coef_to_ff_v_smp_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_v)
!
      use cal_skv_to_ff_scalar_smp
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_v
!
!
      call sub_skv_scalar_2_ff_smp                                      &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     f_v%ff_smp, fem_wk%sk6)
!
      end subroutine sub1_skv_to_ff_v_smp_type
!
! ----------------------------------------------------------------------
!
      subroutine sub1_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,          &
     &           coef, fem_wk, f_v)
!
      use cal_skv_to_ff_scalar_smp
!
      real (kind=kreal), intent(in)    :: coef
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_v
!
!
      call sub_skv_scalar_coef_2_ff_smp                                 &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     coef, f_v%ff_smp, fem_wk%sk6)
!
      end subroutine sub1_skv_coef_to_ff_v_smp_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_v)
!
      use cal_skv_to_ff_vector_smp
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_v
!
!
      call add_skv_vector_2_ff_smp                                      &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     f_v%ff_smp, fem_wk%sk6)
!
      end subroutine add3_skv_to_ff_v_smp_type
!
! ----------------------------------------------------------------------
!
      subroutine add3_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,          &
     &           coef, fem_wk, f_v)
!
      use cal_skv_to_ff_vector_smp
!
      real (kind=kreal), intent(in)    :: coef
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_v
!
!
      call add_skv_vector_coef_2_ff_smp                                 &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     coef, f_v%ff_smp, fem_wk%sk6)
!
      end subroutine add3_skv_coef_to_ff_v_smp_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_v)
!
      use cal_skv_to_ff_vector_smp
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_v
!
!
      call sub_skv_vector_2_ff_smp                                      &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     f_v%ff_smp, fem_wk%sk6)
!
      end subroutine sub3_skv_to_ff_v_smp_type
!
! ----------------------------------------------------------------------
!
      subroutine sub3_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,          &
     &           coef, fem_wk, f_v)
!
      use cal_skv_to_ff_vector_smp
!
      real (kind=kreal), intent(in)    :: coef
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_v
!
!
      call sub_skv_vector_coef_2_ff_smp                                 &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     coef, f_v%ff_smp, fem_wk%sk6)
!
      end subroutine sub3_skv_coef_to_ff_v_smp_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add6_skv_to_ff_t_smp_type(mesh, rhs_tbl, fem_wk, f_t)
!
      use cal_skv_to_ff_tensor_smp
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_t
!
!
      call add_skv_tensor_2_ff_smp                                      &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     f_t%ff_smp, fem_wk%sk6)
!
      end subroutine add6_skv_to_ff_t_smp_type
!
! ----------------------------------------------------------------------
!
      subroutine add6_skv_coef_to_ff_t_smp_type(mesh, rhs_tbl,          &
     &           coef, fem_wk, f_t)
!
      use cal_skv_to_ff_tensor_smp
!
      real (kind=kreal), intent(in)    :: coef
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_t
!
!
      call add_skv_tensor_coef_2_ff_smp                                 &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     coef, f_t%ff_smp, fem_wk%sk6)
!
      end subroutine add6_skv_coef_to_ff_t_smp_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub6_skv_to_ff_t_smp_type(mesh, rhs_tbl, fem_wk, f_t)
!
      use cal_skv_to_ff_tensor_smp
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_t
!
!
      call sub_skv_tensor_2_ff_smp                                      &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     f_t%ff_smp, fem_wk%sk6)
!
      end subroutine sub6_skv_to_ff_t_smp_type
!
! ----------------------------------------------------------------------
!
      subroutine sub6_skv_coef_to_ff_t_smp_type(mesh, rhs_tbl,          &
     &           coef, fem_wk, f_t)
!
      use cal_skv_to_ff_tensor_smp
!
      real (kind=kreal), intent(in)    :: coef
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(finite_ele_mat_node), intent(inout) :: f_t
!
!
      call sub_skv_tensor_coef_2_ff_smp                                 &
     &    (mesh%ele%numele, mesh%ele%nnod_4_ele,                        &
     &     np_smp, mesh%node%max_nod_smp, rhs_tbl%inod_ele_max,         &
     &     rhs_tbl%num_sort_smp, rhs_tbl%nod_stack_smp,                 &
     &     rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp,               &
     &     coef, f_t%ff_smp, fem_wk%sk6)
!
      end subroutine sub6_skv_coef_to_ff_t_smp_type
!
! ----------------------------------------------------------------------
!
      end module cal_skv_to_ff_smp_type
