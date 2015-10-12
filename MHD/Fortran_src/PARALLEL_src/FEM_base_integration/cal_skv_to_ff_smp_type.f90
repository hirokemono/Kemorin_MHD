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
!      subroutine reset_sk6_type(numdir, ele, fem_wk)
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
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(work_finite_element_mat), intent(in) :: fem_wk
!        type(finite_ele_mat_node), intent(inout) :: f_v
!
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
      subroutine reset_sk6_type(numdir, ele, fem_wk)
!
      use cal_skv_to_ff_vector_smp
!
      integer(kind = kint), intent(in) :: numdir
      type(element_data), intent(in) :: ele
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call reset_skv_vector                                             &
     &   (ele%numele, ele%nnod_4_ele, numdir, fem_wk%sk6)
!
      end subroutine reset_sk6_type
!
! ----------------------------------------------------------------------
!
      end module cal_skv_to_ff_smp_type
