!cal_poisson_matrices_type.f90
!      module cal_poisson_matrices_type
!
! numerical integration for finite elememt equations(Poisson's equation)
!        programmed by H.Matsui on May, 2009
!
!
!      subroutine cal_scalar_diffuse_mat_type(ele, rhs_tbl,             &
!     &          djds_const, fem_wk, k2, coef_imp, ak_d, mat11)
!        integer (kind = kint), intent(in) :: k2
!        real(kind=kreal), intent(in) :: coef_imp
!        real(kind=kreal), intent(in) :: ak_d(ele%numele)
!        type(element_data), intent(in) :: ele
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(in) :: djds_const
!        type(work_finite_element_mat), intent(in) :: fem_wk
!        type(DJDS_MATRIX),  intent(inout) :: mat11
!      subroutine cal_vect_diffuse_mat_type(ele, rhs_tbl,               &
!     &          djds_const, fem_wk, k2, coef_imp, ak_d, mat33)
!        integer (kind = kint), intent(in) :: k2
!        real(kind=kreal), intent(in) :: coef_imp
!        real(kind=kreal), intent(in) :: ak_d(ele%numele)
!        type(element_data), intent(in) :: ele
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(in) :: djds_const
!        type(work_finite_element_mat), intent(in) :: fem_wk
!        type(DJDS_MATRIX),  intent(inout) :: mat33
!
!      subroutine cal_consist_coriolis_mat_type(ele, rhs_tbl,           &
!     &          djds_const, fem_wk, k2, mat33)
!
      module cal_poisson_matrices_type
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_geometry_data_MHD
      use t_table_FEM_const
      use t_finite_element_mat
      use t_solver_djds
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_scalar_diffuse_mat_type(ele, rhs_tbl,              &
     &          djds_const, fem_wk, k2, coef_imp, ak_d, mat11)
!
      use m_t_int_parameter
      use t_coefs_element_4_MHD
      use cal_diffuse_matrix
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      integer (kind = kint), intent(in) :: k2
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
!
      call cal_scalar_diffuse_evo_mat                                   &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, djds_const%idx_4_mat, coef_imp, dt,   &
     &    ak_d, k2, fem_wk%sk6, mat11%num_comp, mat11%aiccg)
!
      end subroutine cal_scalar_diffuse_mat_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_vect_diffuse_mat_type(ele, rhs_tbl,                &
     &          djds_const, fem_wk, k2, coef_imp, ak_d, mat33)
!
      use m_t_int_parameter
      use t_coefs_element_4_MHD
      use cal_diffuse_matrix
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      integer (kind = kint), intent(in) :: k2
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
!
      call cal_vector_diffuse_evo_mat                                   &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, djds_const%idx_4_mat, coef_imp, dt,   &
     &    ak_d, k2, fem_wk%sk6, mat33%num_comp, mat33%aiccg)
!
      end subroutine cal_vect_diffuse_mat_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_consist_coriolis_mat_type(ele, rhs_tbl,            &
     &          djds_const, fem_wk, k2, mat33)
!
      use t_coefs_element_4_MHD
      use m_t_int_parameter
      use m_physical_property
      use cal_coriolis_mat33
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      integer (kind = kint), intent(in) :: k2
!
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
!
      call cal_consist_coriolis_matrix                                  &
     &   (np_smp, ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,           &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, djds_const%idx_4_mat, k2,             &
     &    coef_cor, angular, fem_wk%sk6, mat33%num_comp, mat33%aiccg)
!
      end subroutine cal_consist_coriolis_mat_type
!
!-----------------------------------------------------------------------
!
      end module cal_poisson_matrices_type
