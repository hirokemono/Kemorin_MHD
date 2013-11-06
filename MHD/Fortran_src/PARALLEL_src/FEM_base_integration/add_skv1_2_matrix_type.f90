!>@file   add_skv1_2_matrix_type.f90
!!@brief  module add_skv1_2_matrix_type
!!
!!@author H. Matsui
!!@author K. Nakajima and H. Matsui
!!@date        Written by H. Matsui on Jan., 2009
!!@n      modified by H. Matsui on Nov., 2013
!
!>     Matrix assemble for structure
!!
!!@verbatim
!!      subroutine add_skv1_2_matrix11_type(ele, rhs_tbl, idx_4_mat,    &
!!     &          sk_v, k2, mat11)
!!      subroutine add_skv1_2_matrix33_type(ele, rhs_tbl, idx_4_mat,    &
!!     &          sk_v, k2, mat33)
!!@endverbatim
!
      module add_skv1_2_matrix_type
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
      use t_table_FEM_const
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
      subroutine add_skv1_2_matrix11_type(ele, rhs_tbl, idx_4_mat,      &
     &          sk_v, k2, mat11)
!
      use add_skv1_2_matrix
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      integer(kind = kint), intent(in)                                  &
     &            :: idx_4_mat(rhs_tbl%num_sort_smp,ele%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
!
      call add_skv1_2_matrix11(np_smp, ele%numele, ele%nnod_4_ele,      &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, idx_4_mat, k2, sk_v,                  &
     &    mat11%num_non0, mat11%aiccg)
!
      end subroutine add_skv1_2_matrix11_type
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_2_matrix33_type(ele, rhs_tbl, idx_4_mat,      &
     &          sk_v, k2, mat33)
!
      use add_skv1_2_matrix
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      integer(kind = kint), intent(in)                                  &
     &            :: idx_4_mat(rhs_tbl%num_sort_smp,ele%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
!
      call add_skv1_2_matrix33(np_smp, ele%numele, ele%nnod_4_ele,      &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, idx_4_mat, k2, sk_v,                  &
     &    mat33%num_non0, mat33%aiccg)
!
      end subroutine add_skv1_2_matrix33_type
!
!-----------------------------------------------------------------------
!
      end module add_skv1_2_matrix_type
