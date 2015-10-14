!>@file   add_skv1_to_crs_matrix.f90
!!@brief  module add_skv1_to_crs_matrix
!!
!!@author H. Matsui
!!@author K. Nakajima and H. Matsui
!!@date        Written by H. Matsui on Jan., 2009
!!@n      modified by H. Matsui on Nov., 2013
!
!>     Matrix assemble for structure
!!
!!@verbatim
!!      subroutine add_skv1_to_crs_matrix11(ele, rhs_tbl, mat_tbl,      &
!!     &          k2, sk_v, nmat_size, aiccg)
!!      subroutine add_skv1_to_crs_matrix33(ele, rhs_tbl, mat_tbl,      &
!!     &          k2, sk_v, nmat_size, aiccg33)
!!@endverbatim
!
      module add_skv1_to_crs_matrix
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
      use t_table_FEM_const
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_to_crs_matrix11(ele, rhs_tbl, mat_tbl,        &
     &          k2, sk_v, nmat_size, aiccg)
!
      use add_skv1_2_matrix
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      integer (kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
!
      call add_skv1_2_matrix11(np_smp, ele%numele, ele%nnod_4_ele,      &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, mat_tbl%idx_4_mat, k2, sk_v,          &
     &    nmat_size, aiccg)
!
      end subroutine add_skv1_to_crs_matrix11
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_to_crs_matrix33(ele, rhs_tbl, mat_tbl,        &
     &          k2, sk_v, nmat_size, aiccg33)
!
      use add_skv1_2_matrix
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      integer (kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind = kreal), intent(inout) :: aiccg33(-8:nmat_size)
!
!
      call add_skv1_2_matrix33(np_smp, ele%numele, ele%nnod_4_ele,      &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, mat_tbl%idx_4_mat, k2, sk_v,          &
     &    nmat_size, aiccg33)
!
      end subroutine add_skv1_to_crs_matrix33
!
!-----------------------------------------------------------------------
!
      end module add_skv1_to_crs_matrix
