!cal_poisson_matrices_1st.f90
!      module cal_poisson_matrices_1st
!
!     Written by H. Matsui on Oct. 2005
!
!      subroutine add_skv1_2_MHD_matrix11(idx_for_mat, k2,              &
!     &          sk_v, nmat_size, aiccg)
!      subroutine add_skv1_2_MHD_matrix33(idx_for_mat, k2,              &
!     &          sk_v, nmat_size, aiccg33)
!
!      subroutine cal_scalar_diffuse_evo_mat_1st(idx_for_mat, k2,       &
!     &          coef_imp, ak_d, sk_v, nmat_size, aiccg)
!      subroutine cal_vect_diffuse_evo_mat_1st(idx_for_mat, k2,         &
!     &          coef_imp, ak_d, sk_v, nmat_size, aiccg33)
!
!      subroutine cal_consist_coriolis_mat_1st(idx_for_mat, k2,         &
!     &          sk_v, nmat_size, aiccg33)
!
      module cal_poisson_matrices_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_sorted_node
      use m_phys_constants
      use m_t_int_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_2_MHD_matrix11(idx_for_mat, k2,               &
     &          sk_v, nmat_size, aiccg)
!
      use add_skv1_2_matrix
!
      integer (kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in)                                  &
     &               :: idx_for_mat(num_sort_smp, ele1%nnod_4_ele)
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
!
      call add_skv1_2_matrix11(np_smp, ele1%numele, ele1%nnod_4_ele,    &
     &    inod_ele_max, num_sort_smp, nod_stack_smp, iele_sort_smp,     &
     &    iconn_sort_smp, idx_for_mat, k2, sk_v, nmat_size, aiccg)
!
      end subroutine add_skv1_2_MHD_matrix11
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_2_MHD_matrix33(idx_for_mat, k2,               &
     &          sk_v, nmat_size, aiccg33)
!
      use add_skv1_2_matrix
!
      integer (kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in)                                  &
     &               :: idx_for_mat(num_sort_smp, ele1%nnod_4_ele)
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg33(-8:nmat_size)
!
!
      call add_skv1_2_matrix33(np_smp, ele1%numele, ele1%nnod_4_ele,    &
     &    inod_ele_max, num_sort_smp, nod_stack_smp, iele_sort_smp,     &
     &    iconn_sort_smp, idx_for_mat, k2, sk_v, nmat_size, aiccg33)
!
      end subroutine add_skv1_2_MHD_matrix33
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_scalar_diffuse_evo_mat_1st(idx_for_mat, k2,        &
     &          coef_imp, ak_d, sk_v, nmat_size, aiccg)
!
      use cal_diffuse_matrix
!
      integer (kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in)                                  &
     &               :: idx_for_mat(num_sort_smp, ele1%nnod_4_ele)
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
!
      call cal_scalar_diffuse_evo_mat                                   &
     &   (np_smp, ele1%numele, ele1%nnod_4_ele,                         &
     &    inod_ele_max, num_sort_smp, nod_stack_smp, iele_sort_smp,     &
     &    iconn_sort_smp, idx_for_mat, coef_imp, dt, ak_d, k2, sk_v,    &
     &    nmat_size, aiccg)
!
      end subroutine cal_scalar_diffuse_evo_mat_1st
!
!-----------------------------------------------------------------------
!
      subroutine cal_vect_diffuse_evo_mat_1st(idx_for_mat, k2,          &
     &          coef_imp, ak_d, sk_v, nmat_size, aiccg33)
!
      use cal_diffuse_matrix
!
      integer (kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in)                                  &
     &               :: idx_for_mat(num_sort_smp, ele1%nnod_4_ele)
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg33(-8:nmat_size)
!
!
      call cal_vector_diffuse_evo_mat                                   &
     &   (np_smp, ele1%numele, ele1%nnod_4_ele,                         &
     &    inod_ele_max, num_sort_smp, nod_stack_smp, iele_sort_smp,     &
     &    iconn_sort_smp, idx_for_mat, coef_imp, dt, ak_d, k2, sk_v,    &
     &    nmat_size, aiccg33)
!
      end subroutine cal_vect_diffuse_evo_mat_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_consist_coriolis_mat_1st(idx_for_mat, k2,          &
     &          sk_v, nmat_size, aiccg33)
!
      use m_physical_property
      use cal_coriolis_mat33
!
      integer (kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in)                                  &
     &               :: idx_for_mat(num_sort_smp, ele1%nnod_4_ele)
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg33(-8:nmat_size)
!
!
      call cal_consist_coriolis_matrix                                  &
     &   (np_smp, ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,        &
     &     inod_ele_max, num_sort_smp, nod_stack_smp,                   &
     &     iele_sort_smp, iconn_sort_smp, idx_for_mat, k2,              &
     &     coef_cor, angular, sk_v, nmat_size, aiccg33)
!
      end subroutine cal_consist_coriolis_mat_1st
!
!-----------------------------------------------------------------------
!
      end module cal_poisson_matrices_1st
