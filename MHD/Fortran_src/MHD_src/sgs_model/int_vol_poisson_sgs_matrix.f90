!int_vol_poisson_sgs_matrix.f90
!      module int_vol_poisson_sgs_matrix
!
!     Written by H. Matsui on Oct. 2005
!
!      subroutine int_vol_poisson_sgs_mat11(n_int, idx_for_mat,         &
!     &          i_filter, ak_diff, nmat_size, aiccg)
!
!      subroutine int_vol_crank_sgs_mat11(n_int, idx_for_mat, coef_imp, &
!     &          i_filter, ak_diff, ak_d, nmat_size, aiccg)
!      subroutine int_vol_crank_sgs_mat33(n_int, idx_for_mat, coef_imp, &
!     &          i_filter, ak_diff, ak_d, nmat_size, aiccg33)
!
      module int_vol_poisson_sgs_matrix
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_finite_element_matrix
      use m_sorted_node
      use m_t_int_parameter
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_poisson_sgs_mat11(n_int, idx_for_mat,          &
     &          i_filter, ak_diff, nmat_size, aiccg)
!
      use fem_skv_diffusion_sgs_1st
      use cal_poisson_matrices_1st
!
      integer(kind = kint), intent(in)                                  &
     &               :: idx_for_mat(num_sort_smp, ele1%nnod_4_ele)
!
      integer(kind = kint), intent(in) :: n_int, i_filter
      real(kind=kreal), intent(in) :: ak_diff(ele1%numele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar)
        call fem_skv_poisson_linear_sgs_1st(ele1%istack_ele_smp,        &
     &      n_int, k2, i_filter, ak_diff, sk6)
        call add_skv1_2_MHD_matrix11(idx_for_mat, k2, sk6,              &
     &      nmat_size, aiccg)
      end do
!
      end subroutine int_vol_poisson_sgs_mat11
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_crank_sgs_mat11(n_int, idx_for_mat, coef_imp,  &
     &          i_filter, ak_diff, ak_d, nmat_size, aiccg)
!
      use fem_skv_diffusion_sgs_1st
      use cal_poisson_matrices_1st
!
      integer(kind = kint), intent(in)                                  &
     &               :: idx_for_mat(num_sort_smp, ele1%nnod_4_ele)
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
      real(kind=kreal), intent(in) :: ak_diff(ele1%numele)
!
      integer(kind = kint), intent(in) :: n_int, i_filter
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar)
        call fem_skv_poisson_sgs_1st(ele1%istack_ele_smp, n_int, k2,    &
     &      i_filter, ak_diff, sk6)
        call cal_scalar_diffuse_evo_mat_1st(idx_for_mat, k2,            &
     &      coef_imp, ak_d, sk6, nmat_size, aiccg)
      end do
!
      end subroutine int_vol_crank_sgs_mat11
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_crank_sgs_mat33(n_int, idx_for_mat, coef_imp,  &
     &          i_filter, ak_diff, ak_d, nmat_size, aiccg33)
!
      use fem_skv_diffusion_sgs_1st
      use cal_poisson_matrices_1st
!
      integer(kind = kint), intent(in)                                  &
     &               :: idx_for_mat(num_sort_smp, ele1%nnod_4_ele)
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
      real(kind=kreal), intent(in) :: ak_diff(ele1%numele)
!
      integer(kind = kint), intent(in) :: n_int, i_filter
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg33(-8:nmat_size)
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar)
        call fem_skv_poisson_sgs_1st(ele1%istack_ele_smp, n_int, k2,    &
     &      i_filter, ak_diff,  sk6)
        call cal_vect_diffuse_evo_mat_1st(idx_for_mat, k2,              &
     &      coef_imp, ak_d, sk6, nmat_size, aiccg33)
      end do
!
      end subroutine int_vol_crank_sgs_mat33
!
!-----------------------------------------------------------------------
!
      end module int_vol_poisson_sgs_matrix
