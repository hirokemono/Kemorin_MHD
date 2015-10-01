!int_vol_poisson_mat_1st.f90
!      module int_vol_poisson_mat_1st
!
!     Written by H. Matsui on Oct. 2005
!
!      subroutine int_vol_poisson_mat11(n_int, idx_for_mat,             &
!     &          nmat_size, aiccg)
!
!      subroutine int_vol_crank_mat11(n_int, idx_for_mat,               &
!     &          coef_imp, ak_d, nmat_size, aiccg)
!      subroutine int_vol_crank_mat33(n_int, idx_for_mat,               &
!     &          coef_imp, ak_d, nmat_size, aiccg33)
!
      module int_vol_poisson_mat_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_jacobians
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
      subroutine int_vol_poisson_mat11(n_int, idx_for_mat,              &
     &          nmat_size, aiccg)
!
      use fem_skv_diffusion_type
      use cal_poisson_matrices_1st
!
      integer(kind = kint), intent(in)                                  &
     &           :: idx_for_mat(rhs_tbl1%num_sort_smp, ele1%nnod_4_ele)
!
      integer(kind = kint), intent(in) :: n_int
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar)
        call fem_skv_poisson_linear_type(ele1%istack_ele_smp,           &
     &      n_int, k2, ele1, jac1_3d_l, sk6)
        call add_skv1_2_MHD_matrix11(idx_for_mat, k2, sk6,              &
     &      nmat_size, aiccg)
      end do
!
      end subroutine int_vol_poisson_mat11
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_crank_mat11(n_int, idx_for_mat,                &
     &          coef_imp, ak_d, nmat_size, aiccg)
!
      use fem_skv_diffusion_type
      use cal_poisson_matrices_1st
!
      integer(kind = kint), intent(in)                                  &
     &           :: idx_for_mat(rhs_tbl1%num_sort_smp, ele1%nnod_4_ele)
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      integer(kind = kint), intent(in) :: n_int
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar)
        call fem_skv_poisson_type(ele1%istack_ele_smp, n_int, k2,       &
     &      ele1, jac1_3d_q, sk6)
        call cal_scalar_diffuse_evo_mat_1st(idx_for_mat, k2,            &
     &      coef_imp, ak_d, sk6, nmat_size, aiccg)
      end do
!
      end subroutine int_vol_crank_mat11
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_crank_mat33(n_int, idx_for_mat,                &
     &          coef_imp, ak_d, nmat_size, aiccg33)
!
      use fem_skv_diffusion_type
      use cal_poisson_matrices_1st
!
      integer(kind = kint), intent(in)                                  &
     &           :: idx_for_mat(rhs_tbl1%num_sort_smp, ele1%nnod_4_ele)
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      integer(kind = kint), intent(in) :: n_int
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg33(-8:nmat_size)
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar)
        call fem_skv_poisson_type(ele1%istack_ele_smp, n_int, k2,       &
     &      ele1, jac1_3d_q, sk6)
        call cal_vect_diffuse_evo_mat_1st(idx_for_mat, k2,              &
     &      coef_imp, ak_d, sk6, nmat_size, aiccg33)
      end do
!
      end subroutine int_vol_crank_mat33
!
!-----------------------------------------------------------------------
!
      end module int_vol_poisson_mat_1st
