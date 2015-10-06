!int_vol_sgs_fractional_1st.f90
!     module int_vol_sgs_fractional_1st
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_sgs_div_v_linear_1st(iele_fsmp_stack,         &
!     &          n_int, i_vector, i_filter, iak_diff)
!      subroutine int_vol_sgs_solenoidal_co_1st(iele_fsmp_stack,        &
!     &          n_int, i_scalar, i_filter, iak_diff)
!
!      subroutine int_vol_scalar_sgs_diffuse_1st(iele_fsmp_stack,       &
!     &          n_int, coef_crank, ak_d, i_scalar, i_filter, iak_diff)
!      subroutine int_vol_vector_sgs_diffuse_1st(iele_fsmp_stack,       &
!     &          n_int, coef_crank, ak_d, i_vector, i_filter, iak_diff)
!
      module int_vol_sgs_fractional_1st
!
      use m_precision
      use m_geometry_data
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
      use m_filter_elength
      use m_SGS_model_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sgs_div_v_linear_1st(iele_fsmp_stack,          &
     &          n_int, i_vector, i_filter, iak_diff)
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_2_each_ele_1st
      use fem_skv_diffs_sgs_type
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind = kint), intent(in) :: i_filter, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, i_vector, fem1_wk%vector_1)
        call fem_skv_div_sgs_linear(iele_fsmp_stack,                    &
     &      n_int, k2, i_filter, ak_diff(1,iak_diff),                   &
     &      ele1, jac1_3d_q, jac1_3d_l, FEM1_elen,                      &
     &      fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, fem1_wk%sk6)
!
      end subroutine int_vol_sgs_div_v_linear_1st
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sgs_solenoidal_co_1st(iele_fsmp_stack,         &
     &          n_int, i_scalar, i_filter, iak_diff)
!
!      use m_control_parameter
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_2_each_ele_1st
      use fem_skv_diffs_sgs_type
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: i_filter, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, num_t_linear
        call scalar_phys_2_each_element(k2, i_scalar, fem1_wk%scalar_1)
        call fem_skv_grad_sgs_linear(iele_fsmp_stack,                   &
     &      n_int, k2, i_filter, ak_diff(1,iak_diff),                   &
     &      ele1, jac1_3d_q, jac1_3d_l, FEM1_elen,                      &
     &      fem1_wk%scalar_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_sgs_solenoidal_co_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_scalar_sgs_diffuse_1st(iele_fsmp_stack,        &
     &          n_int, coef_crank, ak_d, i_scalar, i_filter, iak_diff)
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_2_each_ele_1st
      use fem_skv_diffusion_sgs_type
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: i_filter, iak_diff
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call scalar_phys_2_each_element(k2, i_scalar, fem1_wk%scalar_1)
        call fem_skv_scalar_diffuse_sgs_type(iele_fsmp_stack,           &
     &      n_int, k2, i_filter, ak_diff(1,iak_diff), ak_d,             &
     &      ele1, jac1_3d_q, FEM1_elen, fem1_wk%scalar_1, fem1_wk%sk6)
      end do
!
      call add1_skv_coef_to_ff_v_smp_1st                                &
     &   (coef_crank, ff_smp, fem1_wk%sk6)
!
      end subroutine int_vol_scalar_sgs_diffuse_1st
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vector_sgs_diffuse_1st(iele_fsmp_stack,        &
     &          n_int, coef_crank, ak_d, i_vector, i_filter, iak_diff)
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_2_each_ele_1st
      use fem_skv_diffusion_sgs_type
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: i_filter, iak_diff
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, i_vector, fem1_wk%vector_1)
        call fem_skv_vector_diffuse_sgs_type(iele_fsmp_stack,           &
     &      n_int, k2, i_filter, ak_diff(1,iak_diff), ak_d,             &
     &      ele1, jac1_3d_q, FEM1_elen, fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_coef_to_ff_v_smp_1st                                &
     &   (coef_crank, ff_smp, fem1_wk%sk6)
!
      end subroutine int_vol_vector_sgs_diffuse_1st
!
!  ---------------------------------------------------------------------
!
      end module int_vol_sgs_fractional_1st
