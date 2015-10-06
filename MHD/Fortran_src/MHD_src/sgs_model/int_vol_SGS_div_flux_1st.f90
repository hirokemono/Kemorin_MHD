!int_vol_SGS_div_flux_1st.f90
!     module int_vol_SGS_div_flux_1st
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_div_SGS_vec_flux                              &
!     &         (iele_fsmp_stack, n_int,  i_vector, i_scalar,           &
!     &          i_SGS_flux, i_filter, iak_diff, coef)
!      subroutine int_vol_div_SGS_tsr_flux                              &
!     &         (iele_fsmp_stack, n_int, i_vector, i_SGS_flux,          &
!     &          i_filter, iak_diff, coef)
!
!      subroutine int_vol_div_SGS_vec_flux_upw                          &
!     &        (iele_fsmp_stack, n_int, i_vector, i_scalar, i_SGS_flux, &
!     &         i_filter, iak_diff, ncomp_ele, ie_upw, d_ele, coef)
!      subroutine int_vol_div_SGS_tsr_flux_upw                          &
!     &         (iele_fsmp_stack, n_int, i_vector, i_SGS_flux,          &
!     &          i_filter, iak_diff, ncomp_ele, ie_upw, d_ele, coef)
!
      module int_vol_SGS_div_flux_1st
!
      use m_precision
      use m_control_parameter
      use m_geometry_data
!
      use m_finite_element_matrix
      use m_SGS_model_coefs
      use m_int_vol_data
      use m_jacobians
      use m_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_vec_flux                               &
     &         (iele_fsmp_stack, n_int,  i_vector, i_scalar,            &
     &          i_SGS_flux, i_filter, iak_diff, coef)
!
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_div_sgs_flux_type
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vector, i_scalar
      integer(kind = kint), intent(in) :: i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter, iak_diff
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call SGS_vector_cst_each_ele_1st(k2, i_vector,  i_scalar,       &
     &      i_SGS_flux, coef, sgs_e, fem1_wk%vector_1)
        call fem_skv_div_sgs_vector(iele_fsmp_stack, n_int, k2,         &
     &      i_filter, ak_diff(1,iak_diff), ele1, jac1_3d_q, FEM1_elen,  &
     &      sgs_e, fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_div_SGS_vec_flux
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_tsr_flux                               &
     &         (iele_fsmp_stack, n_int, i_vector, i_SGS_flux,           &
     &          i_filter, iak_diff, coef)
!
      use nodal_fld_cst_to_ele_1st
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_div_sgs_flux_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vector, i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter, iak_diff
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call SGS_tensor_cst_each_ele_1st(k2, i_vector,                  &
     &      i_SGS_flux, coef, sgs_t, fem1_wk%tensor_1)
        call fem_skv_div_sgs_tensor(iele_fsmp_stack, n_int, k2,         &
     &      i_filter, ak_diff(1,iak_diff), ele1, jac1_3d_q, FEM1_elen,  &
     &      sgs_t, fem1_wk%tensor_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_div_SGS_tsr_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_vec_flux_upw                           &
     &        (iele_fsmp_stack, n_int, i_vector, i_scalar, i_SGS_flux,  &
     &         i_filter, iak_diff, ncomp_ele, ie_upw, d_ele, coef)
!
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_div_sgs_flux_upw_t
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vector, i_scalar
      integer(kind = kint), intent(in) :: i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter, iak_diff
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call SGS_vector_cst_each_ele_1st(k2, i_vector,  i_scalar,       &
     &      i_SGS_flux, coef, sgs_e, fem1_wk%vector_1)
        call fem_skv_div_sgs_vector_upwind(iele_fsmp_stack, n_int, k2,  &
     &      i_filter, ak_diff(1,iak_diff), ele1, jac1_3d_q, FEM1_elen,  &
     &      d_ele(1,ie_upw), sgs_e, fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_div_SGS_vec_flux_upw
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_tsr_flux_upw                           &
     &         (iele_fsmp_stack, n_int, i_vector, i_SGS_flux,           &
     &          i_filter, iak_diff, ncomp_ele, ie_upw, d_ele, coef)
!
      use nodal_fld_cst_to_ele_1st
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_div_sgs_flux_upw_t
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vector, i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter, iak_diff
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call SGS_tensor_cst_each_ele_1st(k2, i_vector,                  &
     &      i_SGS_flux, coef, sgs_t, fem1_wk%tensor_1)
        call fem_skv_div_sgs_tensor_upwind(iele_fsmp_stack, n_int, k2,  &
     &      i_filter, ak_diff(1,iak_diff), ele1, jac1_3d_q, FEM1_elen,  &
     &      d_ele(1,ie_upw), sgs_t, fem1_wk%tensor_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_div_SGS_tsr_flux_upw
!
! ----------------------------------------------------------------------
!
      end module int_vol_SGS_div_flux_1st
