!int_sgs_vect_cst_diff_upw_1.f90
!      module int_sgs_vect_cst_diff_upw_1
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine int_sgs_grad_w_const_upw_1(iele_fsmp_stack, num_int,  &
!     &          i_filter, iak_diff, i_field, coef,                     &
!     &          ncomp_ele, iv_up, d_ele)
!      subroutine int_sgs_div_w_const_upw_1(iele_fsmp_stack, num_int,   &
!     &          i_filter, iak_diff, i_field, coef,                     &
!     &          ncomp_ele, iv_up, d_ele)
!      subroutine int_sgs_rot_w_const_upw_1(iele_fsmp_stack, num_int,   &
!     &          i_filter, iak_diff, i_field, coef,                     &
!     &          ncomp_ele, iv_up, d_ele)
!
!      subroutine int_sgs_div_tsr_w_const_upw_1(iele_fsmp_stack,        &
!     &          num_int, i_filter, iak_diff, i_field, coef, iv_up)
!     &          ncomp_ele, iv_up, d_ele)
!      subroutine int_sgs_div_as_tsr_w_const_upw_1(iele_fsmp_stack,     &
!     &          num_int, i_filter, iak_diff, i_field, coef, iv_up,     &
!     &          ncomp_ele, iv_up, d_ele)
!
      module int_sgs_vect_cst_diff_upw_1
!
      use m_precision
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
      use m_filter_elength
!
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_diffs_sgs_upw_type
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_grad_w_const_upw_1(iele_fsmp_stack, num_int,   &
     &          i_filter, iak_diff, i_field, coef,                      &
     &          ncomp_ele, iv_up, d_ele)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(k2, i_field, coef,  phi_e)
        call fem_skv_gradient_upw(iele_fsmp_stack, num_int, k2,         &
     &      d_ele(1,iv_up), ele1, jac1_3d_q, phi_e, sk6)
        call fem_skv_grad_sgs_upwind(iele_fsmp_stack, num_int, k2,      &
     &      i_filter, ak_diff(1,iak_diff), ele1, jac1_3d_q, FEM1_elen,  &
     &      d_ele(1,iv_up), phi_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_sgs_grad_w_const_upw_1
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_w_const_upw_1(iele_fsmp_stack, num_int,    &
     &          i_filter, iak_diff, i_field, coef,                      &
     &          ncomp_ele, iv_up, d_ele)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, i_field, coef, vect_e)
        call fem_skv_div_sgs_upwind(iele_fsmp_stack, num_int, k2,       &
     &      i_filter, ak_diff(1,iak_diff), ele1, jac1_3d_q, FEM1_elen,  &
     &      d_ele(1,iv_up), vect_e, sk6)
   end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_sgs_div_w_const_upw_1
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_rot_w_const_upw_1(iele_fsmp_stack, num_int,    &
     &          i_filter, iak_diff, i_field, coef,                      &
     &          ncomp_ele, iv_up, d_ele)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, i_field, coef, vect_e)
        call fem_skv_rot_sgs_upwind(iele_fsmp_stack, num_int, k2,       &
     &      i_filter, ak_diff(1,iak_diff), ele1, jac1_3d_q, FEM1_elen,  &
     &      d_ele(1,iv_up), vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_sgs_rot_w_const_upw_1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_tsr_w_const_upw_1(iele_fsmp_stack,         &
     &          num_int, i_filter, iak_diff, i_field, coef,             &
     &          ncomp_ele, iv_up, d_ele)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call tensor_cst_phys_2_each_ele(k2, i_field, coef, tensor_e)
        call fem_skv_div_tsr_sgs_upwind(iele_fsmp_stack, num_int, k2,   &
     &      i_filter, ak_diff(1,iak_diff), ele1, jac1_3d_q, FEM1_elen,  &
     &      d_ele(1,iv_up), tensor_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_sgs_div_tsr_w_const_upw_1
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_as_tsr_w_const_upw_1(iele_fsmp_stack,      &
     &          num_int, i_filter, iak_diff, i_field, coef,             &
     &          ncomp_ele, iv_up, d_ele)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, i_field, coef, vect_e)
        call fem_skv_div_as_tsr_sgs_upwind(iele_fsmp_stack,             &
     &     num_int, k2, i_filter, ak_diff(1,iak_diff),                  &
     &     ele1, jac1_3d_q, FEM1_elen, d_ele(1,iv_up), vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_sgs_div_as_tsr_w_const_upw_1
!
!-----------------------------------------------------------------------
!
      end module int_sgs_vect_cst_diff_upw_1
