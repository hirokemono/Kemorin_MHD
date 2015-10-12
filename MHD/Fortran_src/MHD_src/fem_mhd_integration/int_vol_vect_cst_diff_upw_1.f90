!int_vol_vect_cst_diff_upw_1.f90
!      module int_vol_vect_cst_diff_upw_1
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine int_vol_grad_w_const_upw_1(iele_fsmp_stack, num_int,  &
!     &          i_field, ncomp_ele, iv_up, d_ele, coef)
!      subroutine int_vol_div_w_const_upw_1(iele_fsmp_stack, num_int,   &
!     &          i_field, ncomp_ele, iv_up, d_ele, coef)
!      subroutine int_vol_rot_w_const_upw_1(iele_fsmp_stack, num_int,   &
!     &          i_field, ncomp_ele, iv_up, d_ele, coef)
!
!      subroutine int_vol_div_tsr_w_const_upw_1(iele_fsmp_stack,        &
!     &          num_int, i_field, ncomp_ele, iv_up, d_ele, coef)
!      subroutine int_vol_div_as_tsr_cst_upw_1(iele_fsmp_stack,         &
!     &          num_int, i_field, ncomp_ele, iv_up, d_ele, coef)
!
      module int_vol_vect_cst_diff_upw_1
!
      use m_precision
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_sorted_node
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_vect_diff_upw_type
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_grad_w_const_upw_1(iele_fsmp_stack, num_int,   &
     &          i_field, ncomp_ele, iv_up, d_ele, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele                                 &
     &     (k2, i_field, coef, fem1_wk%scalar_1)
        call fem_skv_gradient_upw(iele_fsmp_stack, num_int, k2,         &
     &      d_ele(1,iv_up), ele1, jac1_3d_q,                            &
     &      fem1_wk%scalar_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_vol_grad_w_const_upw_1
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_w_const_upw_1(iele_fsmp_stack, num_int,    &
     &          i_field, ncomp_ele, iv_up, d_ele, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele                                 &
     &     (k2, i_field, coef, fem1_wk%vector_1)
        call fem_skv_divergence_upw(iele_fsmp_stack, num_int, k2,       &
     &      d_ele(1,iv_up), ele1, jac1_3d_q,                            &
     &      fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_vol_div_w_const_upw_1
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_rot_w_const_upw_1(iele_fsmp_stack, num_int,    &
     &          i_field, ncomp_ele, iv_up, d_ele, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele                                 &
     &     (k2, i_field, coef, fem1_wk%vector_1)
        call fem_skv_rotation_upw(iele_fsmp_stack, num_int, k2,         &
     &      d_ele(1,iv_up), ele1, jac1_3d_q,                            &
     &      fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_vol_rot_w_const_upw_1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_tsr_w_const_upw_1(iele_fsmp_stack,         &
     &          num_int, i_field, ncomp_ele, iv_up, d_ele, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call tensor_cst_phys_2_each_ele                                 &
     &     (k2, i_field, coef, fem1_wk%tensor_1)
        call fem_skv_div_tsr_upw(iele_fsmp_stack, num_int, k2,          &
     &      d_ele(1,iv_up), ele1, jac1_3d_q,                            &
     &      fem1_wk%tensor_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_vol_div_tsr_w_const_upw_1
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_as_tsr_cst_upw_1(iele_fsmp_stack,          &
     &          num_int, i_field, ncomp_ele, iv_up, d_ele, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: coef
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele                                 &
     &     (k2, i_field, coef, fem1_wk%vector_1)
        call fem_skv_div_as_tsr_upw(iele_fsmp_stack, num_int, k2,       &
     &      d_ele(1,iv_up), ele1, jac1_3d_q,                            &
     &      fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_vol_div_as_tsr_cst_upw_1
!
!-----------------------------------------------------------------------
!
      end module int_vol_vect_cst_diff_upw_1
