!int_vol_vect_cst_diff_1st.f90
!      module int_vol_vect_cst_diff_1st
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine int_vol_grad_w_const_1st(iele_fsmp_stack, num_int,    &
!     &          i_field, coef)
!      subroutine int_vol_div_w_const_1st(iele_fsmp_stack, num_int,     &
!     &          i_field, coef)
!      subroutine int_vol_rot_w_const_1st(iele_fsmp_stack, num_int,     &
!     &          i_field, coef)
!
!      subroutine int_vol_div_tsr_w_const_1st(iele_fsmp_stack, num_int, &
!     &          i_field, coef)
!      subroutine int_vol_div_as_tsr_w_const_1st(iele_fsmp_stack,       &
!     &          num_int, i_field, coef)
!
      module int_vol_vect_cst_diff_1st
!
      use m_precision
!
      use m_geometry_data
      use m_phys_constants
      use m_sorted_node
      use m_node_phys_address
      use m_node_phys_data
      use m_finite_element_matrix
      use m_jacobians
!
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
      use fem_skv_vector_diff_type
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_grad_w_const_1st(iele_fsmp_stack, num_int,     &
     &          i_field, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
        call scalar_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_field, coef, fem1_wk%scalar_1)
        call fem_skv_gradient(iele_fsmp_stack, num_int, k2,             &
     &      ele1, jac1_3d_q, fem1_wk%scalar_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_grad_w_const_1st
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_w_const_1st(iele_fsmp_stack, num_int,      &
     &          i_field, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
        call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_field, coef, fem1_wk%vector_1)
        call fem_skv_divergence(iele_fsmp_stack, num_int, k2,           &
     &      ele1, jac1_3d_q, fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_div_w_const_1st
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_rot_w_const_1st(iele_fsmp_stack, num_int,      &
     &          i_field, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
        call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_field, coef, fem1_wk%vector_1)
        call fem_skv_rotation(iele_fsmp_stack, num_int, k2,             &
     &      ele1, jac1_3d_q, fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_rot_w_const_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_tsr_w_const_1st(iele_fsmp_stack, num_int,  &
     &          i_field, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
        call tensor_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_field, coef, fem1_wk%tensor_1)
        call fem_skv_div_tensor(iele_fsmp_stack, num_int, k2,           &
     &      ele1, jac1_3d_q, fem1_wk%tensor_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_div_tsr_w_const_1st
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_as_tsr_w_const_1st(iele_fsmp_stack,        &
     &          num_int, i_field, coef)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
        call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_field, coef, fem1_wk%vector_1)
        call fem_skv_div_asym_tsr(iele_fsmp_stack, num_int, k2,         &
     &      ele1, jac1_3d_q, fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_div_as_tsr_w_const_1st
!
!-----------------------------------------------------------------------
!
      end module int_vol_vect_cst_diff_1st
