!int_vol_inertia_1st.f90
!     module int_vol_inertia_1st
!
!      Written by H. Matsui on june, 2005
!
!!      subroutine int_vol_scalar_inertia_1st(iele_fsmp_stack,          &
!!     &          n_int, i_scalar, ncomp_ele, iele_velo, d_ele, coef)
!!      subroutine int_vol_vector_inertia_1st(iele_fsmp_stack,          &
!!     &          n_int, i_vector, ncomp_ele, iele_velo, d_ele, coef)
!!
!!      subroutine int_vol_rot_inertia_1st(iele_fsmp_stack,             &
!!     &          n_int, i_vector, ncomp_ele, iele_vort, d_ele, coef)
!!
!!      subroutine int_vol_scalar_inertia_upw_1st(iele_fsmp_stack,      &
!!     &          n_int, i_scalar, ncomp_ele, iele_velo, ie_upw, d_ele, &
!!     &          coef)
!!      subroutine int_vol_vector_inertia_upw_1st                       &
!!     &         (iele_fsmp_stack, n_int, i_vector,                     &
!!     &          ncomp_ele, iele_velo, ie_upw, d_ele, coef)
!!
!!      subroutine int_vol_rot_inertia_upw_1st(iele_fsmp_stack, n_int,  &
!!     &          i_vector, ncomp_ele, iele_vort, ie_upw, d_ele, coef)
!
      module int_vol_inertia_1st
!
      use m_precision
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
!
      use m_sorted_node
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_scalar_inertia_1st(iele_fsmp_stack,            &
     &          n_int, i_scalar, ncomp_ele, iele_velo, d_ele, coef)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_type
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_scalar, coef, fem1_wk%scalar_1)
        call fem_skv_scalar_inertia_type(iele_fsmp_stack, n_int, k2,    &
     &      fem1_wk%scalar_1, d_ele(1,iele_velo), ele1, jac1_3d_q,      &
     &      fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_scalar_inertia_1st
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vector_inertia_1st(iele_fsmp_stack,            &
     &          n_int, i_vector, ncomp_ele, iele_velo, d_ele, coef)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_type
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_vector, coef, fem1_wk%vector_1)
        call fem_skv_vector_inertia_type(iele_fsmp_stack, n_int, k2,    &
     &      fem1_wk%vector_1, d_ele(1,iele_velo), ele1, jac1_3d_q,      &
     &      fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_vector_inertia_1st
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_rot_inertia_1st(iele_fsmp_stack,               &
     &          n_int, i_vector, ncomp_ele, iele_vort, d_ele, coef)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_type
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_vort
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_vector, coef, fem1_wk%vector_1)
        call fem_skv_rot_inertia_type(iele_fsmp_stack, n_int, k2,       &
     &      fem1_wk%vector_1, d_ele(1,iele_vort), ele1, jac1_3d_q,      &
     &      fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_rot_inertia_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_scalar_inertia_upw_1st(iele_fsmp_stack,        &
     &          n_int, i_scalar, ncomp_ele, iele_velo, ie_upw, d_ele,   &
     &          coef)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_upw_type
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_scalar, coef, fem1_wk%scalar_1)
        call fem_skv_scalar_inertia_upwind(iele_fsmp_stack, n_int, k2,  &
     &      fem1_wk%scalar_1, d_ele(1,iele_velo), d_ele(1,ie_upw),      &
     &      ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_scalar_inertia_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vector_inertia_upw_1st                         &
     &         (iele_fsmp_stack, n_int, i_vector,                       &
     &          ncomp_ele, iele_velo, ie_upw, d_ele, coef)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_upw_type
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_vector, coef, fem1_wk%vector_1)
        call fem_skv_vector_inertia_upwind(iele_fsmp_stack, n_int, k2,  &
     &      fem1_wk%vector_1, d_ele(1,iele_velo), d_ele(1,ie_upw),      &
     &      ele1, jac1_3d_q,  fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_vector_inertia_upw_1st
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_rot_inertia_upw_1st(iele_fsmp_stack, n_int,    &
     &          i_vector, ncomp_ele, iele_vort, ie_upw, d_ele, coef)
!
      use cal_skv_to_ff_smp
      use nodal_fld_cst_to_element
      use fem_skv_nonlinear_upw_type
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_vort, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the physical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, i_vector, coef, fem1_wk%vector_1)
        call fem_skv_rot_inertia_upwind(iele_fsmp_stack, n_int, k2,     &
     &      fem1_wk%vector_1, d_ele(1,iele_vort), d_ele(1,ie_upw),      &
     &      ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_rot_inertia_upw_1st
!
! ----------------------------------------------------------------------
!
      end module int_vol_inertia_1st
