!cal_prod_w_const_skv_type.f90
!     module cal_prod_w_const_skv_type
!
!        programmed by H.Matsui on May., 2009
!
!      need $omp parallel to use routines
!
!      subroutine prod_w_const_skv_scalar_type(iele_fsmp_stack, coef,   &
!     &          ele, fem_wk)
!      subroutine prod_w_const_skv_vector_type(iele_fsmp_stack, coef,   &
!     &          ele, fem_wk)
!      subroutine prod_w_const_skv_tensor_type(iele_fsmp_stack, coef,   &
!     &          ele, fem_wk)
!
!      subroutine cross_prod_cvec_coef_skv_type(iele_fsmp_stack, coef,  &
!     &          c_vec, ele, fem_wk)
!             fem_wk%sk6(:,:) = coef * c_vec(:) \times fem_wk%sk6(:,:)
!      subroutine cross_prod_cvec_skv_type(iele_fsmp_stack, c_vec,      &
!     &          ele, fem_wk)
!             fem_wk%sk6(:,:) = c_vec(:) \times fem_wk%sk6(:,:)
!
      module cal_prod_w_const_skv_type
!
      use m_precision
!
      use t_geometry_data
      use t_finite_element_mat
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine prod_w_const_skv_scalar_type(iele_fsmp_stack, coef,    &
     &          ele, fem_wk)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call prodct_w_const_skv_scalar(ele%numele, iele_fsmp_stack,       &
     &          ele%nnod_4_ele, coef, fem_wk%sk6)
!
      end subroutine prod_w_const_skv_scalar_type
!
! ----------------------------------------------------------------------
!
      subroutine prod_w_const_skv_vector_type(iele_fsmp_stack, coef,    &
     &          ele, fem_wk)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call prodct_w_const_skv_vector(ele%numele, iele_fsmp_stack,       &
     &          ele%nnod_4_ele, coef, fem_wk%sk6)
!
      end subroutine prod_w_const_skv_vector_type
!
! ----------------------------------------------------------------------
!
      subroutine prod_w_const_skv_tensor_type(iele_fsmp_stack, coef,    &
     &          ele, fem_wk)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call prodct_w_const_skv_tensor(ele%numele, iele_fsmp_stack,       &
     &          ele%nnod_4_ele, coef, fem_wk%sk6)
!
      end subroutine prod_w_const_skv_tensor_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cross_prod_cvec_coef_skv_type(iele_fsmp_stack, coef,   &
     &          c_vec, ele, fem_wk)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: c_vec(3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call cross_product_cvec_coef_skv(ele%numele, iele_fsmp_stack,     &
     &          ele%nnod_4_ele, coef, c_vec, fem_wk%sk6)
!
      end subroutine cross_prod_cvec_coef_skv_type
!
! ----------------------------------------------------------------------
!
      subroutine cross_prod_cvec_skv_type(iele_fsmp_stack, c_vec,       &
     &          ele, fem_wk)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call cross_product_cvec_skv(ele%numele, iele_fsmp_stack,          &
     &          ele%nnod_4_ele, c_vec, fem_wk%sk6)
!
      end subroutine cross_prod_cvec_skv_type
!
! ----------------------------------------------------------------------
!
      end module cal_prod_w_const_skv_type
