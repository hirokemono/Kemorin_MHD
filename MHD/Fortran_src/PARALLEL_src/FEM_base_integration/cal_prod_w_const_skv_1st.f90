!cal_prod_w_const_skv_1st.f90
!     module cal_prod_w_const_skv_1st
!
!        programmed by H.Matsui on May., 2009
!
!      need $omp parallel to use routines
!
!      subroutine prod_w_const_skv_scalar_1st(iele_fsmp_stack,          &
!     &          coef, sk_v)
!      subroutine prod_w_const_skv_vector_1st(iele_fsmp_stack,          &
!     &          coef, sk_v)
!      subroutine prod_w_const_skv_tensor_1st(iele_fsmp_stack,          &
!     &          coef, sk_v)
!
!      subroutine cross_prod_cvec_coef_skv_1st(iele_fsmp_stack,         &
!     &          coef, c_vec, sk_v)
!             sk_v(:,:) = coef * c_vec(:) \times sk_v(:,:)
!      subroutine cross_prod_cvec_skv_1st(iele_fsmp_stack,              &
!     &          c_vec, sk_v)
!             sk_v(:,:) = c_vec(:) \times sk_v(:,:)
!
      module cal_prod_w_const_skv_1st
!
      use m_precision
!
      use m_geometry_data
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
      subroutine prod_w_const_skv_scalar_1st(iele_fsmp_stack,           &
     &          coef, sk_v)
!
      use cal_product_w_const_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call prodct_w_const_skv_scalar(ele1%numele,                       &
     &          iele_fsmp_stack, ele1%nnod_4_ele, coef, sk_v)
!
      end subroutine prod_w_const_skv_scalar_1st
!
! ----------------------------------------------------------------------
!
      subroutine prod_w_const_skv_vector_1st(iele_fsmp_stack,           &
     &          coef, sk_v)
!
      use cal_product_w_const_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call prodct_w_const_skv_vector(ele1%numele,                       &
     &          iele_fsmp_stack, ele1%nnod_4_ele, coef, sk_v)
!
      end subroutine prod_w_const_skv_vector_1st
!
! ----------------------------------------------------------------------
!
      subroutine prod_w_const_skv_tensor_1st(iele_fsmp_stack,           &
     &          coef, sk_v)
!
      use cal_product_w_const_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call prodct_w_const_skv_tensor(ele1%numele,                       &
     &          iele_fsmp_stack, ele1%nnod_4_ele, coef, sk_v)
!
      end subroutine prod_w_const_skv_tensor_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cross_prod_cvec_coef_skv_1st(iele_fsmp_stack,          &
     &          coef, c_vec, sk_v)
!
      use cal_product_w_const_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call cross_product_cvec_coef_skv(ele1%numele,                     &
     &          iele_fsmp_stack, ele1%nnod_4_ele, coef, c_vec, sk_v)
!
      end subroutine cross_prod_cvec_coef_skv_1st
!
! ----------------------------------------------------------------------
!
      subroutine cross_prod_cvec_skv_1st(iele_fsmp_stack,               &
     &          c_vec, sk_v)
!
      use cal_product_w_const_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call cross_product_cvec_skv(ele1%numele,                          &
     &          iele_fsmp_stack, ele1%nnod_4_ele, c_vec, sk_v)
!
      end subroutine cross_prod_cvec_skv_1st
!
! ----------------------------------------------------------------------
!
      end module cal_prod_w_const_skv_1st
