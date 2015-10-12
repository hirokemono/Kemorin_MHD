!cal_prod_w_const_skv.f90
!     module cal_prod_w_const_skv
!
!        programmed by H.Matsui on May., 2009
!
!      need $omp parallel to use routines
!
!      subroutine prod_w_const_skv_scalar(ele, iele_fsmp_stack,         &
!     &          coef, sk_v)
!      subroutine prod_w_const_skv_vector(ele, iele_fsmp_stack,         &
!     &          coef, sk_v)
!      subroutine prod_w_const_skv_tensor(ele, iele_fsmp_stack,         &
!     &          coef, sk_v)
!
!      subroutine cross_prod_cvec_coef_skv(ele, iele_fsmp_stack,        &
!     &          coef, c_vec, sk_v)
!             sk_v(:,:) = coef * c_vec(:) \times sk_v(:,:)
!      subroutine cross_prod_cvec_skv                                   &
!     &         (ele, iele_fsmp_stack, c_vec, sk_v)
!        type(element_data), intent(in) :: ele
!             sk_v(:,:) = c_vec(:) \times sk_v(:,:)
!
      module cal_prod_w_const_skv
!
      use m_precision
!
      use t_geometry_data
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
      subroutine prod_w_const_skv_scalar(ele, iele_fsmp_stack,          &
     &          coef, sk_v)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call prodct_w_const_skv_scalar(ele%numele,                        &
     &          iele_fsmp_stack, ele%nnod_4_ele, coef, sk_v)
!
      end subroutine prod_w_const_skv_scalar
!
! ----------------------------------------------------------------------
!
      subroutine prod_w_const_skv_vector(ele, iele_fsmp_stack,          &
     &          coef, sk_v)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call prodct_w_const_skv_vector(ele%numele, iele_fsmp_stack,       &
     &    ele%nnod_4_ele, coef, sk_v)
!
      end subroutine prod_w_const_skv_vector
!
! ----------------------------------------------------------------------
!
      subroutine prod_w_const_skv_tensor(ele, iele_fsmp_stack,          &
     &          coef, sk_v)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call prodct_w_const_skv_tensor(ele%numele, iele_fsmp_stack,       &
     &    ele%nnod_4_ele, coef, sk_v)
!
      end subroutine prod_w_const_skv_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cross_prod_cvec_coef_skv(ele, iele_fsmp_stack,         &
     &          coef, c_vec, sk_v)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call cross_product_cvec_coef_skv(ele%numele, iele_fsmp_stack,     &
     &    ele%nnod_4_ele, coef, c_vec, sk_v)
!
      end subroutine cross_prod_cvec_coef_skv
!
! ----------------------------------------------------------------------
!
      subroutine cross_prod_cvec_skv                                    &
     &         (ele, iele_fsmp_stack, c_vec, sk_v)
!
      use cal_product_w_const_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call cross_product_cvec_skv(ele%numele, iele_fsmp_stack,          &
     &    ele%nnod_4_ele, c_vec, sk_v)
!
      end subroutine cross_prod_cvec_skv
!
! ----------------------------------------------------------------------
!
      end module cal_prod_w_const_skv
