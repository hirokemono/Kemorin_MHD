!cal_product_to_skv_1st.f90
!     module cal_product_to_skv_1st
!
!        programmed by H.Matsui on May., 2009
!
!> @brief subroutines to obatine products of two fields
!>      need $omp parallel to use these routines
!
!      subroutine scalar_prod_to_skv_scalar_1st(iele_fsmp_stack,        &
!     &          ak_1, sk_v)
!      subroutine coef_scalar_to_skv_scalar_1st(iele_fsmp_stack,        &
!     &          coef, ak_1, sk_v)
!
!      subroutine scalar_prod_to_skv_vector_1st(iele_fsmp_stack,        &
!     &          ak_1, sk_v)
!             sk_v(:,:) = sk_v(:,:)  * ak_1(:)
!      subroutine coef_scalar_to_skv_vect_1st(iele_fsmp_stack,          &
!     &          coef, ak_1, sk_v)
!             sk_v(:,:) = coef * sk_v(:,:)  * ak_1(:)
!
!      subroutine scalar_prod_to_skv_tensor_1st(iele_fsmp_stack,        &
!     &          ak_1, sk_v)
!      subroutine coef_scalar_to_skv_tensor_1st(iele_fsmp_stack,        &
!     &          coef, ak_1, sk_v)
!
!      subroutine vector_prod_to_skv_vector_1st(iele_fsmp_stack,        &
!     &          ak_3, sk_v)
!             sk_v(:,:) = ak_3(:,nd) * sk_v(:,nd)
!      subroutine tensor_prod_to_skv_tensor_1st(iele_fsmp_stack,        &
!     &          ak_6, sk_v)
!             sk_v(:,:) = ak_6(:,nd) * sk_v(:,nd)
!
      module cal_product_to_skv_1st
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
      subroutine coef_scalar_to_skv_scalar_1st(iele_fsmp_stack,         &
     &          coef, ak_1, sk_v)
!
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call coef_scalar_prod_to_skv_scalar(ele1%numele, iele_fsmp_stack, &
     &          nnod_4_ele, coef, ak_1, sk_v)
!
      end subroutine coef_scalar_to_skv_scalar_1st
!
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_scalar_1st(iele_fsmp_stack,         &
     &          ak_1, sk_v)
!
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call scalar_prod_to_skv_scalar(ele1%numele, iele_fsmp_stack,      &
     &          nnod_4_ele, ak_1, sk_v)
!
      end subroutine scalar_prod_to_skv_scalar_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_vector_1st(iele_fsmp_stack,         &
     &          ak_1, sk_v)
!
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call scalar_prod_to_skv_vector(ele1%numele, iele_fsmp_stack,      &
     &            nnod_4_ele, ak_1, sk_v)
!
      end subroutine scalar_prod_to_skv_vector_1st
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_to_skv_vect_1st(iele_fsmp_stack,           &
     &          coef, ak_1, sk_v)
!
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call coef_scalar_prod_to_skv_vector(ele1%numele, iele_fsmp_stack, &
     &          nnod_4_ele, coef, ak_1, sk_v)
!
      end subroutine coef_scalar_to_skv_vect_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_tensor_1st(iele_fsmp_stack,         &
     &          ak_1, sk_v)
!
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call scalar_prod_to_skv_tensor(ele1%numele, iele_fsmp_stack,      &
     &          nnod_4_ele, ak_1, sk_v)
!
      end subroutine scalar_prod_to_skv_tensor_1st
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_to_skv_tensor_1st(iele_fsmp_stack,         &
     &          coef, ak_1, sk_v)
!
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call coef_scalar_prod_to_skv_tensor(ele1%numele, iele_fsmp_stack, &
     &          nnod_4_ele, coef, ak_1, sk_v)
!
      end subroutine coef_scalar_to_skv_tensor_1st
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vector_prod_to_skv_vector_1st(iele_fsmp_stack,         &
     &          ak_3, sk_v)
!
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_3(ele1%numele,n_vector)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call vector_prod_to_skv_vector(ele1%numele, iele_fsmp_stack,      &
     &          nnod_4_ele, ak_3, sk_v)
!
      end subroutine vector_prod_to_skv_vector_1st
!
! ----------------------------------------------------------------------
!
      subroutine tensor_prod_to_skv_tensor_1st(iele_fsmp_stack,         &
     &          ak_6, sk_v)
!
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_6(ele1%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call tensor_prod_to_skv_tensor(ele1%numele, iele_fsmp_stack,      &
     &          nnod_4_ele, ak_6, sk_v)
!
      end subroutine tensor_prod_to_skv_tensor_1st
!
! ----------------------------------------------------------------------
!
      end module cal_product_to_skv_1st
