!cal_products_within_skv.f90
!     module cal_products_within_skv
!
!        programmed by H.Matsui on May., 2009
!
!> @brief subroutines to obatine products of two fields
!>      need $omp parallel to use these routines
!
!!      subroutine scalar_prod_to_scalar_skv(ele, iele_fsmp_stack,      &
!!     &          ntot_comp, i_field, ak_v, sk_v)
!!      subroutine coef_scalar_to_scalar_skv(ele, iele_fsmp_stack,      &
!!     &          coef, ntot_comp, i_field, ak_v, sk_v)
!!
!!      subroutine scalar_prod_to_vector_skv(ele, iele_fsmp_stack,      &
!!     &          ntot_comp, i_field, ak_v, sk_v)
!!             sk_v(:,:) = sk_v(:,:)  * ak_1(:)
!!      subroutine coef_scalar_to_vector_skv(ele, iele_fsmp_stack,      &
!!     &          coef, ntot_comp, i_field, ak_v, sk_v)
!!             sk_v(:,:) = coef * sk_v(:,:)  * ak_1(:)
!!
!!      subroutine scalar_prod_to_tensor_skv(ele, iele_fsmp_stack,      &
!!     &          ntot_comp, i_field, ak_v, sk_v)
!!      subroutine coef_scalar_to_tensor_skv(ele, iele_fsmp_stack,      &
!!     &          coef, ntot_comp, i_field, ak_v, sk_v)
!!
!!      subroutine vector_prod_vector_skv(ele, iele_fsmp_stack,         &
!!     &          ntot_comp, i_field, ak_v, sk_v)
!!             sk_v(:,:) = ak_3(:,nd) * sk_v(:,nd)
!!      subroutine tensor_prod_tensor_skv(ele, iele_fsmp_stack,         &
!!     &          ntot_comp, i_field, ak_v, sk_v)
!!             sk_v(:,:) = ak_6(:,nd) * sk_v(:,nd)
!
      module cal_products_within_skv
!
      use m_precision
!
      use m_phys_constants
      use t_geometry_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_scalar_skv(ele, iele_fsmp_stack,        &
     &          ntot_comp, i_field, ak_v, sk_v)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp, i_field
      real (kind=kreal), intent(in) :: ak_v(ele%numele,ntot_comp)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call scalar_prod_to_skv_scalar(ele%numele, iele_fsmp_stack,       &
     &    ele%nnod_4_ele, ak_v(1,i_field), sk_v)
!
      end subroutine scalar_prod_to_scalar_skv
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_to_scalar_skv(ele, iele_fsmp_stack,        &
     &          coef, ntot_comp, i_field, ak_v, sk_v)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: ntot_comp, i_field
      real (kind=kreal), intent(in) :: ak_v(ele%numele,ntot_comp)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call coef_scalar_prod_to_skv_scalar(ele%numele, iele_fsmp_stack,  &
     &    ele%nnod_4_ele, coef, ak_v(1,i_field), sk_v)
!
      end subroutine coef_scalar_to_scalar_skv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_vector_skv(ele, iele_fsmp_stack,        &
     &          ntot_comp, i_field, ak_v, sk_v)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp, i_field
      real (kind=kreal), intent(in) :: ak_v(ele%numele,ntot_comp)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call scalar_prod_to_skv_vector(ele%numele, iele_fsmp_stack,       &
     &    ele%nnod_4_ele, ak_v(1,i_field), sk_v)
!
      end subroutine scalar_prod_to_vector_skv
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_to_vector_skv(ele, iele_fsmp_stack,        &
     &          coef, ntot_comp, i_field, ak_v, sk_v)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: ntot_comp, i_field
      real (kind=kreal), intent(in) :: ak_v(ele%numele,ntot_comp)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call coef_scalar_prod_to_skv_vector(ele%numele, iele_fsmp_stack,  &
     &    ele%nnod_4_ele, coef, ak_v(1,i_field), sk_v)
!
      end subroutine coef_scalar_to_vector_skv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_tensor_skv(ele, iele_fsmp_stack,        &
     &          ntot_comp, i_field, ak_v, sk_v)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp, i_field
      real (kind=kreal), intent(in) :: ak_v(ele%numele,ntot_comp)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call scalar_prod_to_skv_tensor(ele%numele, iele_fsmp_stack,       &
     &    ele%nnod_4_ele, ak_v(1,i_field), sk_v)
!
      end subroutine scalar_prod_to_tensor_skv
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_to_tensor_skv(ele, iele_fsmp_stack,        &
     &          coef, ntot_comp, i_field, ak_v, sk_v)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: ntot_comp, i_field
      real (kind=kreal), intent(in) :: ak_v(ele%numele,ntot_comp)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call coef_scalar_prod_to_skv_tensor(ele%numele, iele_fsmp_stack,  &
     &    ele%nnod_4_ele, coef, ak_v(1,i_field), sk_v)
!
      end subroutine coef_scalar_to_tensor_skv
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vector_prod_vector_skv(ele, iele_fsmp_stack,           &
     &          ntot_comp, i_field, ak_v, sk_v)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp, i_field
      real (kind=kreal), intent(in) :: ak_v(ele%numele,ntot_comp)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call vector_prod_to_skv_vector(ele%numele, iele_fsmp_stack,       &
     &    ele%nnod_4_ele, ak_v(1,i_field), sk_v)
!
      end subroutine vector_prod_vector_skv
!
! ----------------------------------------------------------------------
!
      subroutine tensor_prod_tensor_skv(ele, iele_fsmp_stack,           &
     &          ntot_comp, i_field, ak_v, sk_v)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp, i_field
      real (kind=kreal), intent(in) :: ak_v(ele%numele,ntot_comp)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call tensor_prod_to_skv_tensor(ele%numele, iele_fsmp_stack,       &
     &    ele%nnod_4_ele, ak_v(1,i_field), sk_v)
!
      end subroutine tensor_prod_tensor_skv
!
! ----------------------------------------------------------------------
!
      end module cal_products_within_skv
