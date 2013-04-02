!cal_product_to_skv.f90
!     module cal_product_to_skv
!
!        programmed by H.Matsui on May., 2009
!
!> @brief subroutines to obatine products of two fields
!>      need $omp parallel to use these routines
!
!      subroutine scalar_prod_to_skv_scalar(numele,                     &
!     &          iele_fsmp_stack, nnod_4_e1, ak_1, sk_v)
!      subroutine coef_scalar_prod_to_skv_scalar(numele,                &
!     &          iele_fsmp_stack, nnod_4_e1, coef, ak_1, sk_v)
!
!      subroutine scalar_prod_to_skv_vector(numele,                     &
!     &          iele_fsmp_stack, nnod_4_e1, ak_1, sk_v)
!             sk_v(:,:) = sk_v(:,:)  * ak_1(:)
!      subroutine coef_scalar_prod_to_skv_vector(numele,                &
!     &          iele_fsmp_stack, nnod_4_e1, coef, ak_1, sk_v)
!             sk_v(:,:) = coef * sk_v(:,:)  * ak_1(:)
!
!      subroutine scalar_prod_to_skv_tensor(numele,                     &
!     &          iele_fsmp_stack, nnod_4_e1, ak_1, sk_v)
!      subroutine coef_scalar_prod_to_skv_tensor(numele,                &
!     &          iele_fsmp_stack, nnod_4_e1, coef, ak_1, sk_v)
!
!      subroutine vector_prod_to_skv_vector(numele,                     &
!     &          iele_fsmp_stack, nnod_4_e1, ak_3, sk_v)
!             sk_v(:,:) = ak_3(:,nd) * sk_v(:,nd)
!      subroutine tensor_prod_to_skv_tensor(numele,                     &
!     &          iele_fsmp_stack, nnod_4_e1, ak_6, sk_v)
!             sk_v(:,:) = ak_6(:,nd) * sk_v(:,nd)
!
      module cal_product_to_skv
!
      use m_precision
!
      use m_machine_parameter
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
      subroutine coef_scalar_prod_to_skv_scalar(numele,                 &
     &          iele_fsmp_stack, nnod_4_e1, coef, ak_1, sk_v)
!
      use overwrite_products_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_scalar_prod_w_coef_smp(np_smp, numele,               &
     &      iele_fsmp_stack, coef, ak_1(1), sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine coef_scalar_prod_to_skv_scalar
!
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_scalar(numele,                      &
     &          iele_fsmp_stack, nnod_4_e1, ak_1, sk_v)
!
      use overwrite_products_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_scalar_prod_no_coef_smp(np_smp, numele,              &
     &      iele_fsmp_stack, ak_1(1), sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine scalar_prod_to_skv_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_vector(numele,                      &
     &          iele_fsmp_stack, nnod_4_e1, ak_1, sk_v)
!
      use overwrite_products_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_vec_scalar_prod_smp(np_smp, numele,                  &
     &      iele_fsmp_stack, ak_1(1), sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine scalar_prod_to_skv_vector
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_prod_to_skv_vector(numele,                 &
     &          iele_fsmp_stack, nnod_4_e1, coef, ak_1, sk_v)
!
      use overwrite_products_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_vec_scalar_coef_smp(np_smp, numele,                  &
     &      iele_fsmp_stack, coef, ak_1(1), sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine coef_scalar_prod_to_skv_vector
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_tensor(numele,                      &
     &          iele_fsmp_stack, nnod_4_e1, ak_1, sk_v)
!
      use overwrite_products_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_tensor_scalar_prod_smp(np_smp, numele,               &
     &      iele_fsmp_stack, ak_1(1), sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine scalar_prod_to_skv_tensor
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_prod_to_skv_tensor(numele,                 &
     &          iele_fsmp_stack, nnod_4_e1, coef, ak_1, sk_v)
!
      use overwrite_products_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_tensor_scalar_coef_smp(np_smp, numele,               &
     &      iele_fsmp_stack, coef, ak_1(1), sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine coef_scalar_prod_to_skv_tensor
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vector_prod_to_skv_vector(numele,                      &
     &          iele_fsmp_stack, nnod_4_e1, ak_3, sk_v)
!
      use overwrite_products_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_3(numele,n_vector)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_vector_vector_prod_smp(np_smp, numele,               &
     &      iele_fsmp_stack, ak_3(1,1), sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine vector_prod_to_skv_vector
!
! ----------------------------------------------------------------------
!
      subroutine tensor_prod_to_skv_tensor(numele,                      &
     &          iele_fsmp_stack, nnod_4_e1, ak_6, sk_v)
!
      use overwrite_products_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_6(numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_tensor_tensor_prod_smp(np_smp, numele,               &
     &      iele_fsmp_stack, ak_6(1,1), sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine tensor_prod_to_skv_tensor
!
! ----------------------------------------------------------------------
!
      end module cal_product_to_skv
