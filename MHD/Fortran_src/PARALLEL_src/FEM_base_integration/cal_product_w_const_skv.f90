!cal_product_w_const_skv.f90
!     module cal_product_w_const_skv
!
!        programmed by H.Matsui on May., 2009
!
!      need $omp parallel to use routines
!
!      subroutine prodct_w_const_skv_scalar(numele,                     &
!     &          iele_fsmp_stack, nnod_4_e1, coef, sk_v)
!      subroutine prodct_w_const_skv_vector(numele,                     &
!     &          iele_fsmp_stack, nnod_4_e1, coef, sk_v)
!      subroutine prodct_w_const_skv_tensor(numele,                     &
!     &          iele_fsmp_stack, nnod_4_e1, coef, sk_v)
!
!      subroutine cross_product_cvec_coef_skv(numele,                   &
!     &          iele_fsmp_stack, nnod_4_e1, coef, c_vec, sk_v)
!             sk_v(:,:) = coef * c_vec(:) \times sk_v(:,:)
!      subroutine cross_product_cvec_skv(numele,                        &
!     &          iele_fsmp_stack, nnod_4_e1, c_vec, sk_v)
!             sk_v(:,:) = c_vec(:) \times sk_v(:,:)
!
      module cal_product_w_const_skv
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
      subroutine prodct_w_const_skv_scalar(numele,                      &
     &          iele_fsmp_stack, nnod_4_e1, coef, sk_v)
!
      use overwrite_prod_const_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_coef_prod_scalar_smp(np_smp, numele,                 &
     &          iele_fsmp_stack, coef, sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine prodct_w_const_skv_scalar
!
! ----------------------------------------------------------------------
!
      subroutine prodct_w_const_skv_vector(numele,                      &
     &          iele_fsmp_stack, nnod_4_e1, coef, sk_v)
!
      use overwrite_prod_const_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_coef_prod_vect_smp(np_smp, numele,                   &
     &          iele_fsmp_stack, coef, sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine prodct_w_const_skv_vector
!
! ----------------------------------------------------------------------
!
      subroutine prodct_w_const_skv_tensor(numele,                      &
     &          iele_fsmp_stack, nnod_4_e1, coef, sk_v)
!
      use overwrite_prod_const_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_coef_prod_tensor_smp(np_smp, numele,                 &
     &          iele_fsmp_stack, coef, sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine prodct_w_const_skv_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cross_product_cvec_coef_skv(numele,                    &
     &          iele_fsmp_stack, nnod_4_e1, coef, c_vec, sk_v)
!
      use overwrite_prod_const_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_vect_prod_cvec_coef_smp(np_smp, numele,              &
     &          iele_fsmp_stack, coef, c_vec, sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine cross_product_cvec_coef_skv
!
! ----------------------------------------------------------------------
!
      subroutine cross_product_cvec_skv(numele,                         &
     &          iele_fsmp_stack, nnod_4_e1, c_vec, sk_v)
!
      use overwrite_prod_const_smp
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_e1
        call ovwrt_vect_prod_cvec_smp(np_smp, numele,                   &
     &          iele_fsmp_stack, c_vec, sk_v(1,1,k1))
      end do
!$omp end parallel
!
      end subroutine cross_product_cvec_skv
!
! ----------------------------------------------------------------------
!
      end module cal_product_w_const_skv
