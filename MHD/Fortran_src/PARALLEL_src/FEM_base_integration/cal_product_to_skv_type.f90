!cal_product_to_skv_type.f90
!     module cal_product_to_skv_type
!
!        programmed by H.Matsui on May., 2009
!
!> @brief subroutines to obatine products of two fields
!>      need $omp parallel to use these routines
!
!      subroutine scalar_prod_to_skv_scalar_type(iele_fsmp_stack,       &
!     &          ak_1, ele, fem_wk)
!      subroutine coef_scalar_to_skv_scalar_type(iele_fsmp_stack,       &
!     &          coef, ak_1, ele, fem_wk)
!
!      subroutine scalar_prod_to_skv_vector_type(iele_fsmp_stack,       &
!     &          ak_1, ele, fem_wk)
!             fem_wk%sk6(:,:) = fem_wk%sk6(:,:)  * ak_1(:)
!      subroutine coef_scalar_to_skv_vect_type(iele_fsmp_stack,         &
!     &          coef, ak_1, ele, fem_wk)
!             fem_wk%sk6(:,:) = coef * fem_wk%sk6(:,:)  * ak_1(:)
!
!      subroutine scalar_prod_to_skv_tensor_type(iele_fsmp_stack,       &
!     &          ak_1, ele, fem_wk)
!      subroutine coef_scalar_to_skv_tensor_type(iele_fsmp_stack,       &
!     &          coef, ak_1, ele, fem_wk)
!
!      subroutine vector_prod_to_skv_vector_type(iele_fsmp_stack,       &
!     &          ak_3, ele, fem_wk)
!             fem_wk%sk6(:,:) = ak_3(:,nd) * fem_wk%sk6(:,nd)
!      subroutine tensor_prod_to_skv_tensor_type(iele_fsmp_stack,       &
!     &          ak_6, ele, fem_wk)
!             fem_wk%sk6(:,:) = ak_6(:,nd) * fem_wk%sk6(:,nd)
!
!        type(element_data), intent(in) :: ele
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        real (kind=kreal), intent(in) :: coef
!        real (kind=kreal), intent(in) :: ak_1(ele%numele)
!        real (kind=kreal), intent(in) :: ak_3(ele%numele,n_vector)
!        real (kind=kreal), intent(in) :: ak_6(ele%numele,n_sym_tensor)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
      module cal_product_to_skv_type
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
      subroutine coef_scalar_to_skv_scalar_type(iele_fsmp_stack,        &
     &          coef, ak_1, ele, fem_wk)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call coef_scalar_prod_to_skv_scalar(ele%numele, iele_fsmp_stack,  &
     &          ele%nnod_4_ele, coef, ak_1, fem_wk%sk6)
!
      end subroutine coef_scalar_to_skv_scalar_type
!
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_scalar_type(iele_fsmp_stack,        &
     &          ak_1, ele, fem_wk)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call scalar_prod_to_skv_scalar(ele%numele, iele_fsmp_stack,       &
     &          ele%nnod_4_ele, ak_1, fem_wk%sk6)
!
      end subroutine scalar_prod_to_skv_scalar_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_vector_type(iele_fsmp_stack,        &
     &          ak_1, ele, fem_wk)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call scalar_prod_to_skv_vector(ele%numele, iele_fsmp_stack,       &
     &            ele%nnod_4_ele, ak_1, fem_wk%sk6)
!
      end subroutine scalar_prod_to_skv_vector_type
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_to_skv_vect_type(iele_fsmp_stack,          &
     &          coef, ak_1, ele, fem_wk)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call coef_scalar_prod_to_skv_vector(ele%numele, iele_fsmp_stack,  &
     &          ele%nnod_4_ele, coef, ak_1, fem_wk%sk6)
!
      end subroutine coef_scalar_to_skv_vect_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_prod_to_skv_tensor_type(iele_fsmp_stack,        &
     &          ak_1, ele, fem_wk)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_1(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call scalar_prod_to_skv_tensor(ele%numele, iele_fsmp_stack,       &
     &          ele%nnod_4_ele, ak_1, fem_wk%sk6)
!
      end subroutine scalar_prod_to_skv_tensor_type
!
! ----------------------------------------------------------------------
!
      subroutine coef_scalar_to_skv_tensor_type(iele_fsmp_stack,        &
     &          coef, ak_1, ele, fem_wk)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_1(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call coef_scalar_prod_to_skv_tensor(ele%numele, iele_fsmp_stack,  &
     &          ele%nnod_4_ele, coef, ak_1, fem_wk%sk6)
!
      end subroutine coef_scalar_to_skv_tensor_type
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vector_prod_to_skv_vector_type(iele_fsmp_stack,        &
     &          ak_3, ele, fem_wk)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_3(ele%numele,n_vector)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call vector_prod_to_skv_vector(ele%numele, iele_fsmp_stack,       &
     &          ele%nnod_4_ele, ak_3, fem_wk%sk6)
!
      end subroutine vector_prod_to_skv_vector_type
!
! ----------------------------------------------------------------------
!
      subroutine tensor_prod_to_skv_tensor_type(iele_fsmp_stack,        &
     &          ak_6, ele, fem_wk)
!
      use cal_product_to_skv
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ak_6(ele%numele,n_sym_tensor)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call tensor_prod_to_skv_tensor(ele%numele, iele_fsmp_stack,       &
     &          ele%nnod_4_ele, ak_6, fem_wk%sk6)
!
      end subroutine tensor_prod_to_skv_tensor_type
!
! ----------------------------------------------------------------------
!
      end module cal_product_to_skv_type
