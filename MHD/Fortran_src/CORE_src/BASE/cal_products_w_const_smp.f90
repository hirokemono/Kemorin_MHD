!>@file   cal_products_w_const_smp.f90
!!@brief  module cal_products_w_const_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Obtain products of field with constant field
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_coef_prod_scalar_smp(nnod, coef, scalar2, prod)
!!      subroutine cal_coef_prod_vect_smp(nnod, coef, vect2, prod)
!!      subroutine cal_coef_prod_tensor_smp(nnod, coef, tensor2, prod)
!!
!!      subroutine cal_dot_prod_cvec_w_coef_smp                         &
!!     &         (nnod, coef, c_vec, vect2, prod)
!!             prod(:) = coef * c_vec(:) \cdot vect2(:,:)
!!      subroutine cal_dot_prod_cvec_no_coef_smp                        &
!!     &         (nnod, c_vec, vect2, prod)
!!             prod(:) = c_vec(:) \cdot vect2(:,:)
!!
!!      subroutine cal_vect_prod_cvec_w_coef_smp                        &
!!     &         (nnod, coef, c_vec, vect2, prod)
!!             prod(:,:) = coef * c_vec(:) \times vect2(:,:)
!!      subroutine cal_vect_prod_cvec_no_coef_smp                       &
!!     &         (nnod, c_vec, vect2, prod)
!!             prod(:,:) = c_vec(:) \times vect2(:,:)
!!@endverbatim
!!
!!@n @param  nnod     Number of data points
!!@n @param  coef     scalar coefficient
!!@n @param  c_vec(3)          constant vector
!!@n @param  scalar2(nnod)     Input scalar data 2
!!@n @param  vect2(nnod,3)     Input vector data 2
!!@n @param  tensor2(nnod,6)   Input symmetric tenso data 2
!!
!!@n @param  prod(nnod,NB)     Product
!!                      (scalar, vector, or symmetric tensor)
!
      module cal_products_w_const_smp
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_coef_prod_scalar_smp(nnod, coef, scalar2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: scalar2(nnod)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!
!$omp workshare
      prod(1:nnod) =  scalar2(1:nnod)*coef
!$omp end workshare nowait
!
      end subroutine cal_coef_prod_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_coef_prod_vect_smp(nnod, coef, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: coef, vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp workshare
      prod(1:nnod,1) =  coef*vect2(1:nnod,1)
      prod(1:nnod,2) =  coef*vect2(1:nnod,2)
      prod(1:nnod,3) =  coef*vect2(1:nnod,3)
!$omp end workshare nowait
!
      end subroutine cal_coef_prod_vect_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_coef_prod_tensor_smp(nnod, coef, tensor2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: coef, tensor2(nnod,6)
!
      real (kind=kreal), intent(inout) :: prod(nnod,6)
!
!
!$omp workshare
      prod(1:nnod,1) =  coef*tensor2(1:nnod,1)
      prod(1:nnod,2) =  coef*tensor2(1:nnod,2)
      prod(1:nnod,3) =  coef*tensor2(1:nnod,3)
      prod(1:nnod,4) =  coef*tensor2(1:nnod,4)
      prod(1:nnod,5) =  coef*tensor2(1:nnod,5)
      prod(1:nnod,6) =  coef*tensor2(1:nnod,6)
!$omp end workshare nowait
!
      end subroutine cal_coef_prod_tensor_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_dot_prod_cvec_w_coef_smp                           &
     &         (nnod, coef, c_vec, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: c_vec(3), vect2(nnod,3)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!
!$omp workshare
      prod(1:nnod) = (c_vec(1)*vect2(1:nnod,1)                          &
     &              + c_vec(2)*vect2(1:nnod,2)                          &
     &              + c_vec(3)*vect2(1:nnod,3)) * coef
!$omp end workshare nowait
!
      end subroutine cal_dot_prod_cvec_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_dot_prod_cvec_no_coef_smp                          &
     &         (nnod, c_vec, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: c_vec(3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
!
!$omp workshare
      prod(1:nnod) =  c_vec(1)*vect2(1:nnod,1)                          &
     &              + c_vec(2)*vect2(1:nnod,2)                          &
     &              + c_vec(3)*vect2(1:nnod,3)
!$omp end workshare nowait
!
      end subroutine cal_dot_prod_cvec_no_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_vect_prod_cvec_w_coef_smp                          &
     &         (nnod, coef, c_vec, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: c_vec(3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp workshare
      prod(1:nnod,1) = (c_vec(2)*vect2(1:nnod,3)                        &
     &                - c_vec(3)*vect2(1:nnod,2) ) * coef
      prod(1:nnod,2) = (c_vec(3)*vect2(1:nnod,1)                        &
     &                - c_vec(1)*vect2(1:nnod,3) ) * coef
      prod(1:nnod,3) = (c_vec(1)*vect2(1:nnod,2)                        &
     &                - c_vec(2)*vect2(1:nnod,1) ) * coef
!$omp end workshare nowait
!
      end subroutine cal_vect_prod_cvec_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_prod_cvec_no_coef_smp                         &
     &         (nnod, c_vec, vect2, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: c_vec(3), vect2(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
!
!$omp workshare
      prod(1:nnod,1) = (c_vec(2)*vect2(1:nnod,3)                        &
     &                - c_vec(3)*vect2(1:nnod,2) )
      prod(1:nnod,2) = (c_vec(3)*vect2(1:nnod,1)                        &
     &                - c_vec(1)*vect2(1:nnod,3) )
      prod(1:nnod,3) = (c_vec(1)*vect2(1:nnod,2)                        &
     &                - c_vec(2)*vect2(1:nnod,1) )
!$omp end workshare nowait
!
      end subroutine cal_vect_prod_cvec_no_coef_smp
!
! ----------------------------------------------------------------------
!
      end module cal_products_w_const_smp
