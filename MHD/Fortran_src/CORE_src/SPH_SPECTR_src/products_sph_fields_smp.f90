!>@file   products_sph_fields_smp.f90
!!@brief  module products_sph_fields_smp
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief subroutines to obatine products of two fields on spherical grid
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_rtp_product_4_scalar(i_s1, i_s2, i_r)
!!                d_rtp(,i_r) = d_rtp(,i_v1) * d_rtp(,i_v2)
!!      subroutine cal_rtp_dot_product(i_v1, i_v2, i_r)
!!                d_rtp(,i_r) = d_rtp(,i_v1) \cdot d_rtp(,i_v2)
!!      subroutine cal_rtp_cross_product(i_v1, i_v2, i_r)
!!                d_rtp(,i_r) = d_rtp(,i_v1) \times d_rtp(,i_v2)
!!
!!      subroutine cal_rtp_dot_prod_w_coef(coef, i_v1, i_v2, i_r)
!!                d_rtp(,i_r) = coef * d_rtp(,i_v1) \cdot d_rtp(,i_v2)
!!      subroutine cal_rtp_cross_prod_w_coef(coef, i_v1, i_v2, i_r)
!!                d_rtp(,i_r) = coef * d_rtp(,i_v1) \times d_rtp(,i_v2)
!!      subroutine cal_rtp_tri_product(coef, i_v1, i_v2, i_v3, i_r)
!!
!!      subroutine cal_rtp_scalar_product_vector(i_v1, i_s1, i_r)
!!                d_rtp(,i_r  ) = d_rtp(,i_s1) * d_rtp(,i_v1  )
!!                d_rtp(,i_r+1) = d_rtp(,i_s1) * d_rtp(,i_v1+1)
!!                d_rtp(,i_r+2) = d_rtp(,i_s1) * d_rtp(,i_v1+2)
!!      subroutine cal_rtp_sym_matvec(i_t1, i_v2, i_r)
!!                d_rtp(,i_r  ) =  d_rtp(,i_t1  )*d_rtp(,i_v2  )      &
!!     &                         + d_rtp(,i_t1+1)*d_rtp(,i_v2+1)      &
!!     &                         + d_rtp(,i_t1+2)*d_rtp(,i_v2+2)
!!                d_rtp(,i_r+1) =  d_rtp(,i_t1+1)*d_rtp(,i_v2  )      &
!!     &                         + d_rtp(,i_t1+3)*d_rtp(,i_v2+1)      &
!!     &                         + d_rtp(,i_t1+4)*d_rtp(,i_v2+2)
!!                d_rtp(,i_r+2) =  d_rtp(,i_t1+2)*d_rtp(,i_v2  )      &
!!     &                         + d_rtp(,i_t1+4)*d_rtp(,i_v2+1)      &
!!     &                         + d_rtp(,i_t1+5)*d_rtp(,i_v2+2)
!!       subroutine prod_rtp_scalar_mag_vector(i_s1, i_v2, i_r)
!!
!!       subroutine rtp_vec_scalar_prod_w_coef(coef, i_v1,i_s1, i_r)
!!                d_rtp(,i_r  ) = coef * d_rtp(,i_s1) * d_rtp(,i_v1  )
!!                d_rtp(,i_r+1) = coef * d_rtp(,i_s1) * d_rtp(,i_v1+1)
!!                d_rtp(,i_r+2) = coef * d_rtp(,i_s1) * d_rtp(,i_v1+2)
!!@endverbatim
!!
!!@n @param  i_s1     Scalar field address for d_rtp
!!@n @param  i_s2     Scalar field address for d_rtp
!!@n @param  i_v1     Vector field address for d_rtp
!!@n @param  i_v2     Vector field address for d_rtp
!!@n @param  i_v3     Vector field address for d_rtp
!!@n @param  i_t1     Symmetric tensor field address for d_rtp
!!@n @param  coef     Scalar coefficients
!!
!!@n @param  i_r      Result field address for d_rtp
!
      module products_sph_fields_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_product_4_scalar(i_s1, i_s2, i_r)
!
      use cal_products_smp
!
      integer (kind = kint), intent(in) :: i_r, i_s1, i_s2
!
!
      call cal_scalar_prod_no_coef_smp(np_smp, nnod_rtp,                &
     &    inod_rtp_smp_stack, d_rtp(1,i_s1), d_rtp(1,i_s2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_dot_product(i_v1, i_v2, i_r)
!
      use cal_products_smp
!
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_dot_prod_no_coef_smp(np_smp, nnod_rtp,                   &
     &    inod_rtp_smp_stack, d_rtp(1,i_v1), d_rtp(1,i_v2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_dot_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_cross_product(i_v1, i_v2, i_r)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_cross_prod_no_coef_smp(np_smp, nnod_rtp,                 &
     &    inod_rtp_smp_stack, d_rtp(1,i_v1), d_rtp(1,i_v2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_cross_product
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_dot_prod_w_coef(coef, i_v1, i_v2, i_r)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_dot_prod_w_coef_smp(np_smp, nnod_rtp,                    &
     &    inod_rtp_smp_stack, coef, d_rtp(1,i_v1), d_rtp(1,i_v2),       &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_dot_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_cross_prod_w_coef(coef, i_v1, i_v2, i_r)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_cross_prod_w_coef_smp(np_smp, nnod_rtp,                  &
     &    inod_rtp_smp_stack, coef, d_rtp(1,i_v1), d_rtp(1,i_v2),       &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_cross_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_tri_product(coef, i_v1, i_v2, i_v3, i_r)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r
      integer (kind = kint), intent(in) :: i_v1, i_v2, i_v3
!
!
      call cal_tri_product_w_coef_smp(np_smp, nnod_rtp,                 &
     &    inod_rtp_smp_stack, coef, d_rtp(1,i_v1), d_rtp(1,i_v2),       &
     &    d_rtp(1,i_v3), d_rtp(1,i_r) )
!
      end subroutine cal_rtp_tri_product
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_scalar_product_vector(i_v1, i_s1, i_r)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v1
!
!
      call cal_vec_scalar_prod_no_coef_smp(np_smp, nnod_rtp,            &
     &    inod_rtp_smp_stack, d_rtp(1,i_v1), d_rtp(1,i_s1),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_scalar_product_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_rtp_sym_matvec(i_t1, i_v2, i_r)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_t1, i_v2
!
!
      call cal_tensor_vec_prod_no_coef_smp(np_smp, nnod_rtp,            &
     &    inod_rtp_smp_stack, d_rtp(1,i_t1), d_rtp(1,i_v2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine cal_rtp_sym_matvec
!
!-----------------------------------------------------------------------
!
      subroutine prod_rtp_scalar_mag_vector(i_s1, i_v2, i_r)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
!
!
      call cal_scalar_mag_vector_prod_smp(np_smp, nnod_rtp,             &
     &    inod_rtp_smp_stack, d_rtp(1,i_s1), d_rtp(1,i_v2),             &
     &    d_rtp(1,i_r) )
!
      end subroutine prod_rtp_scalar_mag_vector
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine rtp_vec_scalar_prod_w_coef(coef, i_v1, i_s1, i_r)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v1
!
!
      call cal_vec_scalar_prod_w_coef_smp(np_smp, nnod_rtp,             &
     &    inod_rtp_smp_stack, coef, d_rtp(1,i_v1), d_rtp(1,i_s1),       &
     &    d_rtp(1,i_r) )
!
      end subroutine rtp_vec_scalar_prod_w_coef
!
!-----------------------------------------------------------------------
!
      end module products_sph_fields_smp
