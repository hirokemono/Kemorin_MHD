!>@file   products_nodal_fields_smp.f90
!!@brief  module products_nodal_fields_smp
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Subroutines to obatine products of two nodal fields
!!       in structure
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_phys_product_4_scalar(i_s1, i_s2, i_r, nod_fld)
!!                d_nod(,i_r) = d_nod(,i_v1) * d_nod(,i_v2)
!!      subroutine cal_phys_dot_product(i_v1, i_v2, i_r, nod_fld)
!!                d_nod(,i_r) = d_nod(,i_v1) \times d_nod(,i_v2)
!!      subroutine cal_phys_cross_product(i_v1, i_v2, i_r, nod_fld)
!!                d_nod(,i_r) = d_nod(,i_v1) \cdot d_nod(,i_v2)
!!
!!      subroutine cal_phys_dot_prod_w_coef                             &
!!     &         (i_v1, i_v2, i_r, coef, nod_fld)
!!                d_nod(,i_r) = coef * d_nod(,i_v1) \cdot d_nod(,i_v2)
!!      subroutine cal_phys_cross_prod_w_coef                           &
!!     &         (i_v1, i_v2, i_r, coef, nod_fld)
!!                d_nod(,i_r) = coef * d_nod(,i_v1) \times d_nod(,i_v2)
!!      subroutine cal_tri_product_4_scalar                             &
!!     &         (i_v1, i_v2, i_v3, i_r, coef, nod_fld)
!!
!!      subroutine cal_phys_scalar_product_vector                       &
!!     &          (i_v1, i_s1, i_r, nod_fld)
!!                d_nod(,i_r  ) = d_nod(,i_s1) * d_nod(,i_v1  )
!!                d_nod(,i_r+1) = d_nod(,i_s1) * d_nod(,i_v1+1)
!!                d_nod(,i_r+2) = d_nod(,i_s1) * d_nod(,i_v1+2)
!!      subroutine cal_phys_sym_matvec(i_t1, i_v2, i_r, nod_fld)
!!        d_nod(,i_r  ) =  d_nod(,i_t1  )*d_nod(,i_v2  )      &
!!     &                 + d_nod(,i_t1+1)*d_nod(,i_v2+1)      &
!!     &                 + d_nod(,i_t1+2)*d_nod(,i_v2+2)
!!        d_nod(,i_r+1) =  d_nod(,i_t1+1)*d_nod(,i_v2  )      &
!!     &                 + d_nod(,i_t1+3)*d_nod(,i_v2+1)      &
!!     &                 + d_nod(,i_t1+4)*d_nod(,i_v2+2)
!!        d_nod(,i_r+2) =  d_nod(,i_t1+2)*d_nod(,i_v2  )      &
!!     &                 + d_nod(,i_t1+4)*d_nod(,i_v2+1)      &
!!     &                 + d_nod(,i_t1+5)*d_nod(,i_v2+2)
!!      subroutine prod_phys_scalar_mag_vector(i_s1, i_v2, i_r, nod_fld)
!!      subroutine phys_vec_scalar_prod_w_coef                          &
!!     &         (i_v1, i_s1, i_r, coef, nod_fld)
!!        type(phys_data), intent(inout) :: nod_fld
!!
!!      subroutine multi_by_const_nod_scalar(i_v1, i_r, const, nod_fld)
!!      subroutine multi_by_const_nod_vector(i_v1, i_r, const, nod_fld)
!!      subroutine multi_by_const_nod_tensor(i_v1, i_r, const, nod_fld)
!!        integer(kind = kint), intent(in) :: i_r, i_v1
!!        real(kind = kreal), intent(in) :: const
!!  !!        type(phys_data), intent(inout) :: nod_fld
!!
!!         d_nod(inod,i_r) =  const * d_nod(inod,i_v1)
!!        i_r: result field ID
!!        i_v1: source field IDs
!!@endverbatim
!!
!!@n @param  i_s1     Scalar field address for d_nod
!!@n @param  i_s2     Scalar field address for d_nod
!!@n @param  i_v1     Vector field address for d_nod
!!@n @param  i_v2     Vector field address for d_nod
!!@n @param  i_v3     Vector field address for d_nod
!!@n @param  i_t1     Symmetric tensor field address for d_nod
!!@n @param  coef     Scalar coefficients
!!
!!@n @param  i_r      Result field address for d_nod
!!@n @param  nod_fld  structure of nodal field data
!
      module products_nodal_fields_smp
!
      use m_precision
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_product_4_scalar(i_s1, i_s2, i_r, nod_fld)
!
      use cal_products_smp
!
      integer (kind = kint), intent(in) :: i_r, i_s1, i_s2
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_scalar_prod_no_coef_smp(nod_fld%n_point,                 &
     &    nod_fld%d_fld(1,i_s1), nod_fld%d_fld(1,i_s2),                 &
     &    nod_fld%d_fld(1,i_r) )
!
      end subroutine cal_phys_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_dot_product(i_v1, i_v2, i_r, nod_fld)
!
      use cal_products_smp
!
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_dot_prod_no_coef_smp(nod_fld%n_point,                    &
     &    nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_v2),                 &
     &    nod_fld%d_fld(1,i_r) )
!
      end subroutine cal_phys_dot_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_cross_product(i_v1, i_v2, i_r, nod_fld)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_cross_prod_no_coef_smp(nod_fld%n_point,                  &
     &    nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_v2),                 &
     &    nod_fld%d_fld(1,i_r) )
!
      end subroutine cal_phys_cross_product
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_phys_dot_prod_w_coef                               &
     &         (i_v1, i_v2, i_r, coef, nod_fld)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_dot_prod_w_coef_smp(nod_fld%n_point, coef,               &
     &    nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_v2),                 &
     &    nod_fld%d_fld(1,i_r) )
!
      end subroutine cal_phys_dot_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_cross_prod_w_coef                             &
     &         (i_v1, i_v2, i_r, coef, nod_fld)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_cross_prod_w_coef_smp(nod_fld%n_point, coef,             &
     &    nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_v2),                 &
     &    nod_fld%d_fld(1,i_r) )
!
      end subroutine cal_phys_cross_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_tri_product_4_scalar                               &
     &         (i_v1, i_v2, i_v3, i_r, coef, nod_fld)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r
      integer (kind = kint), intent(in) :: i_v1, i_v2, i_v3
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_tri_product_w_coef_smp(nod_fld%n_point, coef,            &
     &    nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_v2),                 &
     &    nod_fld%d_fld(1,i_v3), nod_fld%d_fld(1,i_r) )
!
      end subroutine cal_tri_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_scalar_product_vector                         &
     &          (i_v1, i_s1, i_r, nod_fld)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v1
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_vec_scalar_prod_no_coef_smp(nod_fld%n_point,             &
     &    nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_s1),                 &
     &    nod_fld%d_fld(1,i_r) )
!
      end subroutine cal_phys_scalar_product_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_sym_matvec(i_t1, i_v2, i_r, nod_fld)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_t1, i_v2
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_tensor_vec_prod_no_coef_smp(nod_fld%n_point,             &
     &    nod_fld%d_fld(1,i_t1), nod_fld%d_fld(1,i_v2),                 &
     &    nod_fld%d_fld(1,i_r))
!
      end subroutine cal_phys_sym_matvec
!
!-----------------------------------------------------------------------
!
      subroutine prod_phys_scalar_mag_vector(i_s1, i_v2, i_r, nod_fld)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_scalar_mag_vector_prod_smp(nod_fld%n_point,              &
     &    nod_fld%d_fld(1,i_s1), nod_fld%d_fld(1,i_v2),                 &
     &    nod_fld%d_fld(1,i_r))
!
      end subroutine prod_phys_scalar_mag_vector
!
!-----------------------------------------------------------------------
!
      subroutine phys_vec_scalar_prod_w_coef                            &
     &         (i_v1, i_s1, i_r, coef, nod_fld)
!
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v1
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_vec_scalar_prod_w_coef_smp(nod_fld%n_point, coef,        &
     &    nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_s1),                 &
     &    nod_fld%d_fld(1,i_r) )
!
      end subroutine phys_vec_scalar_prod_w_coef
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_scalar(i_v1, i_r, const, nod_fld)
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_coef_prod_scalar_smp(nod_fld%n_point,                    &
     &    const, nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_r) )
!
      end subroutine multi_by_const_nod_scalar
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_vector(i_v1, i_r, const, nod_fld)
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_coef_prod_vect_smp(nod_fld%n_point,                      &
     &    const, nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_r) )
!
      end subroutine multi_by_const_nod_vector
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_tensor(i_v1, i_r, const, nod_fld)
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_coef_prod_tensor_smp(nod_fld%n_point,                    &
     &    const, nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_r) )
!
      end subroutine multi_by_const_nod_tensor
!
!-----------------------------------------------------------------------
!
      end module products_nodal_fields_smp
