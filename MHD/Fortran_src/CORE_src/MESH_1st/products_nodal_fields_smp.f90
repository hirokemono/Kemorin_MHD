!>@file   products_nodal_fields_smp.f90
!!@brief  module products_nodal_fields_smp
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief subroutines to obatine products of two nodal fields
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_phys_product_4_scalar(i_s1, i_s2, i_r)
!!                d_nod(,i_r) = d_nod(,i_v1) * d_nod(,i_v2)
!!      subroutine cal_phys_dot_product(i_v1, i_v2, i_r)
!!                d_nod(,i_r) = d_nod(,i_v1) \cdot d_nod(,i_v2)
!!      subroutine cal_phys_cross_product(i_v1, i_v2, i_r)
!!                d_nod(,i_r) = d_nod(,i_v1) \times d_nod(,i_v2)
!!
!!      subroutine cal_phys_dot_prod_w_coef(coef, i_v1, i_v2, i_r)
!!                d_nod(,i_r) = coef * d_nod(,i_v1) \cdot d_nod(,i_v2)
!!      subroutine cal_phys_cross_prod_w_coef(coef, i_v1, i_v2, i_r)
!!                d_nod(,i_r) = coef * d_nod(,i_v1) \times d_nod(,i_v2)
!!      subroutine cal_tri_product_4_scalar(coef, i_v1, i_v2, i_v3, i_r)
!!
!!      subroutine cal_phys_scalar_product_vector(i_v1, i_s1, i_r)
!!                d_nod(,i_r  ) = d_nod(,i_s1) * d_nod(,i_v1  )
!!                d_nod(,i_r+1) = d_nod(,i_s1) * d_nod(,i_v1+1)
!!                d_nod(,i_r+2) = d_nod(,i_s1) * d_nod(,i_v1+2)
!!      subroutine cal_phys_sym_matvec(i_t1, i_v2, i_r)
!!                d_nod(,i_r  ) =  d_nod(,i_t1  )*d_nod(,i_v2  )      &
!!     &                         + d_nod(,i_t1+1)*d_nod(,i_v2+1)      &
!!     &                         + d_nod(,i_t1+2)*d_nod(,i_v2+2)
!!                d_nod(,i_r+1) =  d_nod(,i_t1+1)*d_nod(,i_v2  )      &
!!     &                         + d_nod(,i_t1+3)*d_nod(,i_v2+1)      &
!!     &                         + d_nod(,i_t1+4)*d_nod(,i_v2+2)
!!                d_nod(,i_r+2) =  d_nod(,i_t1+2)*d_nod(,i_v2  )      &
!!     &                         + d_nod(,i_t1+4)*d_nod(,i_v2+1)      &
!!     &                         + d_nod(,i_t1+5)*d_nod(,i_v2+2)
!!       subroutine prod_phys_scalar_mag_vector(i_s1, i_v2, i_r)
!!
!!       subroutine phys_vec_scalar_prod_w_coef(coef, i_v1, i_s1, i_r)
!!                d_nod(,i_r  ) = coef * d_nod(,i_s1) * d_nod(,i_v1  )
!!                d_nod(,i_r+1) = coef * d_nod(,i_s1) * d_nod(,i_v1+1)
!!                d_nod(,i_r+2) = coef * d_nod(,i_s1) * d_nod(,i_v1+2)
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
!
      module products_nodal_fields_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_product_4_scalar(i_s1, i_s2, i_r)
!
      use products_nod_field_t_smp
!
      integer (kind = kint), intent(in) :: i_r, i_s1, i_s2
!
!
      call cal_nod_phys_product_4_scalar                                &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, i_s1, i_s2, i_r)
!
      end subroutine cal_phys_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_dot_product(i_v1, i_v2, i_r)
!
      use products_nod_field_t_smp
!
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_nod_phys_dot_product                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, i_v1, i_v2, i_r)
!
      end subroutine cal_phys_dot_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_cross_product(i_v1, i_v2, i_r)
!
      use products_nod_field_t_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_nod_phys_cross_product                                   &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, i_v1, i_v2, i_r)
!
      end subroutine cal_phys_cross_product
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_phys_dot_prod_w_coef(coef, i_v1, i_v2, i_r)
!
      use products_nod_field_t_smp
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_nod_phys_dot_prod_w_coef                                 &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, coef, i_v1, i_v2, i_r)
!
      end subroutine cal_phys_dot_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_cross_prod_w_coef(coef, i_v1, i_v2, i_r)
!
      use products_nod_field_t_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call cal_nod_phys_cross_prod_w_coef                               &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, coef, i_v1, i_v2, i_r)
!
      end subroutine cal_phys_cross_prod_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_tri_product_4_scalar(coef, i_v1, i_v2, i_v3, i_r)
!
      use products_nod_field_t_smp
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_r
      integer (kind = kint), intent(in) :: i_v1, i_v2, i_v3
!
!
      call cal_nod_tri_product_w_coef                                   &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, coef,                     &
     &    i_v1, i_v2, i_v3, i_r)
!
      end subroutine cal_tri_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_scalar_product_vector(i_v1, i_s1, i_r)
!
      use products_nod_field_t_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v1
!
!
      call cal_nod_phys_scalar_prod_vect                                &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, i_v1, i_s1, i_r)
!
      end subroutine cal_phys_scalar_product_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_sym_matvec(i_t1, i_v2, i_r)
!
      use products_nod_field_t_smp
!
      integer(kind = kint), intent(in) :: i_r, i_t1, i_v2
!
!
      call cal_nod_phys_sym_matvec                                      &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, i_t1, i_v2, i_r)
!
      end subroutine cal_phys_sym_matvec
!
!-----------------------------------------------------------------------
!
      subroutine prod_phys_scalar_mag_vector(i_s1, i_v2, i_r)
!
      use products_nod_field_t_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
!
      call prod_nod_phys_scalar_mag_vect                                &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, i_s1, i_v2, i_r)
!
      end subroutine prod_phys_scalar_mag_vector
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine phys_vec_scalar_prod_w_coef(coef, i_v1, i_s1, i_r)
!
      use products_nod_field_t_smp
!
      real (kind = kreal), intent(in) :: coef
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v1
!
!
      call nod_phys_vec_scalar_prod_w_c                                 &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, nod_fld1%d_fld, coef, i_v1, i_s1, i_r)
!
      end subroutine phys_vec_scalar_prod_w_coef
!
!-----------------------------------------------------------------------
!
      end module products_nodal_fields_smp
