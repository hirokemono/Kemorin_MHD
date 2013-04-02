!products_nodal_fields.f90
!     module products_nodal_fields
!
!      Written by H. Matsui
!
!      subroutine cal_phys_product_4_scalar(i_r, i_s1, i_s2)
!                d_nod(,i_r) = d_nod(,i_v1) * d_nod(,i_v2)
!      subroutine cal_phys_vector_product(i_r, i_v1, i_v2)
!                d_nod(,i_r) = d_nod(,i_v1) \times d_nod(,i_v2)
!      subroutine cal_phys_dot_product(i_r, i_v1, i_v2)
!                d_nod(,i_r) = d_nod(,i_v1) \cdot d_nod(,i_v2)
!      subroutine cal_tri_product_4_scalar(i_solution, i_comp1,         &
!     &          i_comp2, i_comp3, coef )
!
!      subroutine cal_phys_scalar_product_vector(i_r, i_s1, i_v2)
!                d_nod(,i_r  ) = d_nod(,i_s1) * d_nod(,i_v2  )
!                d_nod(,i_r+1) = d_nod(,i_s1) * d_nod(,i_v2+1)
!                d_nod(,i_r+2) = d_nod(,i_s1) * d_nod(,i_v2+2)
!      subroutine cal_phys_sym_matvec(i_r, i_t1, i_v2)
!        d_nod(,i_r  ) =  d_nod(,i_t1  )*d_nod(,i_v2  )      &
!     &                 + d_nod(,i_t1+1)*d_nod(,i_v2+1)      &
!     &                 + d_nod(,i_t1+2)*d_nod(,i_v2+2)
!        d_nod(,i_r+1) =  d_nod(,i_t1+1)*d_nod(,i_v2  )      &
!     &                 + d_nod(,i_t1+3)*d_nod(,i_v2+1)      &
!     &                 + d_nod(,i_t1+4)*d_nod(,i_v2+2)
!        d_nod(,i_r+2) =  d_nod(,i_t1+2)*d_nod(,i_v2  )      &
!     &                 + d_nod(,i_t1+4)*d_nod(,i_v2+1)      &
!     &                 + d_nod(,i_t1+5)*d_nod(,i_v2+2)
!       subroutine prod_phys_scalar_mag_vector(i_r, i_s1, i_v2)
!
      module products_nodal_fields
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_product_4_scalar(i_r, i_s1, i_s2)
!
      use m_geometry_parameter
      use m_node_phys_data
      use cal_products_smp
!
      integer (kind = kint), intent(in) :: i_r, i_s1, i_s2
!
!
!$omp parallel
      call cal_scalar_prod_no_coef_smp(np_smp, numnod, inod_smp_stack,  &
     &    d_nod(1,i_s1), d_nod(1,i_s2),  d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine cal_phys_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_dot_product(i_r, i_v1, i_v2)
!
      use m_geometry_parameter
      use m_node_phys_data
      use cal_products_smp
!
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
!$omp parallel
      call cal_dot_prod_no_coef_smp(np_smp, numnod, inod_smp_stack,     &
     &    d_nod(1,i_v1), d_nod(1,i_v2),  d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine cal_phys_dot_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_vector_product(i_r, i_v1, i_v2)
!
      use m_geometry_parameter
      use m_node_phys_data
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
!$omp parallel
      call cal_vect_prod_no_coef_smp(np_smp, numnod, inod_smp_stack,    &
     &    d_nod(1,i_v1), d_nod(1,i_v2),  d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine cal_phys_vector_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_tri_product_4_scalar(i_solution, i_comp1, i_comp2, &
     &          i_comp3, coef )
!
      use m_geometry_parameter
      use m_node_phys_data
      use cal_products_smp
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_solution
      integer (kind = kint), intent(in) :: i_comp1, i_comp2, i_comp3
!
!
!$omp parallel
      call cal_tri_product_w_coef_smp(np_smp, numnod, inod_smp_stack,   &
     &    coef, d_nod(1,i_comp1), d_nod(1,i_comp2), d_nod(1,i_comp3),   &
     &    d_nod(1,i_solution) )
!$omp end parallel
!
      end subroutine cal_tri_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_scalar_product_vector(i_r, i_s1, i_v2)
!
      use m_geometry_parameter
      use m_node_phys_data
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
!
!
!$omp parallel
      call cal_vec_scalar_prod_no_coef_smp(np_smp, numnod,              &
     &    inod_smp_stack, d_nod(1,i_v2), d_nod(1,i_s1), d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine cal_phys_scalar_product_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_sym_matvec(i_r, i_t1, i_v2)
!
      use m_geometry_parameter
      use m_node_phys_data
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_t1, i_v2
!
!
!$omp parallel
      call cal_tensor_vec_prod_no_coef_smp(np_smp, numnod,              &
     &    inod_smp_stack, d_nod(1,i_t1), d_nod(1,i_v2), d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine cal_phys_sym_matvec
!
!-----------------------------------------------------------------------
!
      subroutine prod_phys_scalar_mag_vector(i_r, i_s1, i_v2)
!
      use m_geometry_parameter
      use m_node_phys_data
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
!
!$omp parallel
      call cal_scalar_mag_vector_prod_smp(np_smp, numnod,               &
     &    inod_smp_stack, d_nod(1,i_s1), d_nod(1,i_v2), d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine prod_phys_scalar_mag_vector
!
!-----------------------------------------------------------------------
!
      end module products_nodal_fields
