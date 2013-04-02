!products_nodal_field_type.f90
!     module products_nodal_field_type
!
!      Written by H. Matsui
!
!      subroutine cal_phys_type_product_4_scalar(node, nod_fld,         &
!     &          i_r, i_s1, i_s2)
!                d_nod(,i_r) = d_nod(,i_v1) * d_nod(,i_v2)
!      subroutine cal_phys_type_dot_product(node, nod_fld,              &
!     &          i_r, i_v1, i_v2)
!                d_nod(,i_r) = d_nod(,i_v1) \times d_nod(,i_v2)
!      subroutine cal_phys_type_vector_product(node, nod_fld,           &
!     &          i_r, i_v1, i_v2)
!                d_nod(,i_r) = d_nod(,i_v1) \cdot d_nod(,i_v2)
!      subroutine cal_tri_product_type_w_coef(node, nod_fld,            &
!     &          i_solution, i_comp1, i_comp2, i_comp3, coef )
!
!      subroutine cal_phys_type_scalar_prod_vect(node, nod_fld,         &
!     &           i_r, i_s1, i_v2)
!                d_nod(,i_r  ) = d_nod(,i_s1) * d_nod(,i_v2  )
!                d_nod(,i_r+1) = d_nod(,i_s1) * d_nod(,i_v2+1)
!                d_nod(,i_r+2) = d_nod(,i_s1) * d_nod(,i_v2+2)
!      subroutine cal_phys_type_sym_matvec(node, nod_fld,               &
!     &          i_r, i_t1, i_v2)
!        d_nod(,i_r  ) =  d_nod(,i_t1  )*d_nod(,i_v2  )      &
!     &                 + d_nod(,i_t1+1)*d_nod(,i_v2+1)      &
!     &                 + d_nod(,i_t1+2)*d_nod(,i_v2+2)
!        d_nod(,i_r+1) =  d_nod(,i_t1+1)*d_nod(,i_v2  )      &
!     &                 + d_nod(,i_t1+3)*d_nod(,i_v2+1)      &
!     &                 + d_nod(,i_t1+4)*d_nod(,i_v2+2)
!        d_nod(,i_r+2) =  d_nod(,i_t1+2)*d_nod(,i_v2  )      &
!     &                 + d_nod(,i_t1+4)*d_nod(,i_v2+1)      &
!     &                 + d_nod(,i_t1+5)*d_nod(,i_v2+2)
!      subroutine prod_phys_type_scalar_mag_vec(node, nod_fld,          &
!     &          i_r, i_s1, i_v2)
!
!        type(node_data), intent(in) :: node
!        type(phys_data), intent(inout) :: nod_fld
!
      module products_nodal_field_type
!
      use m_precision
!
      implicit none
!
      private :: cal_nod_phys_product_4_scalar
      private :: cal_nod_phys_dot_product, cal_nod_phys_vector_product
      private :: cal_nod_tri_product_w_coef
      private :: cal_nod_phys_scalar_prod_vect
      private :: cal_nod_phys_sym_matvec
      private :: prod_nod_phys_scalar_mag_vect
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_type_product_4_scalar(node, nod_fld,          &
     &          i_r, i_s1, i_s2)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer (kind = kint), intent(in) :: i_r, i_s1, i_s2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_nod_phys_product_4_scalar(np_smp, node%numnod,           &
     &    node%istack_nod_smp, nod_fld%ntot_phys, nod_fld%d_fld,        &
     &    i_r, i_s1, i_s2)
!
      end subroutine cal_phys_type_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_type_dot_product(node, nod_fld,               &
     &          i_r, i_v1, i_v2)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_nod_phys_dot_product(np_smp, node%numnod,                &
     &    node%istack_nod_smp, nod_fld%ntot_phys, nod_fld%d_fld,        &
     &    i_r, i_v1, i_v2)
!
      end subroutine cal_phys_type_dot_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_type_vector_product(node, nod_fld,            &
     &          i_r, i_v1, i_v2)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_nod_phys_vector_product(np_smp, node%numnod,             &
     &    node%istack_nod_smp, nod_fld%ntot_phys, nod_fld%d_fld,        &
     &    i_r, i_v1, i_v2)
!
      end subroutine cal_phys_type_vector_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_tri_product_type_w_coef(node, nod_fld,             &
     &          i_solution, i_comp1, i_comp2, i_comp3, coef )
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_solution
      integer (kind = kint), intent(in) :: i_comp1, i_comp2, i_comp3
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_nod_tri_product_w_coef(np_smp, node%numnod,              &
     &    node%istack_nod_smp, nod_fld%ntot_phys, nod_fld%d_fld,        &
     &    i_solution, i_comp1, i_comp2, i_comp3, coef )
!
      end subroutine cal_tri_product_type_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_type_scalar_prod_vect(node, nod_fld,          &
     &           i_r, i_s1, i_v2)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_nod_phys_scalar_prod_vect(np_smp, node%numnod,           &
     &    node%istack_nod_smp, nod_fld%ntot_phys, nod_fld%d_fld,        &
     &    i_r, i_s1, i_v2)
!
      end subroutine cal_phys_type_scalar_prod_vect
!
!-----------------------------------------------------------------------
!
      subroutine cal_phys_type_sym_matvec(node, nod_fld,                &
     &          i_r, i_t1, i_v2)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer(kind = kint), intent(in) :: i_r, i_t1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_nod_phys_sym_matvec(np_smp, node%numnod,                 &
     &    node%istack_nod_smp, nod_fld%ntot_phys, nod_fld%d_fld,        &
     &    i_r, i_t1, i_v2)
!
      end subroutine cal_phys_type_sym_matvec
!
!-----------------------------------------------------------------------
!
      subroutine prod_phys_type_scalar_mag_vec(node, nod_fld,           &
     &          i_r, i_s1, i_v2)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call prod_nod_phys_scalar_mag_vect(np_smp, node%numnod,           &
     &    node%istack_nod_smp, nod_fld%ntot_phys, nod_fld%d_fld,        &
     &    i_r, i_s1, i_v2)
!
      end subroutine prod_phys_type_scalar_mag_vec
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_nod_phys_product_4_scalar(np_smp, nnod,            &
     &          inod_smp_stack, ntot_comp, d_fld, i_r, i_s1, i_s2)
!
      use cal_products_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: i_r, i_s1, i_s2
!
      real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
!
!
!$omp parallel
      call cal_scalar_prod_no_coef_smp(np_smp, nnod, inod_smp_stack,  &
     &    d_fld(1,i_s1), d_fld(1,i_s2),  d_fld(1,i_r) )
!$omp end parallel
!
      end subroutine cal_nod_phys_product_4_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_nod_phys_dot_product(np_smp, nnod, inod_smp_stack, &
     &          ntot_comp, d_fld, i_r, i_v1, i_v2)
!
      use cal_products_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: i_r, i_v1, i_v2
!
      real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
!
!
!$omp parallel
      call cal_dot_prod_no_coef_smp(np_smp, nnod, inod_smp_stack,     &
     &    d_fld(1,i_v1), d_fld(1,i_v2),  d_fld(1,i_r) )
!$omp end parallel
!
      end subroutine cal_nod_phys_dot_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_nod_phys_vector_product(np_smp, nnod,              &
     &          inod_smp_stack, ntot_comp, d_fld, i_r, i_v1, i_v2)
!
      use cal_products_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
      real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
!
!
!$omp parallel
      call cal_vect_prod_no_coef_smp(np_smp, nnod, inod_smp_stack,    &
     &    d_fld(1,i_v1), d_fld(1,i_v2),  d_fld(1,i_r) )
!$omp end parallel
!
      end subroutine cal_nod_phys_vector_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_nod_tri_product_w_coef(np_smp, nnod,               &
     &          inod_smp_stack, ntot_comp, d_fld, i_solution,          &
     &          i_comp1, i_comp2, i_comp3, coef )
!
      use cal_products_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind = kreal), intent(in) :: coef
      integer (kind = kint), intent(in) :: i_solution
      integer (kind = kint), intent(in) :: i_comp1, i_comp2, i_comp3
!
      real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
!
!
!$omp parallel
      call cal_tri_product_w_coef_smp(np_smp, nnod, inod_smp_stack,     &
     &    coef, d_fld(1,i_comp1), d_fld(1,i_comp2),                   &
     &    d_fld(1,i_comp3), d_fld(1,i_solution) )
!$omp end parallel
!
      end subroutine cal_nod_tri_product_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_nod_phys_scalar_prod_vect(np_smp, nnod,            &
     &           inod_smp_stack, ntot_comp, d_fld, i_r, i_s1, i_v2)
!
      use cal_products_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
!
!
!$omp parallel
      call cal_vec_scalar_prod_no_coef_smp(np_smp, nnod,                &
     &    inod_smp_stack, d_fld(1,i_v2), d_fld(1,i_s1), d_fld(1,i_r) )
!$omp end parallel
!
      end subroutine cal_nod_phys_scalar_prod_vect
!
!-----------------------------------------------------------------------
!
      subroutine cal_nod_phys_sym_matvec(np_smp, nnod, inod_smp_stack,  &
     &          ntot_comp, d_fld, i_r, i_t1, i_v2)
!
      use cal_products_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) :: i_r, i_t1, i_v2
!
!
!$omp parallel
      call cal_tensor_vec_prod_no_coef_smp(np_smp, nnod,                &
     &    inod_smp_stack, d_fld(1,i_t1), d_fld(1,i_v2), d_fld(1,i_r))
!$omp end parallel
!
      end subroutine cal_nod_phys_sym_matvec
!
!-----------------------------------------------------------------------
!
      subroutine prod_nod_phys_scalar_mag_vect(np_smp, nnod,            &
     &          inod_smp_stack, ntot_comp, d_fld, i_r, i_s1, i_v2)
!
      use cal_products_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(inout) :: d_fld(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) :: i_r, i_s1, i_v2
!
!$omp parallel
      call cal_scalar_mag_vector_prod_smp(np_smp, nnod, inod_smp_stack, &
     &    d_fld(1,i_s1), d_fld(1,i_v2), d_fld(1,i_r))
!$omp end parallel
!
      end subroutine prod_nod_phys_scalar_mag_vect
!
!-----------------------------------------------------------------------
!
      end module products_nodal_field_type
