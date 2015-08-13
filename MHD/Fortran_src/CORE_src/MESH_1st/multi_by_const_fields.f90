!
!     module multi_by_const_fields
!
!      Written by H. Matsui
!
!       subroutine multi_by_const_nod_scalar(i_r, i_v1, const)
!       subroutine multi_by_const_nod_vector(i_r, i_v1, const)
!       subroutine multi_by_const_nod_tensor(i_r, i_v1, const)
!
!         d_nod(inod,i_r) =  const * d_nod(inod,i_v1)
!        i_r: result field ID
!        i_v1: source field IDs
!
      module multi_by_const_fields
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
      subroutine multi_by_const_nod_scalar(i_r, i_v1, const)
!
      use m_geometry_data
      use m_node_phys_data
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
!$omp parallel
      call cal_coef_prod_scalar_smp                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    const, d_nod(1,i_v1), d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine multi_by_const_nod_scalar
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_vector(i_r, i_v1, const)
!
      use m_geometry_data
      use m_node_phys_data
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
!$omp parallel
      call cal_coef_prod_vect_smp                                       &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    const, d_nod(1,i_v1), d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine multi_by_const_nod_vector
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_tensor(i_r, i_v1, const)
!
      use m_geometry_data
      use m_node_phys_data
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
!$omp parallel
      call cal_coef_prod_tensor_smp                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    const, d_nod(1,i_v1), d_nod(1,i_r) )
!$omp end parallel
!
      end subroutine multi_by_const_nod_tensor
!
!-----------------------------------------------------------------------
!
      end module multi_by_const_fields
