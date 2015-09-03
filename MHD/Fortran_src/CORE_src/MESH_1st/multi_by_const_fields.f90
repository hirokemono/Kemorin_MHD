!
!     module multi_by_const_fields
!
!      Written by H. Matsui
!
!       subroutine multi_by_const_nod_scalar(const, i_v1, i_r)
!       subroutine multi_by_const_nod_vector(const, i_v1, i_r)
!       subroutine multi_by_const_nod_tensor(const, i_v1, i_r)
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
      subroutine multi_by_const_nod_scalar(const, i_v1, i_r)
!
      use multi_by_const_fields_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
      call multi_by_const_nod_phys1                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    num_tot_nod_phys, d_nod, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_scalar
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_vector(const, i_v1, i_r)
!
      use multi_by_const_fields_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
      call multi_by_const_nod_phys3                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    num_tot_nod_phys, d_nod, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_vector
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_tensor(const, i_v1, i_r)
!
      use multi_by_const_fields_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
      call multi_by_const_nod_phys6                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    num_tot_nod_phys, d_nod, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_tensor
!
!-----------------------------------------------------------------------
!
      end module multi_by_const_fields
