!
!     module subtract_nodal_fields
!
!      Written by H. Matsui
!
!       subroutine subtract_2_nod_scalars(i_v1, i_v2, i_r)
!       subroutine subtract_2_nod_vectors(i_v1, i_v2, i_r)
!       subroutine subtract_2_nod_tensors(i_v1, i_v2, i_r)
!
!         d_nod(inod,i_r) =  d_nod(inod,i_v1) - d_nod(inod,i_v2)
!        i_r: result field ID
!        i_v1, i_v2: source field IDs
!
      module subtract_nodal_fields
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine subtract_2_nod_scalars(i_v1, i_v2, i_r)
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use subtract_nodal_field_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call subtract_scalar_array_smp                                    &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    num_tot_nod_phys, d_nod, i_v1, i_v2, i_r)
!
      end subroutine subtract_2_nod_scalars
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_vectors(i_v1, i_v2, i_r)
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use subtract_nodal_field_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call subtract_vector_array_smp                                    &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    num_tot_nod_phys, d_nod, i_v1, i_v2, i_r)
!
      end subroutine subtract_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_tensors(i_v1, i_v2, i_r)
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use subtract_nodal_field_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call subtract_tensor_array_smp                                    &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    num_tot_nod_phys, d_nod, i_v1, i_v2, i_r)
!
      end subroutine subtract_2_nod_tensors
!
!-----------------------------------------------------------------------
!
      end module subtract_nodal_fields
