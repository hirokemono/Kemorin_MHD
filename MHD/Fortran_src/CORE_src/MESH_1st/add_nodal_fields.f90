!
!     module add_nodal_fields
!
!      Written by H. Matsui
!
!       subroutine add_2_nod_scalars(i_r, i_v1, i_v2)
!       subroutine add_2_nod_vectors(i_r, i_v1, i_v2)
!       subroutine add_2_nod_tensors(i_r, i_v1, i_v2)
!
!         d_nod(inod,i_r) =  d_nod(inod,i_v1) + d_nod(inod,i_v2)
!        i_r: result field ID
!        i_v1, i_v2: source field IDs
!
      module add_nodal_fields
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
       subroutine add_2_nod_scalars(i_r, i_v1, i_v2)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_node_phys_data
      use cal_add_smp
!
       integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call add_scalars_smp(np_smp, numnod, inod_smp_stack,              &
     &    d_nod(1,i_v1), d_nod(1,i_v2), d_nod(1,i_r))
!
       end subroutine add_2_nod_scalars
!
!-----------------------------------------------------------------------
!
       subroutine add_2_nod_vectors(i_r, i_v1, i_v2)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_node_phys_data
      use cal_add_smp
!
       integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call add_vectors_smp(np_smp, numnod, inod_smp_stack,              &
     &    d_nod(1,i_v1), d_nod(1,i_v2), d_nod(1,i_r))
!
       end subroutine add_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine add_2_nod_tensors(i_r, i_v1, i_v2)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_node_phys_data
      use cal_add_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call add_tensors_smp(np_smp, numnod, inod_smp_stack,              &
     &    d_nod(1,i_v1), d_nod(1,i_v2), d_nod(1,i_r))
!
      end subroutine add_2_nod_tensors
!
!-----------------------------------------------------------------------
!
      end module add_nodal_fields
