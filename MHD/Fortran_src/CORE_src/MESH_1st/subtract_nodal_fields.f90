!
!     module subtract_nodal_fields
!
!      Written by H. Matsui
!
!       subroutine subtract_2_nod_scalars(i_r, i_v1, i_v2)
!       subroutine subtract_2_nod_vectors(i_r, i_v1, i_v2)
!       subroutine subtract_2_nod_tensors(i_r, i_v1, i_v2)
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
       subroutine subtract_2_nod_scalars(i_r, i_v1, i_v2)
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use cal_subtract_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
!$omp parallel
      call subtract_scalars_smp(np_smp, node1%numnod, inod_smp_stack,   &
     &    d_nod(1,i_v1), d_nod(1,i_v2), d_nod(1,i_r))
!$omp end parallel
!
      end subroutine subtract_2_nod_scalars
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_vectors(i_r, i_v1, i_v2)
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use cal_subtract_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
!$omp parallel
      call subtract_vectors_smp(np_smp, node1%numnod, inod_smp_stack,   &
     &    d_nod(1,i_v1), d_nod(1,i_v2), d_nod(1,i_r))
!$omp end parallel
!
      end subroutine subtract_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_tensors(i_r, i_v1, i_v2)
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use cal_subtract_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
!$omp parallel
      call subtract_tensors_smp(np_smp, node1%numnod, inod_smp_stack,   &
     &    d_nod(1,i_v1), d_nod(1,i_v2), d_nod(1,i_r))
!$omp end parallel
!
      end subroutine subtract_2_nod_tensors
!
!-----------------------------------------------------------------------
!
      end module subtract_nodal_fields
