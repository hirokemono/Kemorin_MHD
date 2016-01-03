!
!      module filter_all_fields
!
!      Written by H. Matsui on July, 2006
!
!     subroutine filtering_all_fields
!
      module filter_all_fields
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine filtering_all_fields
!
      use calypso_mpi
      use m_nod_comm_table
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
!
      use cal_filtering_scalars
      use cal_filtering_vectors
      use cal_filtering_tensors
!
      integer(kind = kint) :: i, j
!
!
      j = 1
      do i = 1, nod_fld1%num_phys_viz
        if ( nod_fld1%num_component(i) .eq. n_scalar) then
          if (my_rank.eq.0) write(*,*)'filtering scalar field: ',       &
     &      trim(nod_fld1%phys_name(i))
         call cal_filtered_scalar(nod_comm, node1, j, j, nod_fld1)
        else if ( nod_fld1%num_component(i) .eq. n_vector) then
          if (my_rank.eq.0) write(*,*)'filtering vector field: ',       &
     &      trim(nod_fld1%phys_name(i))
         call cal_filtered_vector(nod_comm, node1, j, j, nod_fld1)
        else if ( nod_fld1%num_component(i) .eq. n_sym_tensor) then
          if (my_rank.eq.0) write(*,*)'filtering tensor field: ',       &
     &      trim(nod_fld1%phys_name(i))
         call cal_filtered_sym_tensor(nod_comm, node1, j, j, nod_fld1)
        end if
        j = j + nod_fld1%num_component(i)
      end do
!
      end subroutine filtering_all_fields
!
! -----------------------------------------------------------------------
!
      end module filter_all_fields
