!
!      module filter_all_fields
!
!      Written by H. Matsui on July, 2006
!
!!     subroutine filtering_all_fields(nod_comm, node, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!
      module filter_all_fields
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine filtering_all_fields(nod_comm, node, nod_fld)
!
      use calypso_mpi
      use m_phys_constants
!
      use cal_filtering_scalars
      use cal_filtering_vectors
      use cal_filtering_tensors
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, j
!
!
      j = 1
      do i = 1, nod_fld%num_phys_viz
        if ( nod_fld%num_component(i) .eq. n_scalar) then
          if (my_rank.eq.0) write(*,*)'filtering scalar field: ',       &
     &      trim(nod_fld%phys_name(i))
         call cal_filtered_scalar(nod_comm, node, j, j, nod_fld)
        else if ( nod_fld%num_component(i) .eq. n_vector) then
          if (my_rank.eq.0) write(*,*)'filtering vector field: ',       &
     &      trim(nod_fld%phys_name(i))
         call cal_filtered_vector(nod_comm, node, j, j, nod_fld)
        else if ( nod_fld%num_component(i) .eq. n_sym_tensor) then
          if (my_rank.eq.0) write(*,*)'filtering tensor field: ',       &
     &      trim(nod_fld%phys_name(i))
         call cal_filtered_sym_tensor(nod_comm, node, j, j, nod_fld)
        end if
        j = j + nod_fld%num_component(i)
      end do
!
      end subroutine filtering_all_fields
!
! -----------------------------------------------------------------------
!
      end module filter_all_fields
