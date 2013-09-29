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
      do i = 1, num_nod_phys_vis
        if ( num_nod_component(i) .eq.1 ) then
          if (my_rank.eq.0) write(*,*)'filtering scalar field: ',       &
     &      trim(phys_nod_name(i))
         call cal_filtered_scalar(j, j)
        else if ( num_nod_component(i) .eq.3 ) then
          if (my_rank.eq.0) write(*,*)'filtering vector field: ',       &
     &      trim(phys_nod_name(i))
         call cal_filtered_vector(j, j)
        else if ( num_nod_component(i) .eq.6 ) then
          if (my_rank.eq.0) write(*,*)'filtering tensor field: ',       &
     &      trim(phys_nod_name(i))
         call cal_filtered_sym_tensor(j, j)
        end if
        j = j + num_nod_component(i)
      end do
!
      end subroutine filtering_all_fields
!
! -----------------------------------------------------------------------
!
      end module filter_all_fields
