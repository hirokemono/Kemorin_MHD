!
!      module filter_all_fields
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine filtering_all_fields(filter_param, nod_comm, node,   &
!!     &          filtering, wk_filter, nod_fld, v_sol)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!
      module filter_all_fields
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_filtering_data
      use t_vector_for_solver
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine filtering_all_fields(filter_param, nod_comm, node,     &
     &          filtering, wk_filter, nod_fld, v_sol)
!
      use calypso_mpi
      use m_phys_constants
!
      use cal_filtering_scalars
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i, j
!
!
      j = 1
      do i = 1, nod_fld%num_phys_viz
        if ( nod_fld%num_component(i) .eq. n_scalar) then
          if (my_rank.eq.0) write(*,*)'filtering scalar field: ',       &
     &      trim(nod_fld%phys_name(i))
         call cal_filtered_scalar_whole(filter_param, nod_comm,         &
     &       node, filtering, j, j, wk_filter, nod_fld, v_sol)
        else if ( nod_fld%num_component(i) .eq. n_vector) then
          if (my_rank.eq.0) write(*,*)'filtering vector field: ',       &
     &      trim(nod_fld%phys_name(i))
         call cal_filtered_vector_whole(filter_param, nod_comm,         &
     &       node, filtering, j, j, wk_filter, nod_fld, v_sol)
        else if ( nod_fld%num_component(i) .eq. n_sym_tensor) then
          if (my_rank.eq.0) write(*,*)'filtering tensor field: ',       &
     &      trim(nod_fld%phys_name(i))
         call cal_filtered_sym_tensor_whole(filter_param, nod_comm,     &
     &       node, filtering, j, j, wk_filter, nod_fld, v_sol)
        end if
        j = j + nod_fld%num_component(i)
      end do
!
      end subroutine filtering_all_fields
!
! -----------------------------------------------------------------------
!
      end module filter_all_fields
