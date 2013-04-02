!expand_next_nod_id_w_hang.f90
!      module expand_next_nod_id_w_hang
!
!> @brief Neighbouring node list for each node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine const_next_nod_id_w_hang
!      subroutine overwrite_next_nod_by_hanged
!
      module expand_next_nod_id_w_hang
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_next_node_id_4_node
      use m_next_nod_id_nod_w_hang
!
      implicit none
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_next_nod_id_w_hang
!
      use m_hanging_mesh_data
      use cal_minmax_and_stacks
      use set_next_node_w_hanging
!
!
      call allocate_num_next_nod_w_hang(numnod)
      call allocate_iflag_nod_hang(numnod)
!
      call count_next_node_w_hanging(numnod, iflag_hang_nod,            &
     &          nnod_hang4, inod_hang4, nnod_hang2, inod_hang2,         &
     &          ntot_next_nod_4_node, inod_next_stack_4_node,           &
     &          inod_next_4_node, nnod_next_node_hanged)
!
      call s_cal_minmax_and_stacks(numnod, nnod_next_node_hanged,       &
     &    izero, istack_next_node_hanged, ntot_next_node_hanged,        &
     &    nmax_next_node_hanged, nmin_next_node_hanged)
!
      call allocate_inod_next_nod_w_hang
!
      call s_set_next_node_w_hanging(numnod, iflag_hang_nod,            &
     &          nnod_hang4, inod_hang4, nnod_hang2, inod_hang2,         &
     &          ntot_next_nod_4_node, inod_next_stack_4_node,           &
     &          inod_next_4_node, ntot_next_node_hanged,                &
     &          istack_next_node_hanged, inod_next_node_hanged)
!
      call deallocate_iflag_nod_hang
!
      end subroutine const_next_nod_id_w_hang
!
!-----------------------------------------------------------------------
!
      subroutine overwrite_next_nod_by_hanged
!
!
      call deallocate_inod_next_node
!
      ntot_next_nod_4_node = ntot_next_node_hanged
      nmin_next_nod_4_node = nmin_next_node_hanged
      nmax_next_nod_4_node = nmax_next_node_hanged
!
      call allocate_num_next_node(numnod)
      call allocate_inod_next_node
!
      nnod_next_4_node(1:numnod)                                        &
     &      = nnod_next_node_hanged(1:numnod)
      inod_next_stack_4_node(0:numnod)                                  &
     &      = istack_next_node_hanged(0:numnod)
!
      inod_next_4_node(1:ntot_next_nod_4_node)                          &
     &      = inod_next_node_hanged(1:ntot_next_nod_4_node) 
      iweight_next_4_node(1:ntot_next_nod_4_node)                       &
     &      = iweight_next_hanged(1:ntot_next_nod_4_node) 
!
      end subroutine overwrite_next_nod_by_hanged
!
!-----------------------------------------------------------------------
!
      end module expand_next_nod_id_w_hang
