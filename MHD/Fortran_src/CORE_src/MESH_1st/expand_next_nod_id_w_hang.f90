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
      use m_geometry_data
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
      call allocate_num_next_nod_w_hang(node1%numnod)
      call allocate_iflag_nod_hang(node1%numnod)
!
      call count_next_node_w_hanging                                    &
     &   (node1%numnod, hang1%nod_hang%iflag_hang,                      &
     &    hang1%nod_hang%n_sf, hang1%nod_hang%id_sf,                    &
     &    hang1%nod_hang%n_ed, hang1%nod_hang%id_ed,                    &
     &    neib_nod1%ntot, neib_nod1%istack_next, neib_nod1%inod_next,   &
     &    nnod_next_node_hanged)
!
      call s_cal_minmax_and_stacks(node1%numnod, nnod_next_node_hanged, &
     &    izero, istack_next_node_hanged, ntot_next_node_hanged,        &
     &    nmax_next_node_hanged, nmin_next_node_hanged)
!
      call allocate_inod_next_nod_w_hang
!
      call s_set_next_node_w_hanging                                    &
     &   (node1%numnod, hang1%nod_hang%iflag_hang,                      &
     &    hang1%nod_hang%n_sf, hang1%nod_hang%id_sf,                    &
     &    hang1%nod_hang%n_ed, hang1%nod_hang%id_ed,                    &
     &    neib_nod1%ntot, neib_nod1%istack_next, neib_nod1%inod_next,   &
     &    ntot_next_node_hanged, istack_next_node_hanged,               &
     &    inod_next_node_hanged)
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
      call dealloc_inod_next_node(neib_nod1)
!
      neib_nod1%ntot = ntot_next_node_hanged
      neib_nod1%nmin = nmin_next_node_hanged
      neib_nod1%nmax = nmax_next_node_hanged
!
      call alloc_num_next_node(node1%numnod, neib_nod1)
      call alloc_inod_next_node(neib_nod1)
!
      neib_nod1%nnod_next(1:node1%numnod)                               &
     &      = nnod_next_node_hanged(1:node1%numnod)
      neib_nod1%istack_next(0:node1%numnod)                             &
     &      = istack_next_node_hanged(0:node1%numnod)
!
      neib_nod1%inod_next(1:neib_nod1%ntot)                             &
     &      = inod_next_node_hanged(1:neib_nod1%ntot) 
      neib_nod1%iweight_next(1:neib_nod1%ntot)                          &
     &      = iweight_next_hanged(1:neib_nod1%ntot) 
!
      end subroutine overwrite_next_nod_by_hanged
!
!-----------------------------------------------------------------------
!
      end module expand_next_nod_id_w_hang
