!m_next_node_id_4_node.f90
!      module m_next_node_id_4_node
!
!> @brief Neighbouring node list for each node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine const_next_nod_id_4_node
!
      module m_next_node_id_4_node
!
      use m_precision
      use m_constants
      use t_next_node_ele_4_node
!
      implicit none
!
!>   Structure of neighbouring node list for each node
      type(next_nod_id_4_nod), save :: neib_nod1
!neib_nod1%istack_next
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_next_nod_id_4_node
!
      use m_machine_parameter
      use m_geometry_data
!
      use m_element_id_4_node
      use find_node_4_group
      use cal_minmax_and_stacks
!
!
      call alloc_num_next_node(node1%numnod, neib_nod1)
      call allocate_work_next_node(np_smp, node1%numnod)
!
      call count_nod_4_grp_smp(np_smp, node1%numnod, ele1%numele,       &
     &    ele1%nnod_4_ele, ele1%ie, node1%istack_nod_smp, node1%numnod, &
     &    ele_4_nod1%ntot, ele_4_nod1%istack_4_node,                    &
     &    ele_4_nod1%iele_4_node, neib_nod1%nnod_next)
!
      call s_cal_minmax_and_stacks                                      &
     &   (node1%numnod, neib_nod1%nnod_next, izero,                     &
     &    neib_nod1%istack_next, neib_nod1%ntot,                        &
     &    neib_nod1%nmax, neib_nod1%nmin)
!
!
      call alloc_inod_next_node(neib_nod1)
!
!
      call set_nod_4_grp_smp                                            &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    node1%istack_nod_smp, node1%numnod, ele_4_nod1%ntot,          &
     &    ele_4_nod1%istack_4_node, ele_4_nod1%iele_4_node,             &
     &    neib_nod1%ntot, neib_nod1%istack_next,                        &
     &    neib_nod1%nnod_next, neib_nod1%inod_next,                     &
     &    neib_nod1%iweight_next)
!
      call deallocate_work_next_node
!
!
      neib_nod1%iweight_next(1:neib_nod1%ntot)                          &
     &     = - neib_nod1%iweight_next(1:neib_nod1%ntot)
!
      call move_myself_2_first_smp(np_smp, node1%numnod,                &
     &    neib_nod1%ntot, node1%istack_nod_smp, neib_nod1%istack_next,  &
     &    neib_nod1%inod_next, neib_nod1%iweight_next)
!
      call sort_next_node_list_by_weight(np_smp, node1%numnod,          &
     &    neib_nod1%ntot, node1%istack_nod_smp, neib_nod1%istack_next,  &
     &    neib_nod1%inod_next, neib_nod1%iweight_next)
!
      neib_nod1%iweight_next(1:neib_nod1%ntot)                          &
     &     = - neib_nod1%iweight_next(1:neib_nod1%ntot)
!
      end subroutine const_next_nod_id_4_node
!
!-----------------------------------------------------------------------
!
      end module m_next_node_id_4_node
