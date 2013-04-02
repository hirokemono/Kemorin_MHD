!
!      module set_element_id_4_node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine set_ele_id_4_node
!      subroutine set_layerd_ele_id_4_node(nnod, iele_start, iele_end)
!      subroutine set_grouped_ele_id_4_node(nele_grp, iele_grp)
!
!      subroutine const_next_nod_id_4_node
!
      module set_element_id_4_node
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_ele_id_4_node
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
!
      call allocate_numele_belonged(numnod)
!
      call count_iele_4_node(numnod, numele, nnod_4_ele, ie,            &
     &    ione, numele, nele_4_node)
      call s_cal_minmax_and_stacks(numnod, nele_4_node, izero,          &
     &    iele_stack_4_node, ntot_ele_4_node,                           &
     &    nmax_ele_4_node, nmin_ele_4_node)
!
!
      call allocate_iele_belonged
!
      call set_iele_4_node(numnod, numele, nnod_4_ele, ie,              &
     &    ione, numele, ntot_ele_4_node, iele_stack_4_node,             &
     &    nele_4_node, iele_4_node, iconn_4_node)
!
      end subroutine set_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_layerd_ele_id_4_node(nnod, iele_start, iele_end)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nnod, iele_start, iele_end
!
!
      call allocate_numele_belonged(numnod)
!
      call count_iele_4_node(numnod, numele, nnod, ie,                  &
     &    iele_start, iele_end, nele_4_node)
      call s_cal_minmax_and_stacks(numnod, nele_4_node, izero,          &
     &    iele_stack_4_node, ntot_ele_4_node,                           &
     &    nmax_ele_4_node, nmin_ele_4_node)
!
!
      call allocate_iele_belonged
!
      call set_iele_4_node(numnod, numele, nnod, ie,                    &
     &    iele_start, iele_end, ntot_ele_4_node, iele_stack_4_node,     &
     &    nele_4_node, iele_4_node, iconn_4_node)
!
      end subroutine set_layerd_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_grouped_ele_id_4_node(nele_grp, iele_grp)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
      use find_grp_ele_id_4_node
      use cal_minmax_and_stacks
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
!
      call allocate_numele_belonged(numnod)
!
      call count_grp_iele_4_node(numnod, numele, nnod_4_ele, ie,        &
     &    nele_grp, iele_grp, nele_4_node)
      call s_cal_minmax_and_stacks(numnod, nele_4_node, izero,          &
     &    iele_stack_4_node, ntot_ele_4_node,                           &
     &    nmax_ele_4_node, nmin_ele_4_node)
!
      call allocate_iele_belonged
!
      call set_grp_iele_4_node(numnod, numele, nnod_4_ele, ie,          &
     &    nele_grp, iele_grp, ntot_ele_4_node, iele_stack_4_node,       &
     &    nele_4_node, iele_4_node, iconn_4_node)
!
      end subroutine set_grouped_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine const_next_nod_id_4_node
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
!
      use m_element_id_4_node
      use m_next_node_id_4_node
      use find_node_4_group
      use cal_minmax_and_stacks
!
!
      call allocate_num_next_node(numnod)
      call allocate_work_next_node(np_smp, numnod)
!
      call count_nod_4_grp_smp(np_smp, numnod, numele, nnod_4_ele, ie,  &
     &    inod_smp_stack, numnod, ntot_ele_4_node, iele_stack_4_node,   &
     &    iele_4_node, nnod_next_4_node)
!
      call s_cal_minmax_and_stacks(numnod, nnod_next_4_node, izero,     &
     &    inod_next_stack_4_node, ntot_next_nod_4_node,                 &
     &    nmax_next_nod_4_node, nmin_next_nod_4_node)
!
!
      call allocate_inod_next_node
!
!
      call set_nod_4_grp_smp(np_smp, numnod, numele, nnod_4_ele, ie,    &
     &    inod_smp_stack, numnod, ntot_ele_4_node, iele_stack_4_node,   &
     &    iele_4_node, ntot_next_nod_4_node, inod_next_stack_4_node,    &
     &    nnod_next_4_node, inod_next_4_node, iweight_next_4_node)
!
      call deallocate_work_next_node
!
!
      iweight_next_4_node(1:ntot_next_nod_4_node)                       &
     &     = - iweight_next_4_node(1:ntot_next_nod_4_node)
!
      call move_myself_2_first_smp(np_smp, numnod,                      &
     &    ntot_next_nod_4_node, inod_smp_stack, inod_next_stack_4_node, &
     &    inod_next_4_node, iweight_next_4_node)
!
      call sort_next_node_list_by_weight(np_smp, numnod,                &
     &    ntot_next_nod_4_node, inod_smp_stack, inod_next_stack_4_node, &
     &    inod_next_4_node, iweight_next_4_node)
!
      iweight_next_4_node(1:ntot_next_nod_4_node)                       &
     &     = - iweight_next_4_node(1:ntot_next_nod_4_node)
!
      end subroutine const_next_nod_id_4_node
!
!-----------------------------------------------------------------------
!
      end module set_element_id_4_node
