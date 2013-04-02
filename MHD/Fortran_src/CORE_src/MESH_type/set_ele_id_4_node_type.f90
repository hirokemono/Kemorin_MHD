!set_ele_id_4_node_type.f90
!      module set_ele_id_4_node_type
!
!      Written by H.Matsui on Dec., 2008
!
!      subroutine s_set_ele_id_4_node_type(mesh, neib_ele)
!        type(mesh_geometry), intent(in) :: mesh
!        type(element_around_node), intent(inout) :: neib_ele
!      subroutine set_layerd_ele_id_4_node_type(nnod,                   &
!     &          iele_start, iele_end, mesh, neib_ele)
!        integer(kind = kint), intent(in) :: nnod
!        integer(kind = kint), intent(in) :: iele_start, iele_end
!        type(mesh_geometry), intent(in) :: mesh
!        type(element_around_node), intent(inout) :: neib_ele
!      subroutine set_grouped_ele_id_4_node_type(nele_grp, iele_grp,    &
!     &          mesh, neib_ele)
!        integer (kind=kint), intent(in) :: nele_grp
!        integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!        type(mesh_geometry), intent(in) :: mesh
!        type(element_around_node), intent(inout) :: neib_ele
!
!      subroutine const_next_nod_id_4_node_type(mesh, neib_ele,         &
!     &          neib_nod)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(element_around_node), intent(in) :: neib_ele
!        type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
      module set_ele_id_4_node_type
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
      subroutine s_set_ele_id_4_node_type(mesh, neib_ele)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_nele_belonged_type(mesh%node%numnod, neib_ele)
!
      call count_iele_4_node(mesh%node%numnod, mesh%ele%numele,         &
     &    mesh%ele%nnod_4_ele, mesh%ele%ie, ione, mesh%ele%numele,      &
     &    neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks(mesh%node%numnod,                    &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged_type(neib_ele)
!
      call set_iele_4_node(mesh%node%numnod, mesh%ele%numele,           &
     &    mesh%ele%nnod_4_ele, mesh%ele%ie,  ione, mesh%ele%numele,     &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine s_set_ele_id_4_node_type
!
!-----------------------------------------------------------------------
!
      subroutine set_layerd_ele_id_4_node_type(nnod,                    &
     &          iele_start, iele_end, mesh, neib_ele)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: iele_start, iele_end
      type(mesh_geometry), intent(in) :: mesh
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_nele_belonged_type(mesh%node%numnod, neib_ele)
!
      call count_iele_4_node(mesh%node%numnod, mesh%ele%numele, nnod,   &
     &    mesh%ele%ie, iele_start, iele_end, neib_ele%nele_4_node )
      call s_cal_minmax_and_stacks(mesh%node%numnod,                    &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged_type(neib_ele)
!
      call set_iele_4_node(mesh%node%numnod, mesh%ele%numele, nnod,     &
     &    mesh%ele%ie,  iele_start, iele_end, neib_ele%ntot,            &
     &    neib_ele%istack_4_node, neib_ele%nele_4_node,                 &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_layerd_ele_id_4_node_type
!
!-----------------------------------------------------------------------
!
      subroutine set_grouped_ele_id_4_node_type(nele_grp, iele_grp,     &
     &          mesh, neib_ele)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use find_grp_ele_id_4_node
      use cal_minmax_and_stacks
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      type(mesh_geometry), intent(in) :: mesh
!
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_nele_belonged_type(mesh%node%numnod, neib_ele)
!
      call count_grp_iele_4_node(mesh%node%numnod, mesh%ele%numele,     &
     &    mesh%ele%nnod_4_ele, mesh%ele%ie, nele_grp, iele_grp,         &
     &    neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks(mesh%node%numnod,                    &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
      call alloc_iele_belonged_type(neib_ele)
!
      call set_grp_iele_4_node(mesh%node%numnod, mesh%ele%numele,       &
     &    mesh%ele%nnod_4_ele, mesh%ele%ie, nele_grp, iele_grp,         &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_grouped_ele_id_4_node_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_next_nod_id_4_node_type(mesh, neib_ele,          &
     &          neib_nod)
!
      use m_machine_parameter
      use t_mesh_data
      use t_next_node_ele_4_node
!
      use find_node_4_group
      use cal_minmax_and_stacks
!
      type(mesh_geometry),       intent(in) :: mesh
      type(element_around_node), intent(in) :: neib_ele
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
!
      call alloc_num_next_node_type(mesh%node%numnod, neib_nod)
      call allocate_work_next_node(np_smp, mesh%node%numnod)
!
      call count_nod_4_grp_smp(np_smp, mesh%node%numnod,                &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,            &
     &    mesh%node%istack_nod_smp, mesh%node%numnod, neib_ele%ntot,    &
     &    neib_ele%istack_4_node,  neib_ele%iele_4_node,                &
     &    neib_nod%nnod_next)
!
      call s_cal_minmax_and_stacks(mesh%node%numnod,                    &
     &    neib_nod%nnod_next, izero, neib_nod%istack_next,              &
     &    neib_nod%ntot, neib_nod%nmax, neib_nod%nmin)
!
!
      call alloc_inod_next_node_type(neib_nod)
!
!
      call set_nod_4_grp_smp(np_smp, mesh%node%numnod, mesh%ele%numele, &
     &    mesh%ele%nnod_4_ele, mesh%ele%ie, mesh%node%istack_nod_smp,   &
     &    mesh%node%numnod, neib_ele%ntot, neib_ele%istack_4_node,      &
     &    neib_ele%iele_4_node, neib_nod%ntot, neib_nod%istack_next,    &
     &    neib_nod%nnod_next, neib_nod%inod_next,                       &
     &    neib_nod%iweight_next)
!
      call deallocate_work_next_node
!
!
      neib_nod%iweight_next(1:neib_nod%ntot)                            &
     &     = - neib_nod%iweight_next(1:neib_nod%ntot)
!
      call move_myself_2_first_smp(np_smp, mesh%node%numnod,            &
     &    neib_nod%ntot, mesh%node%istack_nod_smp,                      &
     &    neib_nod%istack_next, neib_nod%inod_next,                     &
     &    neib_nod%iweight_next)
!
      call sort_next_node_list_by_weight(np_smp, mesh%node%numnod,      &
     &    neib_nod%ntot, mesh%node%istack_nod_smp,                      &
     &    neib_nod%istack_next, neib_nod%inod_next,                     &
     &    neib_nod%iweight_next)
!
      neib_nod%iweight_next(1:neib_nod%ntot)                            &
     &     = - neib_nod%iweight_next(1:neib_nod%ntot)
!
      end subroutine const_next_nod_id_4_node_type
!
!-----------------------------------------------------------------------
!
      end module set_ele_id_4_node_type
