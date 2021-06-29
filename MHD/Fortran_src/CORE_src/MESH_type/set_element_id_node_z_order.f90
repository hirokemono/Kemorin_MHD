!>@file   set_element_id_node_z_order.f90
!!@brief  module set_element_id_node_z_order
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Set belonged element list for each node
!!
!!@verbatim
!!      subroutine set_ele_id_4_node_z_order(node, ele, neib_ele)
!!      subroutine set_surf_id_4_node_z_order(node, surf, neib_ele)
!!      subroutine set_edge_id_4_node_z_order(node, edge, neib_ele)
!!        type(node_data),        intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(element_around_node), intent(inout) :: neib_ele
!!@endverbatim
!
      module set_element_id_node_z_order
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
      subroutine set_ele_id_4_node_z_order(node, ele, neib_ele)
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data),           intent(in) :: node
      type(element_data),        intent(in) :: ele
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_numele_belonged(node%numnod, neib_ele)
!
      call count_iele_4_node(node%numnod, ele%numele, ele%nnod_4_ele,   &
     &    ele%ie, ione, ele%numele, neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged(neib_ele)
!
      call set_iele_4_node_z_order(node%numnod, ele%numele,             &
     &    ele%nnod_4_ele, ele%ie, ele%x_ele, ione, ele%numele,          &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_ele_id_4_node_z_order
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_node_z_order(node, surf, neib_ele)
!
      use t_geometry_data
      use t_surface_data
      use t_next_node_ele_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_numele_belonged(node%numnod, neib_ele)
!
      call count_iele_4_node(node%numnod, surf%numsurf,                 &
     &    surf%nnod_4_surf, surf%ie_surf, ione, surf%numsurf,           &
     &    neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged(neib_ele)
!
      call set_iele_4_node_z_order                                      &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                  &
     &    surf%ie_surf, surf%x_surf, ione, surf%numsurf,                &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_surf_id_4_node_z_order
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_id_4_node_z_order(node, edge, neib_ele)
!
      use t_geometry_data
      use t_edge_data
      use t_next_node_ele_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(element_around_node), intent(inout) :: neib_ele
!
!
      call alloc_numele_belonged(node%numnod, neib_ele)
!
      call count_iele_4_node(node%numnod, edge%numedge,                 &
     &    edge%nnod_4_edge, edge%ie_edge, ione, edge%numedge,           &
     &    neib_ele%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    neib_ele%nele_4_node, izero, neib_ele%istack_4_node,          &
     &    neib_ele%ntot, neib_ele%nmax, neib_ele%nmin)
!
!
      call alloc_iele_belonged(neib_ele)
!
      call set_iele_4_node_z_order                                      &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge,                  &
     &    edge%ie_edge, edge%x_edge,  ione, edge%numedge,               &
     &    neib_ele%ntot, neib_ele%istack_4_node, neib_ele%nele_4_node,  &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node)
!
      end subroutine set_edge_id_4_node_z_order
!
!-----------------------------------------------------------------------
!
      end module set_element_id_node_z_order
