!>@file   set_element_id_4_node.f90
!!@brief  module set_element_id_4_node
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2006
!
!> @brief Set belonged element list for each node
!!
!!@verbatim
!!      subroutine set_belonged_ele_and_next_nod
!!
!!      subroutine set_ele_id_4_node
!!      subroutine set_ele_id_4_node_t
!!      subroutine set_surf_id_4_node
!!      subroutine set_edge_id_4_node
!!
!!      subroutine set_layerd_ele_id_4_node(nnod, iele_start, iele_end)
!!      subroutine set_grouped_ele_id_4_node(nele_grp, iele_grp)
!!
!!      subroutine const_next_nod_id_4_node
!!@endverbatim
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
      subroutine set_belonged_ele_and_next_nod
!
      use m_next_node_id_4_node
!
!
!      Search surrounding node and element
!
      call set_ele_id_4_node
      call const_next_nod_id_4_node
!
      end subroutine set_belonged_ele_and_next_nod
!
!-----------------------------------------------------------------------
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
      subroutine set_ele_id_4_node_t
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
!
      call alloc_nele_belonged_type(numnod, ele_4_nod)
!
      call count_iele_4_node(numnod, numele, nnod_4_ele, ie,            &
     &    ione, numele, ele_4_nod%nele_4_node)
      call s_cal_minmax_and_stacks(numnod, ele_4_nod%nele_4_node,       &
     &    izero, ele_4_nod%istack_4_node, ele_4_nod%ntot,               &
     &    ele_4_nod%nmax, ele_4_nod%nmin)
!
!
      call alloc_iele_belonged_type(ele_4_nod)
!
      call set_iele_4_node(numnod, numele, nnod_4_ele, ie,              &
     &    ione, numele, ele_4_nod%ntot, ele_4_nod%istack_4_node,        &
     &    ele_4_nod%nele_4_node, ele_4_nod%iele_4_node,                 &
     &    ele_4_nod%iconn_4_node)
!
      end subroutine set_ele_id_4_node_t
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_node
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
!
      call alloc_nele_belonged_type(numnod, surf_4_nod)
!
      call count_iele_4_node(numnod, numsurf, nnod_4_surf, ie_surf,     &
     &    ione, numsurf, surf_4_nod%nele_4_node)
      call s_cal_minmax_and_stacks(numnod,                              &
     &    surf_4_nod%nele_4_node, izero, surf_4_nod%istack_4_node,      &
     &    surf_4_nod%ntot, surf_4_nod%nmax, surf_4_nod%nmin)
!
!
      call alloc_iele_belonged_type(surf_4_nod)
!
      call set_iele_4_node(numnod, numsurf, nnod_4_surf, ie_surf,       &
     &    ione, numsurf, surf_4_nod%ntot, surf_4_nod%istack_4_node,     &
     &    surf_4_nod%nele_4_node, surf_4_nod%iele_4_node,               &
     &    surf_4_nod%iconn_4_node)
!
      end subroutine set_surf_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_id_4_node
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
!
      call alloc_nele_belonged_type(numnod, edge_4_nod)
!
      call count_iele_4_node(numnod, numedge, nnod_4_edge, ie_edge,     &
     &    ione, numedge, edge_4_nod%nele_4_node)
      call s_cal_minmax_and_stacks(numnod,                              &
     &    edge_4_nod%nele_4_node, izero, edge_4_nod%istack_4_node,      &
     &    edge_4_nod%ntot, edge_4_nod%nmax, edge_4_nod%nmin)
!
!
      call alloc_iele_belonged_type(edge_4_nod)
!
      call set_iele_4_node(numnod, numedge, nnod_4_edge, ie_edge,       &
     &    ione, numedge, edge_4_nod%ntot, edge_4_nod%istack_4_node,     &
     &    edge_4_nod%nele_4_node, edge_4_nod%iele_4_node,               &
     &    edge_4_nod%iconn_4_node)
!
      end subroutine set_edge_id_4_node
!
!-----------------------------------------------------------------------
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
      end module set_element_id_4_node
