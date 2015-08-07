!const_edge_type_data.f90
!      module const_edge_type_data
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine s_const_edge_type_data(nod, ele, surf, edge)
!      subroutine const_ele_list_4_edge_type(ele, edge)
!      subroutine const_surf_list_4_edge_type(surf, edge)
!      subroutine empty_edge_connect_type(ele, surf, edge)
!        type(node_data),    intent(in) :: nod
!        type(element_data), intent(in) :: ele
!        type(surface_data), intent(in) :: surf
!        type(edge_data),    intent(inout) :: edge
!
      module const_edge_type_data
!
      use m_precision
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      use t_sum_hash
!
      implicit none
!
      type(sum_hash_tbl), save, private :: edge_sf_tbl
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_edge_type_data(nod, ele, surf, edge)
!
      use m_machine_parameter
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data),    intent(inout) :: edge
!
!
      call alloc_sum_hash(nod%numnod, surf%numsurf, nedge_4_surf,       &
     &    edge%nnod_4_edge, edge_sf_tbl)
!
!   set hash data for edge elements using sum of local node ID
!
      if (iflag_debug.eq.1) write(*,*) 'const_edge_hash_4_sf'
      call const_edge_hash_4_sf(nod%numnod, surf%numsurf,               &
     &    surf%nnod_4_surf, edge%nnod_4_edge, surf%ie_surf,             &
     &    edge_sf_tbl%num_hash, edge_sf_tbl%istack_hash,                &
     &    edge_sf_tbl%iend_hash, edge_sf_tbl%id_hash,                   &
     &    edge_sf_tbl%iflag_hash)
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_edges_by_sf'
      call count_num_edges_by_sf                                        &
     &   (nod%numnod, surf%numsurf, edge%nnod_4_edge,                   &
     &    edge_sf_tbl%istack_hash, edge_sf_tbl%iend_hash,               &
     &    edge_sf_tbl%iflag_hash, edge%numedge)
!
      call allocate_edge_connect_type(edge, surf%numsurf)
!
      if (iflag_debug.eq.1) write(*,*) 'set_edges_connect_by_sf'
      call set_edges_connect_by_sf                                      &
     &   (nod%numnod, surf%numsurf, edge%numedge,                       &
     &    surf%nnod_4_surf, edge%nnod_4_edge, surf%ie_surf,             &
     &    edge_sf_tbl%istack_hash, edge_sf_tbl%iend_hash,               &
     &    edge_sf_tbl%id_hash, edge_sf_tbl%iflag_hash,                  &
     &    edge%ie_edge, edge%iedge_4_sf, edge%node_on_edge_sf)
!
!
      call allocate_edge_4_ele_type(edge, ele%numele)
!
      if (iflag_debug.eq.1) write(*,*) 'set_edges_connect_4_ele'
      call set_edges_connect_4_ele                                      &
     &   (nod%numnod, ele%numele, surf%numsurf, edge%numedge,           &
     &    ele%nnod_4_ele, edge%nnod_4_edge, ele%ie, edge%iedge_4_sf,    &
     &    edge_sf_tbl%istack_hash, edge_sf_tbl%id_hash,                 &
     &    edge_sf_tbl%iflag_hash, edge%ie_edge, edge%iedge_4_ele)
!
      call dealloc_sum_hash(edge_sf_tbl)
!
      end subroutine s_const_edge_type_data
!
!------------------------------------------------------------------
!
      subroutine const_ele_list_4_edge_type(ele, edge)
!
      use set_element_list_4_surface
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(inout) :: edge
!
      call alloc_ele_4_edge_num_type(edge)
      call count_ele_list_4_edge(ele%numele, edge%numedge, nedge_4_ele, &
     &    edge%iedge_4_ele, edge%ntot_iele_4_edge,                      &
     &    edge%num_iele_4_edge, edge%istack_iele_4_edge)
!
      call alloc_ele_4_edge_item_type(edge)
      call set_ele_list_4_edge(ele%numele, edge%numedge, nedge_4_ele,   &
     &    edge%iedge_4_ele, edge%ntot_iele_4_edge,                      &
     &    edge%num_iele_4_edge, edge%istack_iele_4_edge,                &
     &    edge%iele_4_edge)
!
      end subroutine const_ele_list_4_edge_type
!
!------------------------------------------------------------------
!
      subroutine const_surf_list_4_edge_type(surf, edge)
!
      use set_element_list_4_surface
!
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      call alloc_surf_4_edge_num_type(edge)
      call count_ele_list_4_edge(surf%numsurf, edge%numedge,            &
     &    nedge_4_surf, edge%iedge_4_sf, edge%ntot_isurf_4_edge,        &
     &    edge%num_isurf_4_edge, edge%istack_isurf_4_edge)
!
      call alloc_surf_4_edge_item_type(edge)
      call set_ele_list_4_edge(surf%numsurf, edge%numedge,              &
     &    nedge_4_surf, edge%iedge_4_sf, edge%ntot_isurf_4_edge,        &
     &    edge%num_isurf_4_edge, edge%istack_isurf_4_edge,              &
     &    edge%isurf_4_edge)
!
      end subroutine const_surf_list_4_edge_type
!
!------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine empty_edge_connect_type(ele, surf, edge)
!
      use m_machine_parameter
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      edge%numedge = 0
      if (iflag_debug.eq.1) write(*,*) 's_const_edge_type_data'
      call allocate_edge_connect_type(edge, surf%numsurf)
      call allocate_edge_4_ele_type(edge, ele%numele)
!
      if (iflag_debug.eq.1) write(*,*) 'count_overlap_edge'
      call allocate_edge_param_smp_type(edge)
!
      end subroutine empty_edge_connect_type
!
! ----------------------------------------------------------------------
!
      end module const_edge_type_data
