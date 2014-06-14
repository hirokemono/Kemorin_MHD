!
!      module const_2nd_edge_and_surface
!
!      Written by H. Matsui on July, 2006
!
!      subroutine const_2nd_surface_data
!      subroutine const_2nd_edge_data
!
      module const_2nd_edge_and_surface
!
      use m_precision
!
      use m_machine_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_2nd_surface_data
!
      use m_surface_hash
!
      use set_surface_hash
      use mark_surf_hash
      use set_surface_data
!
!
      call allocate_surface_hash(nnod_2nd, nele_2nd, surf_2nd%nnod_4_surf)
!
!       if (iflag_debug.eq.1) write(*,*) 'count_surface_hash'
      call count_surface_hash(nnod_2nd, nele_2nd, nnod_4_ele_2nd,       &
     &          surf_2nd%nnod_4_surf, ie_2nd)
!
!       if (iflag_debug.eq.1) write(*,*) 'set_surf_hash'
      call set_surf_hash(nele_2nd, nnod_4_ele_2nd, ie_2nd)
!
!   mark for all surfaces
!
!       if (iflag_debug.eq.1) write(*,*) 'mark_all_surfaces'
      call mark_all_surfaces(nele_2nd, nnod_4_ele_2nd, ie_2nd)
!
!   set surface data
!
!       if (iflag_debug.eq.1) write(*,*) 'count_all_surfaces'
      call count_all_surfaces(nele_2nd, surf_2nd%numsurf)
!
      call allocate_2nd_surface_connect
!
!       if (iflag_debug.eq.1) write(*,*) 'set_all_surfaces'
      call set_all_surfaces(nele_2nd, surf_2nd%numsurf, nnod_4_ele_2nd,        &
     &    surf_2nd%nnod_4_surf, ie_2nd, surf_2nd%node_on_sf, surf_2nd%ie_surf,         &
     &     surf_2nd%isf_4_ele)
!
      call deallocate_surface_hash
!
      end subroutine const_2nd_surface_data
!
!------------------------------------------------------------------
!
      subroutine const_2nd_edge_data
!
      use m_edge_hash
!
      use set_edge_hash
      use mark_edge_hash
      use set_edge_data
!
!
      call allocate_edge_hash(nnod_2nd, surf_2nd%numsurf, edge_2nd%nnod_4_edge)
!
!       if (iflag_debug.eq.1) write(*,*) 'count_edge_hash_4_sf'
      call count_edge_hash_4_sf(nnod_2nd, surf_2nd%numsurf, surf_2nd%nnod_4_surf,   &
     &          edge_2nd%nnod_4_edge, surf_2nd%ie_surf)
!
!       if (iflag_debug.eq.1) write(*,*) 'set_edge_hash_4_sf'
      call set_edge_hash_4_sf(surf_2nd%numsurf, surf_2nd%nnod_4_surf, surf_2nd%ie_surf)
!
!
!       if (iflag_debug.eq.1) write(*,*) 'mark_all_edges'
      call mark_all_edges(surf_2nd%numsurf, surf_2nd%nnod_4_surf, surf_2nd%ie_surf)
!
!
!       if (iflag_debug.eq.1) write(*,*) 'count_num_edges'
      call count_num_edges(edge_2nd%numedge)
!
      call allocate_edge_connect_type(edge_2nd, surf_2nd%numsurf)
!
!       if (iflag_debug.eq.1) write(*,*) 'set_edges_connection'
      call set_edges_connection(surf_2nd%numsurf, edge_2nd%numedge,  &
     &    surf_2nd%nnod_4_surf, edge_2nd%nnod_4_edge, surf_2nd%ie_surf,              &
     &    edge_2nd%ie_edge, edge_2nd%iedge_4_sf, edge_2nd%node_on_edge)
!
!
      call allocate_edge_4_ele_type(edge_2nd, nele_2nd)
!
!       if (iflag_debug.eq.1) write(*,*) 'set_edges_connect_4_ele'
      call set_edges_connect_4_ele(nele_2nd, surf_2nd%numsurf, edge_2nd%numedge,      &
     &    nnod_4_ele_2nd, edge_2nd%nnod_4_edge, ie_2nd, edge_2nd%iedge_4_sf, &
     &    edge_2nd%ie_edge, edge_2nd%iedge_4_ele)
!
      call deallocate_edge_hash
!
      end subroutine const_2nd_edge_data
!
!------------------------------------------------------------------
!
      end module const_2nd_edge_and_surface
