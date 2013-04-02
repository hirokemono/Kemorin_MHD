!
!      module const_edge_4_viewer
!
      module const_edge_4_viewer
!
!     Written by H. Matsui on Jan., 2007
!
      use m_precision
!
      use m_geometry_parameter
      use m_surface_mesh_4_merge
!
      implicit    none
!
      private :: const_all_edge_4_viewer
      private :: construct_edge_4_domain
!
!      subroutine construct_edge_4_viewer
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_viewer
!
      use m_edge_hash
      use const_grp_edge_4_viewer
      use count_edge_domain_4_viewer
!
!
      call allocate_edge_hash(nodpetot_viewer, surfpetot_viewer,        &
     &    nnod_4_edge)
!
      call const_all_edge_4_viewer
!
      call construct_edge_4_domain
!
      call construct_edge_4_ele_grp
      call construct_edge_4_surf_grp
!
      call deallocate_edge_hash
!
         write(*,*)  'count_nedge_4_each_domain'
      call count_nedge_4_each_domain
         write(*,*)  'count_nedge_domain_4_domain'
      call count_nedge_domain_4_domain
         write(*,*)  'count_nedge_ele_grp_4_domain'
      call count_nedge_ele_grp_4_domain
         write(*,*)  'count_nedge_surf_grp_4_domain'
      call count_nedge_surf_grp_4_domain
!
      end subroutine construct_edge_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_all_edge_4_viewer
!
      use set_edge_hash
      use mark_edge_hash
      use set_edge_data
!
!   set hash data for edge elements using sum of local node ID
!
      write(*,*) 'count_edge_hash_4_sf'
      call count_edge_hash_4_sf(nodpetot_viewer, surfpetot_viewer,      &
     &    nnod_4_surf, nnod_4_edge, ie_sf_viewer)
!
      write(*,*) 'set_edge_hash_4_sf'
      call set_edge_hash_4_sf(surfpetot_viewer, nnod_4_surf,            &
     &    ie_sf_viewer)
!
!
      write(*,*) 'mark_all_edges'
      call mark_all_edges(surfpetot_viewer, nnod_4_surf, ie_sf_viewer)
!
!
      write(*,*) 'count_num_edges'
      call count_num_edges(edgepetot_viewer)
!
      call allocate_edge_data_4_sf
!
      write(*,*) 'set_edges_connection'
      call set_edges_connection(surfpetot_viewer, edgepetot_viewer,     &
     &    nnod_4_surf, nnod_4_edge, ie_sf_viewer, ie_edge_viewer,       &
     &    iedge_sf_viewer, node_on_edge_sf)
!
      end subroutine const_all_edge_4_viewer
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_domain
!
      use set_edge_hash
      use mark_edge_hash
      use set_edge_data
!
!   set hash data for edge elements using sum of local node ID
!
      call cleear_edge_hash
!
      write(*,*) 'count_part_edge_hash_4_sf'
      call count_part_edge_hash_4_sf(nodpetot_viewer, surfpetot_viewer, &
     &    nsurf_domain_sf, nnod_4_surf, nnod_4_edge, ie_sf_viewer,      &
     &    isurf_domain_sf)
!
      write(*,*) 'set_part_edge_hash_4_sf'
      call set_part_edge_hash_4_sf(surfpetot_viewer, nsurf_domain_sf,   &
     &    nnod_4_surf, ie_sf_viewer, isurf_domain_sf)
!
!
       write(*,*) 'mark_all_edges'
      call mark_all_edges(surfpetot_viewer, nnod_4_surf, ie_sf_viewer)
!
!
      call count_num_edges(nedge_domain_sf)
!
      call allocate_domain_edge_item_sf
!
      write(*,*) 'set_part_edges'
      call set_part_edges(surfpetot_viewer, edgepetot_viewer,           &
     &    nedge_domain_sf, iedge_sf_viewer, edge_item_domain_sf)
!
      end subroutine construct_edge_4_domain
!
!------------------------------------------------------------------
!
      end module const_edge_4_viewer
