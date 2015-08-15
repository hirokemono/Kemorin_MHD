!
!      module const_edge_4_viewer
!
!     Written by H. Matsui on Jan., 2007
!
!      subroutine construct_edge_4_viewer
!
      module const_edge_4_viewer
!
      use m_precision
!
      use m_geometry_constants
      use m_geometry_data
      use m_surface_mesh_4_merge
!
      use t_sum_hash
!
      implicit    none
!
      type(sum_hash_tbl), save :: edge_sf_tbl
!
      private :: edge_sf_tbl
      private :: const_all_edge_4_viewer, construct_edge_4_domain
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_viewer
!
      use const_grp_edge_4_viewer
      use count_edge_domain_4_viewer
!
!
      call alloc_sum_hash(nodpetot_viewer, surfpetot_viewer,        &
     &    nedge_4_surf, nnod_4_edge, edge_sf_tbl)
!
      call const_all_edge_4_viewer(edge_sf_tbl)
!
      call construct_edge_4_domain(edge_sf_tbl)
!
      call construct_edge_4_ele_grp(edge_sf_tbl)
      call construct_edge_4_surf_grp(edge_sf_tbl)
!
      call dealloc_sum_hash(edge_sf_tbl)
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
      subroutine const_all_edge_4_viewer(ed_sf_tbl)
!
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      type(sum_hash_tbl), intent(inout) :: ed_sf_tbl
!
!   set hash data for edge elements using sum of local node ID
!
      call clear_sum_hash(ed_sf_tbl)
!
      write(*,*) 'const_edge_hash_4_sf'
      call const_edge_hash_4_sf(nodpetot_viewer, surfpetot_viewer,      &
     &    surf1%nnod_4_surf, nnod_4_edge, ie_sf_viewer,                 &
     &    ed_sf_tbl%num_hash, ed_sf_tbl%istack_hash,                    &
     &    ed_sf_tbl%iend_hash, ed_sf_tbl%id_hash, ed_sf_tbl%iflag_hash)
!
      write(*,*) 'count_num_edges_by_sf'
      call count_num_edges_by_sf(nodpetot_viewer, surfpetot_viewer,     &
     &    nnod_4_edge, ed_sf_tbl%istack_hash, ed_sf_tbl%iend_hash,      &
     &    ed_sf_tbl%iflag_hash, edgepetot_viewer)
!
      call allocate_edge_data_4_sf
!
      write(*,*) 'set_edges_connect_by_sf'
      call set_edges_connect_by_sf                                      &
     &   (nodpetot_viewer, surfpetot_viewer, edgepetot_viewer,          &
     &    surf1%nnod_4_surf, nnod_4_edge, ie_sf_viewer,                 &
     &    ed_sf_tbl%istack_hash, ed_sf_tbl%iend_hash,                   &
     &    ed_sf_tbl%id_hash, ed_sf_tbl%iflag_hash,                      &
     &    ie_edge_viewer, iedge_sf_viewer, node_on_edge_sf)
!
      end subroutine const_all_edge_4_viewer
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_domain(ed_sf_tbl)
!
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      type(sum_hash_tbl), intent(inout) :: ed_sf_tbl
!
!   set hash data for edge elements using sum of local node ID
!
      call clear_sum_hash(ed_sf_tbl)
!
      write(*,*) 'const_part_edge_hash_4_sf'
      call const_part_edge_hash_4_sf                                    &
     &   (nodpetot_viewer, surfpetot_viewer, nsurf_domain_sf,           &
     &    surf1%nnod_4_surf, nnod_4_edge,                               &
     &    ie_sf_viewer, isurf_domain_sf,                                &
     &    ed_sf_tbl%num_hash, ed_sf_tbl%istack_hash,                    &
     &    ed_sf_tbl%iend_hash, ed_sf_tbl%id_hash, ed_sf_tbl%iflag_hash)
!
!
      call count_num_edges_by_sf(nodpetot_viewer, surfpetot_viewer,     &
     &    nnod_4_edge, ed_sf_tbl%istack_hash, ed_sf_tbl%iend_hash,      &
     &    ed_sf_tbl%iflag_hash, nedge_domain_sf)
!
      call allocate_domain_edge_item_sf
!
      write(*,*) 'set_part_edges_4_sf'
      call set_part_edges_4_sf(nodpetot_viewer, surfpetot_viewer,       &
     &    nnod_4_edge, nedge_domain_sf, iedge_sf_viewer,                &
     &    ed_sf_tbl%istack_hash, ed_sf_tbl%iend_hash,                   &
     &    ed_sf_tbl%id_hash, ed_sf_tbl%iflag_hash,                      &
     &    edge_item_domain_sf)
!
      end subroutine construct_edge_4_domain
!
!------------------------------------------------------------------
!
      end module const_edge_4_viewer
