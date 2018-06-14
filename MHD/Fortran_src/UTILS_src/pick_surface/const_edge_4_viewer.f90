!
!      module const_edge_4_viewer
!
!     Written by H. Matsui on Jan., 2007
!
!!      subroutine construct_edge_4_viewer(surf, edge, num_pe,          &
!!     &          inod_sf_stack, nedge_sf, iedge_sf_stack, view_mesh,   &
!!     &          domain_grps, view_ele_grps, view_sf_grps)
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(viewer_surface_groups), intent(inout) :: domain_grps
!!        type(viewer_surface_groups), intent(inout) :: view_ele_grps
!!        type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      module const_edge_4_viewer
!
      use m_precision
!
      use m_geometry_constants
!
      use t_viewer_mesh
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
      subroutine construct_edge_4_viewer(surf, edge, num_pe,            &
     &          inod_sf_stack, nedge_sf, iedge_sf_stack, view_mesh,     &
     &          domain_grps, view_ele_grps, view_sf_grps)
!
      use t_surface_data
      use t_edge_data
      use const_grp_edge_4_viewer
      use count_edge_domain_4_viewer
!
      integer(kind = kint), intent(in)  :: num_pe
      integer(kind = kint), intent(in) :: inod_sf_stack(0:num_pe)
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint), intent(inout) :: nedge_sf(num_pe)
      integer(kind = kint), intent(inout) :: iedge_sf_stack(0:num_pe)
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
!
      call alloc_sum_hash                                               &
     &   (view_mesh%nodpetot_viewer, view_mesh%nsurf_viewer,            &
     &    nedge_4_surf, edge%nnod_4_edge, edge_sf_tbl)
!
      call const_all_edge_4_viewer(surf, edge, view_mesh, edge_sf_tbl)
!
      call construct_edge_4_domain                                      &
     &   (surf%nnod_4_surf, edge%nnod_4_edge, view_mesh,                &
     &    domain_grps%surf_grp, domain_grps%edge_grp, edge_sf_tbl)
!
      call construct_edge_4_ele_grp                                     &
     &   (surf%nnod_4_surf, edge%nnod_4_edge, num_pe, view_mesh,        &
     &    view_ele_grps%num_grp,  view_ele_grps%surf_grp,               &
     &    view_ele_grps%edge_grp, edge_sf_tbl)
      call construct_edge_4_surf_grp                                    &
     &   (surf%nnod_4_surf, edge%nnod_4_edge, num_pe, view_mesh,        &
     &    view_sf_grps%num_grp, view_sf_grps%surf_grp,                  &
     &    view_sf_grps%edge_grp, edge_sf_tbl)
!
      call dealloc_sum_hash(edge_sf_tbl)
!
         write(*,*)  'count_nedge_4_each_domain'
      call count_nedge_4_each_domain                                    &
     &   (num_pe, inod_sf_stack, nedge_sf, iedge_sf_stack, view_mesh)
         write(*,*)  'count_nedge_domain_4_domain'
      call count_nedge_domain_4_domain(num_pe, inod_sf_stack,           &
     &    view_mesh, domain_grps%edge_grp)
         write(*,*)  'count_nedge_ele_grp_4_domain'
      call count_nedge_ele_grp_4_domain                                 &
     &   (num_pe, inod_sf_stack, view_mesh,                             &
     &    view_ele_grps%num_grp, view_ele_grps%edge_grp)
         write(*,*)  'count_nedge_surf_grp_4_domain'
      call count_nedge_surf_grp_4_domain                                &
     &   (num_pe, inod_sf_stack, view_mesh,                             &
     &    view_sf_grps%num_grp, view_sf_grps%edge_grp)
!
      end subroutine construct_edge_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_all_edge_4_viewer                                &
     &         (surf, edge, view_mesh, ed_sf_tbl)
!
      use t_surface_data
      use t_edge_data
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(sum_hash_tbl), intent(inout) :: ed_sf_tbl
!
!   set hash data for edge elements using sum of local node ID
!
      call clear_sum_hash(ed_sf_tbl)
!
      write(*,*) 'const_edge_hash_4_sf'
      call const_edge_hash_4_sf                                         &
     &   (view_mesh%nodpetot_viewer, view_mesh%nsurf_viewer,            &
     &    surf%nnod_4_surf, edge%nnod_4_edge, view_mesh%ie_sf_viewer,   &
     &    ed_sf_tbl%num_hash, ed_sf_tbl%istack_hash,                    &
     &    ed_sf_tbl%iend_hash, ed_sf_tbl%id_hash, ed_sf_tbl%iflag_hash)
!
      write(*,*) 'count_num_edges_by_sf'
      call count_num_edges_by_sf                                        &
     &   (view_mesh%nodpetot_viewer, view_mesh%nsurf_viewer,            &
     &    edge%nnod_4_edge, ed_sf_tbl%istack_hash, ed_sf_tbl%iend_hash, &
     &    ed_sf_tbl%iflag_hash, view_mesh%edgepetot_viewer)
!
      call alloc_edge_data_4_sf(edge%nnod_4_edge, view_mesh)
!
      write(*,*) 'set_edges_connect_by_sf'
      call set_edges_connect_by_sf(view_mesh%nodpetot_viewer,           &
     &    view_mesh%nsurf_viewer, view_mesh%edgepetot_viewer,           &
     &    surf%nnod_4_surf, edge%nnod_4_edge, view_mesh%ie_sf_viewer,   &
     &    ed_sf_tbl%istack_hash, ed_sf_tbl%iend_hash,                   &
     &    ed_sf_tbl%id_hash, ed_sf_tbl%iflag_hash,                      &
     &    view_mesh%ie_edge_viewer, view_mesh%iedge_sf_viewer,          &
     &    edge%node_on_edge_sf)
!
      end subroutine const_all_edge_4_viewer
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_domain                                &
     &         (nnod_4_surf, nnod_4_edge, view_mesh, domain_surf_grp,   &
     &          domain_edge_grp, ed_sf_tbl)
!
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_group_data), intent(in) :: domain_surf_grp
      type(sum_hash_tbl), intent(inout) :: ed_sf_tbl
      type(viewer_group_data), intent(inout) :: domain_edge_grp
!
!   set hash data for edge elements using sum of local node ID
!
      call clear_sum_hash(ed_sf_tbl)
!
!      write(*,*) 'const_part_edge_hash_4_sf'
      call const_part_edge_hash_4_sf                                    &
     &   (view_mesh%nodpetot_viewer, view_mesh%nsurf_viewer,            &
     &    domain_surf_grp%num_item, nnod_4_surf, nnod_4_edge,           &
     &    view_mesh%ie_sf_viewer, domain_surf_grp%item_sf,              &
     &    ed_sf_tbl%num_hash, ed_sf_tbl%istack_hash,                    &
     &    ed_sf_tbl%iend_hash, ed_sf_tbl%id_hash, ed_sf_tbl%iflag_hash)
!
!
      call count_num_edges_by_sf                                        &
     &   (view_mesh%nodpetot_viewer, view_mesh%nsurf_viewer,            &
     &    nnod_4_edge, ed_sf_tbl%istack_hash, ed_sf_tbl%iend_hash,      &
     &    ed_sf_tbl%iflag_hash, domain_edge_grp%num_item)
!
      call alloc_merged_group_item(domain_edge_grp)
!
!      write(*,*) 'set_part_edges_4_sf'
      call set_part_edges_4_sf                                          &
     &   (view_mesh%nodpetot_viewer, view_mesh%nsurf_viewer,            &
     &    nnod_4_edge, domain_edge_grp%num_item,                        &
     &    view_mesh%iedge_sf_viewer,ed_sf_tbl%istack_hash,              &
     &    ed_sf_tbl%iend_hash, ed_sf_tbl%id_hash, ed_sf_tbl%iflag_hash, &
     &    domain_edge_grp%item_sf)
!
      end subroutine construct_edge_4_domain
!
!------------------------------------------------------------------
!
      end module const_edge_4_viewer
