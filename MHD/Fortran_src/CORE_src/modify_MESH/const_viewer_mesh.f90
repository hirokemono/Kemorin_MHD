!>@file   const_viewer_mesh.f90
!!@brief  module const_viewer_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine s_const_viewer_mesh                                  &
!!     &         (mesh, ele_mesh, group, view_mesh, domain_grps,        &
!!     &          view_nod_grps, view_ele_grps, view_sf_grps)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(viewer_surface_groups), intent(inout) :: domain_grps
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!!        type(viewer_surface_groups), intent(inout) :: view_ele_grps
!!        type(viewer_surface_groups), intent(inout) :: view_sf_grps
!!      subroutine dealloc_viewer_mesh(view_mesh, domain_grps,          &
!!     &          view_nod_grps, view_ele_grps, view_sf_grps)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(viewer_surface_groups), intent(inout) :: domain_grps
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!!        type(viewer_surface_groups), intent(inout) :: view_ele_grps
!!        type(viewer_surface_groups), intent(inout) :: view_sf_grps
!!@endverbatim
!
      module const_viewer_mesh
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_mesh_data
      use t_merged_viewer_mesh
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_viewer_mesh                                    &
     &         (mesh, ele_mesh, group, view_mesh, domain_grps,          &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      use t_const_mesh_data_4_viewer
      use const_mesh_list_4_viewer
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_groups), intent(in) :: group
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_node_groups), intent(inout) :: view_nod_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      type(index_list_4_pick_surface) :: idx_lst_s
!
!
      call alloc_index_list_pick_surf                                   &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge, idx_lst_s)
!
      call const_index_list_4_viewer                                    &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    group%nod_grp, group%ele_grp, group%surf_grp,                 &
     &    idx_lst_s, view_mesh)
!
      call const_mesh_data_4_viewer                                     &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    idx_lst_s, view_mesh)
!
!
      call const_domain_groups_4_viewer                                 &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    idx_lst_s, domain_grps)
!
      call const_node_groups_4_viewer(mesh%node, group%nod_grp,         &
     &    idx_lst_s%inod_ksm, idx_lst_s%iflag_node, view_nod_grps)
      call const_element_groups_4_viewer                                &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    group%ele_grp, idx_lst_s, view_ele_grps)
      call const_surface_groups_4_viewer                                &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    group%surf_grp, idx_lst_s, view_sf_grps)
!
      call dealloc_index_list_pick_surf(idx_lst_s)
!
      end subroutine s_const_viewer_mesh
!
!------------------------------------------------------------------
!
      subroutine dealloc_viewer_mesh(view_mesh, domain_grps,            &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_node_groups), intent(inout) :: view_nod_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
!
      call dealloc_viewer_node_grps_item(view_nod_grps)
      call dealloc_viewer_surf_grps_item(view_ele_grps)
      call dealloc_viewer_surf_grps_item(view_sf_grps)
!
      call dealloc_viewer_surf_grps_item(domain_grps)
!
      call dealloc_surf_type_viewer(view_mesh)
      call dealloc_edge_data_4_sf(view_mesh)
      call dealloc_surf_connect_viewer(view_mesh)
      call dealloc_nod_position_viewer(view_mesh)
!
      end subroutine dealloc_viewer_mesh
!
!------------------------------------------------------------------
!
      end module const_viewer_mesh
