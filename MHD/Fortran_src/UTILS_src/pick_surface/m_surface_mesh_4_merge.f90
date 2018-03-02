!
!      module m_surface_mesh_4_merge
!
!      Written by Kemorin
!
      module m_surface_mesh_4_merge
!
      use m_precision
      use m_constants
      use t_merged_viewer_mesh
!
      implicit none
!
!
      type(merged_viewer_mesh), save :: mgd_view_mesh1
!
!mgd_view_mesh1%view_mesh
        type(viewer_surface_groups), save :: domain_grps
!
        type(viewer_node_groups), save :: view_nod_grps
        type(viewer_surface_groups), save :: view_ele_grps
        type(viewer_surface_groups), save :: view_sf_grps
!
!
      character (len = kchara) :: surface_file_head = 'in_surface'
      character (len = kchara) :: surface_file_name = 'in_surface.ksm'
!
      end module m_surface_mesh_4_merge
