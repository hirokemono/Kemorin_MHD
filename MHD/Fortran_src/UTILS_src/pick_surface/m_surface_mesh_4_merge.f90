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
      character (len = kchara) :: surface_file_head = 'in_surface'
      character (len = kchara) :: surface_file_name = 'in_surface.ksm'
!
      end module m_surface_mesh_4_merge
