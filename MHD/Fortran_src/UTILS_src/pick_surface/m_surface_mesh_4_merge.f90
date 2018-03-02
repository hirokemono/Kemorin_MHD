!
!      module m_surface_mesh_4_merge
!
!      Written by Kemorin
!
!      subroutine allocate_num_mesh_sf
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
!mgd_view_mesh1%inod_sf_stack
        integer(kind = kint), allocatable :: iedge_sf_stack(:)
        integer(kind = kint), allocatable :: isurf_sf_stack(:)
!
        type(viewer_mesh_data), save :: view_mesh
!
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
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_num_mesh_sf(num_pe)
!
      integer(kind = kint), intent(in) :: num_pe
!
      allocate( mgd_view_mesh1%inod_sf_stack(0:num_pe)  )
      allocate( isurf_sf_stack(0:num_pe) )
      allocate( iedge_sf_stack(0:num_pe) )
      mgd_view_mesh1%inod_sf_stack  = 0
      isurf_sf_stack = 0
      iedge_sf_stack = 0
!
      end subroutine allocate_num_mesh_sf
!
!------------------------------------------------------------------
!
      end module m_surface_mesh_4_merge
