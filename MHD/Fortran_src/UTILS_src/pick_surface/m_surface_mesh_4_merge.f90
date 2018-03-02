!
!      module m_surface_mesh_4_merge
!
!      Written by Kemorin
!
!      subroutine allocate_num_mesh_sf
!      subroutine check_edge_connent_viewer(nnod_4_edge)
!
      module m_surface_mesh_4_merge
!
      use m_precision
      use m_constants
      use t_surface_mesh_4_merge
!
      implicit none
!
!
        integer(kind = kint)  :: num_pe_sf
!
        integer(kind = kint), allocatable :: inod_sf_stack(:)
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
      allocate( inod_sf_stack(0:num_pe)  )
      allocate( isurf_sf_stack(0:num_pe) )
      allocate( iedge_sf_stack(0:num_pe) )
      inod_sf_stack  = 0
      isurf_sf_stack = 0
      iedge_sf_stack = 0
!
      end subroutine allocate_num_mesh_sf
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_edge_connent_viewer(nnod_4_edge)
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      integer(kind = kint) :: i
!
!
      write(50,*) 'edgepetot_viewer', view_mesh%edgepetot_viewer
      write(50,*) 'iedge_sf_stack', iedge_sf_stack
      write(50,*) 'ie_edge_viewer'
      do i = 1, view_mesh%edgepetot_viewer
        write(50,*) i, view_mesh%ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      end subroutine check_edge_connent_viewer
!
!------------------------------------------------------------------
!
      end module m_surface_mesh_4_merge
