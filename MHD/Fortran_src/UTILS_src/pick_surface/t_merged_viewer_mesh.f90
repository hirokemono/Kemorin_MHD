!
!      module t_merged_viewer_mesh
!
!      Written by Kemorin
!
!!      subroutine alloc_num_mesh_sf(num_pe, mgd_view_mesh)
!!      subroutine dealloc_num_mesh_sf(mgd_view_mesh)
!!      subroutine num_merged_viewer_nod_surf_edge(mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!      subroutine check_edge_connent_viewer(nnod_4_edge, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      module t_merged_viewer_mesh
!
      use m_precision
      use m_constants
      use t_viewer_mesh
!
      implicit none
!
!
      type merged_viewer_mesh
        character (len = kchara) :: surface_file_head = 'in_surface'
!
        integer(kind = kint)  :: num_pe_sf
!
        integer(kind = kint), allocatable :: inod_sf_stack(:)
        integer(kind = kint), allocatable :: iedge_sf_stack(:)
        integer(kind = kint), allocatable :: isurf_sf_stack(:)
!
        type(viewer_mesh_data) :: view_mesh
!
        type(viewer_surface_groups) :: domain_grps
!
        type(viewer_node_groups) :: view_nod_grps
        type(viewer_surface_groups) :: view_ele_grps
        type(viewer_surface_groups) :: view_sf_grps
      end type merged_viewer_mesh
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_num_mesh_sf(num_pe, mgd_view_mesh)
!
      integer(kind = kint), intent(in) :: num_pe
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
      mgd_view_mesh%num_pe_sf = num_pe
!
      allocate(mgd_view_mesh%inod_sf_stack(0:mgd_view_mesh%num_pe_sf) )
      allocate(mgd_view_mesh%isurf_sf_stack(0:mgd_view_mesh%num_pe_sf))
      allocate(mgd_view_mesh%iedge_sf_stack(0:mgd_view_mesh%num_pe_sf))
      mgd_view_mesh%inod_sf_stack  = 0
      mgd_view_mesh%isurf_sf_stack = 0
      mgd_view_mesh%iedge_sf_stack = 0
!
      end subroutine alloc_num_mesh_sf
!
!------------------------------------------------------------------
!
      subroutine dealloc_num_mesh_sf(mgd_view_mesh)
!
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
      deallocate( mgd_view_mesh%inod_sf_stack  )
      deallocate( mgd_view_mesh%isurf_sf_stack )
      deallocate( mgd_view_mesh%iedge_sf_stack )
!
      end subroutine dealloc_num_mesh_sf
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine num_merged_viewer_nod_surf_edge(mgd_view_mesh)
!
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
      mgd_view_mesh%view_mesh%nodpetot_viewer                           &
     &   = mgd_view_mesh%inod_sf_stack(mgd_view_mesh%num_pe_sf)
      mgd_view_mesh%view_mesh%surfpetot_viewer                          &
     &   = mgd_view_mesh%isurf_sf_stack(mgd_view_mesh%num_pe_sf)
      mgd_view_mesh%view_mesh%edgepetot_viewer                          &
     &   = mgd_view_mesh%iedge_sf_stack(mgd_view_mesh%num_pe_sf)
!
      end subroutine num_merged_viewer_nod_surf_edge
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_edge_connent_viewer(nnod_4_edge, mgd_view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      integer(kind = kint) :: i
!
!
      write(50,*) 'edgepetot_viewer',                                   &
     &           mgd_view_mesh%view_mesh%edgepetot_viewer
      write(50,*) 'iedge_sf_stack', mgd_view_mesh%iedge_sf_stack
      write(50,*) 'ie_edge_viewer'
      do i = 1, mgd_view_mesh%view_mesh%edgepetot_viewer
        write(50,*)                                                     &
     &    i, mgd_view_mesh%view_mesh%ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      end subroutine check_edge_connent_viewer
!
!------------------------------------------------------------------
!
      end module t_merged_viewer_mesh
