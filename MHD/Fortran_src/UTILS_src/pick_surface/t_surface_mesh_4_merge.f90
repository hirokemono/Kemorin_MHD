!>@file   t_surface_mesh_4_merge.f90
!!@brief  module t_surface_mesh_4_merge
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Structure of surface information for pickup surface
!!
!!@verbatim
!!      subroutine alloc_nod_position_viewer(view_mesh)
!!      subroutine alloc_surf_type_viewer(view_mesh)
!!      subroutine alloc_edge_type_viewer(view_mesh)
!!      subroutine alloc_surf_connect_viewer(nnod_4_surf, view_mesh)
!!      subroutine dealloc_nod_position_viewer(view_mesh)
!!      subroutine dealloc_surf_type_viewer(view_mesh)
!!      subroutine dealloc_edge_type_viewer(view_mesh)
!!      subroutine dealloc_surf_connect_viewer(view_mesh)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!
!!      subroutine alloc_viewer_node_grps_stack(num_pe, view_nod_grps)
!!      subroutine dealloc_viewer_node_grps_stack(view_nod_grps)
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!!      subroutine alloc_viewer_surf_grps_stack(num_pe, view_grps)
!!      subroutine dealloc_viewer_surf_grps_stack(view_grps)
!!        type(viewer_surface_groups), intent(inout) :: view_grps
!!
!!      subroutine dealloc_nod_position_viewer(view_mesh)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!
!!      subroutine alloc_merged_group_stack(num_pe, ngrp, group)
!!      subroutine alloc_merged_group_item(group)
!!      subroutine dealloc_merged_group_stack(group)
!!      subroutine dealloc_merged_group_item(group)
!!        type(viewer_group_data), intent(inout) :: group
!!@endverbatim
!
      module t_surface_mesh_4_merge
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
!
      integer (kind = kint), parameter :: surface_id = 15
!
      type viewer_group_data
        integer(kind = kint) :: num_item
        integer(kind = kint), allocatable :: istack_sf(:)
        integer(kind = kint), allocatable :: item_sf(:)
      end type viewer_group_data
!
      type viewer_node_groups
        integer(kind = kint) :: num_grp
        character(len=kchara), allocatable :: grp_name(:)
        type(viewer_group_data) :: node_grp
      end type viewer_node_groups
!
      type viewer_surface_groups
        integer(kind = kint) :: num_grp
        character(len=kchara), allocatable :: grp_name(:)
        type(viewer_group_data) :: surf_grp
        type(viewer_group_data) :: edge_grp
        type(viewer_group_data) :: node_grp
      end type viewer_surface_groups
!
!
      type viewer_mesh_data
        integer(kind = kint), allocatable :: inod_sf_stack(:)
        integer(kind = kint), allocatable :: iedge_sf_stack(:)
        integer(kind = kint), allocatable :: isurf_sf_stack(:)
!
        integer(kind = kint)  ::  surfpetot_viewer
        integer(kind = kint)  ::  edgepetot_viewer
        integer(kind = kint)  ::  nodpetot_viewer
!
        integer(kind = kint), allocatable  ::  ie_sf_viewer(:,:)
        integer(kind = kint), allocatable  ::  ie_edge_viewer(:,:)
        integer(kind = kint), allocatable  ::  surftyp_viewer(:  )
        integer(kind = kint), allocatable  ::  edgetyp_viewer(:  )
        integer(kind = kint), allocatable  ::  iedge_sf_viewer(:,:)
!
        real   (kind=kreal), dimension(:,:), allocatable  ::  xx_view
      end type viewer_mesh_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_nod_position_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      allocate( view_mesh%xx_view(view_mesh%nodpetot_viewer,3) )
      if(view_mesh%nodpetot_viewer .gt. 0) view_mesh%xx_view = 0.0d0
!
      end subroutine alloc_nod_position_viewer
!
!------------------------------------------------------------------
!
      subroutine alloc_surf_type_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      allocate(view_mesh%surftyp_viewer(view_mesh%surfpetot_viewer))
      if(view_mesh%surfpetot_viewer .gt. 0) then
        view_mesh%surftyp_viewer = 0
      end if
!
      end subroutine alloc_surf_type_viewer
!
!------------------------------------------------------------------
!
      subroutine alloc_edge_type_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      allocate(view_mesh%edgetyp_viewer(view_mesh%edgepetot_viewer))
      if(view_mesh%edgepetot_viewer .gt. 0) then
        view_mesh%edgetyp_viewer = 0
      end if
!
      end subroutine alloc_edge_type_viewer
!
!------------------------------------------------------------------
!
      subroutine alloc_surf_connect_viewer(nnod_4_surf, view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind  = kint) :: num
!
      num = view_mesh%surfpetot_viewer
      allocate( view_mesh%ie_sf_viewer(num,nnod_4_surf) )
      if(num .gt. 0) view_mesh%ie_sf_viewer = 0
!
      end subroutine alloc_surf_connect_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_nod_position_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      deallocate( view_mesh%xx_view )
!
      end subroutine dealloc_nod_position_viewer
!
!------------------------------------------------------------------
!
      subroutine dealloc_surf_type_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      deallocate( view_mesh%surftyp_viewer )
!
      end subroutine dealloc_surf_type_viewer
!
!------------------------------------------------------------------
!
      subroutine dealloc_edge_type_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      deallocate(view_mesh%edgetyp_viewer)
!
      end subroutine dealloc_edge_type_viewer
!
!------------------------------------------------------------------
!
      subroutine dealloc_surf_connect_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      deallocate( view_mesh%ie_sf_viewer )
!
      end subroutine dealloc_surf_connect_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_viewer_node_grps_stack(num_pe, view_nod_grps)
!
      integer(kind = kint), intent(in) :: num_pe
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
!
      allocate(view_nod_grps%grp_name(view_nod_grps%num_grp))
!
      call alloc_merged_group_stack                                     &
     &   (num_pe, view_nod_grps%num_grp, view_nod_grps%node_grp)
!
      end subroutine alloc_viewer_node_grps_stack
!
!------------------------------------------------------------------
!
      subroutine alloc_viewer_surf_grps_stack(num_pe, view_grps)
!
      integer(kind = kint), intent(in) :: num_pe
      type(viewer_surface_groups), intent(inout) :: view_grps
!
!
      allocate(view_grps%grp_name(view_grps%num_grp))
!
      call alloc_merged_group_stack                                     &
     &   (num_pe, view_grps%num_grp, view_grps%surf_grp)
      call alloc_merged_group_stack                                     &
     &   (num_pe, view_grps%num_grp, view_grps%edge_grp)
      call alloc_merged_group_stack                                     &
     &   (num_pe, view_grps%num_grp, view_grps%node_grp)
!
      end subroutine alloc_viewer_surf_grps_stack
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_viewer_node_grps_stack(view_nod_grps)
!
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
!
      deallocate(view_nod_grps%grp_name)
      call dealloc_merged_group_stack(view_nod_grps%node_grp)
!
      end subroutine dealloc_viewer_node_grps_stack
!
!------------------------------------------------------------------
!
      subroutine dealloc_viewer_surf_grps_stack(view_grps)
!
      type(viewer_surface_groups), intent(inout) :: view_grps
!
!
      deallocate(view_grps%grp_name)
!
      call dealloc_merged_group_stack(view_grps%surf_grp)
      call dealloc_merged_group_stack(view_grps%edge_grp)
      call dealloc_merged_group_stack(view_grps%node_grp)
!
      end subroutine dealloc_viewer_surf_grps_stack
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_merged_group_stack(num_pe, ngrp, group)
!
      integer(kind = kint), intent(in) :: num_pe, ngrp
      type(viewer_group_data), intent(inout) :: group
!
!
      allocate( group%istack_sf(0:num_pe*ngrp)  )
      group%istack_sf = 0
!
      end subroutine alloc_merged_group_stack
!
!------------------------------------------------------------------
!
      subroutine alloc_merged_group_item(group)
!
      type(viewer_group_data), intent(inout) :: group
!
!
      allocate( group%item_sf(group%num_item)  )
      if(group%num_item .gt. 0) group%item_sf = 0
!
      end subroutine alloc_merged_group_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_merged_group_stack(group)
!
      type(viewer_group_data), intent(inout) :: group
!
!
      deallocate(group%istack_sf)
!
      end subroutine dealloc_merged_group_stack
!
!------------------------------------------------------------------
!
      subroutine dealloc_merged_group_item(group)
!
      type(viewer_group_data), intent(inout) :: group
!
      deallocate(group%item_sf)
!
      end subroutine dealloc_merged_group_item
!
!------------------------------------------------------------------
!
      end module t_surface_mesh_4_merge
