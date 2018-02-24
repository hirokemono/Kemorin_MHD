!>@file   t_surface_mesh_4_merge.f90
!!@brief  module t_surface_mesh_4_merge
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Structure of surface information for pickup surface
!!
!!@verbatim
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
!
      implicit none
!
!
      type viewer_group_data
        integer(kind = kint) :: num_item
        integer(kind = kint), allocatable :: istack_sf(:)
        integer(kind = kint), allocatable :: item_sf(:)
      end type viewer_group_data
!
      type viewer_mesh_data
        integer(kind = kint)  :: num_pe_sf
!
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
      type viewer_groups
        type(viewer_group_data) :: domain_surf_grp
        type(viewer_group_data) :: domain_edge_grp
        type(viewer_group_data) :: domain_nod_grp
!
        character(len=kchara), allocatable :: nod_gp_name_sf(:)
        integer(kind = kint) :: ngrp_nod_sf
        type(viewer_group_data) :: nod_nod_grp
!
        integer(kind = kint) :: ngrp_ele_sf
        character(len=kchara), allocatable :: ele_gp_name_sf(:)
        type(viewer_group_data) :: ele_surf_grp
        type(viewer_group_data) :: ele_edge_grp
        type(viewer_group_data) :: ele_nod_grp
!
        integer(kind = kint) :: ngrp_surf_sf
        character(len=kchara), allocatable :: surf_gp_name_sf(:)
        type(viewer_group_data) :: sf_surf_grp
        type(viewer_group_data) :: sf_edge_grp
        type(viewer_group_data) :: sf_nod_grp
      end type viewer_groups
!
!
!domain_surf_grp%num_item
!        integer(kind = kint) :: nsurf_domain_sf
!domain_surf_grp%istack_sf
!        integer(kind = kint), allocatable :: isurf_stack_domain_sf(:)
!domain_surf_grp%item_sf
!        integer(kind = kint), allocatable :: isurf_domain_sf(:)
!domain_edge_grp%num_item
!        integer(kind = kint) :: nedge_domain_sf
!domain_edge_grp%istack_sf
!        integer(kind = kint), allocatable :: edge_stack_domain_sf(:)
!domain_edge_grp%item_sf
!        integer(kind = kint), allocatable :: edge_item_domain_sf(:)
!domain_nod_grp%num_item
!        integer(kind = kint) :: nnod_domain_sf
!domain_nod_grp%istack_sf
!        integer(kind = kint), allocatable :: nod_stack_domain_sf(:)
!domain_nod_grp%item_sf
!        integer(kind = kint), allocatable :: nod_item_domain_sf(:)
!
!
!nod_nod_grp%num_item
!        integer(kind = kint) :: nnod_nod_sf
!nod_nod_grp%istack_sf
!        integer(kind = kint), allocatable :: nod_stack_sf(:)
!nod_nod_grp%item_sf
!        integer(kind = kint), allocatable :: nod_item_sf(:)
!
!ele_surf_grp%num_item
!        integer(kind = kint) :: nele_ele_sf
!ele_surf_grp%istack_sf
!        integer(kind = kint), allocatable :: ele_stack_sf(:)
!ele_surf_grp%item_sf
!        integer(kind = kint), allocatable :: ele_item_sf(:)
!ele_edge_grp%num_item
!        integer(kind = kint) :: nedge_ele_sf
!ele_edge_grp%istack_sf
!        integer(kind = kint), allocatable :: ele_edge_stack_sf(:)
!ele_edge_grp%item_sf
!        integer(kind = kint), allocatable :: ele_edge_item_sf(:)
!ele_nod_grp%num_item
!        integer(kind = kint) :: nnod_ele_sf
!ele_nod_grp%istack_sf
!        integer(kind = kint), allocatable :: ele_nod_stack_sf(:)
!ele_nod_grp%item_sf
!        integer(kind = kint), allocatable :: ele_nod_item_sf(:)
!
!sf_surf_grp%num_item
!        integer(kind = kint) :: nsurf_surf_sf
!sf_surf_grp%istack_sf
!        integer(kind = kint), allocatable :: surf_stack_sf(:)
!sf_surf_grp%item_sf
!        integer(kind = kint), allocatable :: surf_item_sf(:)
!
!sf_edge_grp%num_item
!        integer(kind = kint) :: nedge_surf_sf
!sf_edge_grp%istack_sf
!        integer(kind = kint), allocatable :: surf_edge_stack_sf(:)
!sf_edge_grp%item_sf
!        integer(kind = kint), allocatable :: surf_edge_item_sf(:)
!
!sf_nod_grp%num_item
!        integer(kind = kint) :: nnod_surf_sf
!sf_nod_grp%istack_sf
!        integer(kind = kint), allocatable :: surf_nod_stack_sf(:)
!sf_nod_grp%item_sf
!        integer(kind = kint), allocatable :: surf_nod_item_sf(:)
!
!------------------------------------------------------------------
!
      contains
!
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
