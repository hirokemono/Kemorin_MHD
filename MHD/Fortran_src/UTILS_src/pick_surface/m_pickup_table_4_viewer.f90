!
!      module m_pickup_table_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine allocate_imark_surf(merged_surf)
!!      subroutine allocate_imark_node(numnod)
!!      subroutine allocate_ele_edge_item_tmp(ele_edge_grp)
!!        type(viewer_group_data), intent(in) :: ele_edge_grp
!!      subroutine allocate_sf_edge_item_tmp(sf_edge_grp)
!!        type(viewer_group_data), intent(in)  :: sf_edge_grp
!!      subroutine allocate_sf_cvt_table_viewer(merged_surf)
!!        type(mesh_geometry), intent(in) :: merged
!!        type(surface_data), intent(in) :: merged_surf
!!
!!      subroutine deallocate_imark_surf
!!      subroutine deallocate_imark_node
!!      subroutine deallocate_sf_cvt_table_viewer
!!      subroutine deallocate_ele_edge_item_tmp
!!      subroutine deallocate_sf_edge_item_tmp
!
      module m_pickup_table_4_viewer
!
      use m_precision
      use t_surface_data
!
      implicit none
!
!
      integer(kind = kint), allocatable :: imark_surf(:)
      integer(kind = kint), allocatable :: imark_edge(:)
      integer(kind = kint), allocatable :: imark_node(:)
!
      integer(kind = kint), allocatable :: isf_merge2viewer(:)
      integer(kind = kint), allocatable :: isf_viewer2merge(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_imark_surf(merged_surf)
!
      type(surface_data), intent(in) :: merged_surf
!
      allocate( imark_surf(merged_surf%numsurf) )
      imark_surf = 0
!
      end subroutine allocate_imark_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_imark_node(numnod)
!
      integer(kind = kint) :: numnod
!
      allocate( imark_node(numnod) )
      imark_node = 0
!
      end subroutine allocate_imark_node
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_imark_surf
!
      deallocate( imark_surf )
!
      end subroutine deallocate_imark_surf
!
!------------------------------------------------------------------
!
      subroutine deallocate_imark_node
!
      deallocate( imark_node )
!
      end subroutine deallocate_imark_node
!
!------------------------------------------------------------------
!
      subroutine deallocate_sf_cvt_table_viewer
!
      deallocate( isf_merge2viewer )
      deallocate( isf_viewer2merge )
!
      end subroutine deallocate_sf_cvt_table_viewer
!
!------------------------------------------------------------------
!
      end module m_pickup_table_4_viewer
