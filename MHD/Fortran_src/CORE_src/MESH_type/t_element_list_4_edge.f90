!>@file   t_element_list_4_edge.f90
!!@brief  module t_element_list_4_edge
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Structure of edge geometry data
!!
!!@verbatim
!!      subroutine const_element_list_4_edge(ele, edge, ele_4_edge)
!!      subroutine const_surface_list_4_edge(surf, edge, ele_4_edge)
!!
!!      subroutine dealloc_ele_4_edge_item(ele_4_edge)
!!      subroutine dealloc_surf_4_edge_item(ele_4_edge)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(element_list_4_edge), intent(inout) :: ele_4_edge
!!@endverbatim
!
      module t_element_list_4_edge
!
      use m_precision
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!>     Element and surface list for edge
      type element_list_4_edge
!>   total number of element list for edge
        integer(kind=kint) :: ntot_iele_4_edge
!>   number of element list for each edge
        integer(kind=kint), allocatable :: num_iele_4_edge(:)
!>   end address of element list for each edge
        integer(kind=kint), allocatable :: istack_iele_4_edge(:)
!>   element id list for each edge (negative: opposite direction)
        integer(kind=kint), allocatable  :: iele_4_edge(:,:)
!
!>   total number of surface list for edge
        integer(kind=kint) :: ntot_isurf_4_edge
!>   number of surface list for each edge
        integer(kind=kint), allocatable :: num_isurf_4_edge(:)
!>   end address of surface list for each edge
        integer(kind=kint), allocatable :: istack_isurf_4_edge(:)
!>   surafce id list for each edge (negative: opposite direction)
        integer(kind=kint), allocatable  :: isurf_4_edge(:,:)
      end type element_list_4_edge
!
      private :: alloc_ele_4_edge_num, alloc_ele_4_edge_item
      private :: alloc_surf_4_edge_num, alloc_surf_4_edge_item
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_element_list_4_edge(ele, edge, ele_4_edge)
!
      use set_element_list_4_surface
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(element_list_4_edge), intent(inout) :: ele_4_edge
!
!
      call alloc_ele_4_edge_num(edge, ele_4_edge)
      call count_ele_list_4_edge(ele%numele, edge%numedge, nedge_4_ele, &
     &    edge%iedge_4_ele, ele_4_edge%ntot_iele_4_edge,                &
     &    ele_4_edge%num_iele_4_edge, ele_4_edge%istack_iele_4_edge)
!
      call alloc_ele_4_edge_item(edge, ele_4_edge)
      call set_ele_list_4_edge(ele%numele, edge%numedge, nedge_4_ele,   &
     &    edge%iedge_4_ele, ele_4_edge%ntot_iele_4_edge,                &
     &    ele_4_edge%num_iele_4_edge, ele_4_edge%istack_iele_4_edge,    &
     &    ele_4_edge%iele_4_edge)
!
      end subroutine const_element_list_4_edge
!
!------------------------------------------------------------------
!
      subroutine const_surface_list_4_edge(surf, edge, ele_4_edge)
!
      use set_element_list_4_surface
!
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(element_list_4_edge), intent(inout) :: ele_4_edge
!
!
      call alloc_surf_4_edge_num(edge, ele_4_edge)
      call count_ele_list_4_edge(surf%numsurf, edge%numedge,            &
     &    nedge_4_surf, edge%iedge_4_sf, ele_4_edge%ntot_isurf_4_edge,  &
     &    ele_4_edge%num_isurf_4_edge, ele_4_edge%istack_isurf_4_edge)
!
      call alloc_surf_4_edge_item(edge, ele_4_edge)
      call set_ele_list_4_edge(surf%numsurf, edge%numedge,              &
     &    nedge_4_surf, edge%iedge_4_sf, ele_4_edge%ntot_isurf_4_edge,  &
     &    ele_4_edge%num_isurf_4_edge, ele_4_edge%istack_isurf_4_edge,  &
     &    ele_4_edge%isurf_4_edge)
!
      end subroutine const_surface_list_4_edge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ele_4_edge_item(ele_4_edge)
!
      type(element_list_4_edge), intent(inout) :: ele_4_edge
!
!
      deallocate( ele_4_edge%iele_4_edge )
      deallocate( ele_4_edge%num_iele_4_edge)
      deallocate( ele_4_edge%istack_iele_4_edge)
!
      end subroutine dealloc_ele_4_edge_item
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surf_4_edge_item(ele_4_edge)
!
      type(element_list_4_edge), intent(inout) :: ele_4_edge
!
!
      deallocate( ele_4_edge%isurf_4_edge )
      deallocate( ele_4_edge%num_isurf_4_edge)
      deallocate( ele_4_edge%istack_isurf_4_edge)
!
      end subroutine dealloc_surf_4_edge_item
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_ele_4_edge_num(edge, ele_4_edge)
!
      type(edge_data), intent(in) :: edge
      type(element_list_4_edge), intent(inout) :: ele_4_edge
!
!
      allocate( ele_4_edge%num_iele_4_edge(edge%numedge) )
      allocate( ele_4_edge%istack_iele_4_edge(0:edge%numedge) )
      if (edge%numedge .gt. 0) ele_4_edge%num_iele_4_edge =    0
      ele_4_edge%istack_iele_4_edge = 0
!
      end subroutine alloc_ele_4_edge_num
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ele_4_edge_item(edge, ele_4_edge)
!
      type(edge_data), intent(in) :: edge
      type(element_list_4_edge), intent(inout) :: ele_4_edge
!
!
      ele_4_edge%ntot_iele_4_edge                                       &
     &               = ele_4_edge%istack_iele_4_edge(edge%numedge)
      allocate(ele_4_edge%iele_4_edge(ele_4_edge%ntot_iele_4_edge,2))
!
      if(ele_4_edge%ntot_iele_4_edge .le. 0) return
      ele_4_edge%iele_4_edge = 0
!
      end subroutine alloc_ele_4_edge_item
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_4_edge_num(edge, ele_4_edge)
!
      type(edge_data), intent(in) :: edge
      type(element_list_4_edge), intent(inout) :: ele_4_edge
!
!
      allocate( ele_4_edge%num_isurf_4_edge(edge%numedge) )
      allocate( ele_4_edge%istack_isurf_4_edge(0:edge%numedge) )
      if(edge%numedge .gt. 0) ele_4_edge%num_isurf_4_edge =    0
      ele_4_edge%istack_isurf_4_edge = 0
!
      end subroutine alloc_surf_4_edge_num
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_4_edge_item(edge, ele_4_edge)
!
      type(edge_data), intent(in) :: edge
      type(element_list_4_edge), intent(inout) :: ele_4_edge
!
!
      ele_4_edge%ntot_isurf_4_edge                                      &
     &              = ele_4_edge%istack_isurf_4_edge(edge%numedge)
      allocate(ele_4_edge%isurf_4_edge(ele_4_edge%ntot_isurf_4_edge,2))
!
      if(ele_4_edge%ntot_isurf_4_edge .le. 0) return
      ele_4_edge%isurf_4_edge = 0
!
      end subroutine alloc_surf_4_edge_item
!
!  ---------------------------------------------------------------------
!
      end module t_element_list_4_edge
