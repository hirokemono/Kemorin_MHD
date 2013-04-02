!set_tables_4_surf_grp_type.f90
!     module set_tables_4_surf_grp_type
!
!     Writteg by H.Matsui on Dec., 2008
!
!      subroutine set_surf_id_4_surf_grp_type(ele, surf,                &
!     &          surf_grp, sf_grp_tbl)
!        type(element_data),       intent(in) :: ele
!        type(surface_data),       intent(in) :: surf
!        type(surface_group_data), intent(in) :: surf_grp
!        type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!      subroutine set_edge_4_surf_grp_type(surf, edge, surf_grp,        &
!     &          sf_grp_tbl)
!        type(surface_data),        intent(in) :: surf
!        type(edge_data),           intent(in) :: edge
!        type(surface_group_data), intent(in) :: surf_grp
!        type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!      subroutine set_node_4_surf_grp_type(surf, nod, surf_grp,         &
!     &          sf_grp_tbl)
!        type(surface_data),        intent(in) :: surf
!        type(node_data),           intent(in) :: nod
!        type (surface_group_data), intent(in) :: surf_grp
!        type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!      subroutine empty_sf_ed_nod_surf_grp_type(surf_grp, sf_grp_tbl)
!        type(surface_group_data), intent(in) :: surf_grp
!        type(surface_group_table), intent(inout) :: sf_grp_tbl
!
      module set_tables_4_surf_grp_type
!
      use m_precision
!
      use t_geometry_data
      use t_group_data
      use t_group_connects
      use t_surface_data
!
      use set_node_4_group
!
      implicit none
!
      integer(kind=kint), allocatable :: imark_surf_grp(:)
!
      private :: imark_surf_grp
      private :: allocate_imark_sf_grp
      private :: deallocate_imark_sf_grp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_surf_grp_type(ele, surf,                 &
     &          surf_grp, sf_grp_tbl)
!
      use set_surface_id_4_surf_grp
!
      type(element_data),       intent(in) :: ele
      type(surface_data),       intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call alloc_surf_item_sf_grp_type(surf_grp%num_item, sf_grp_tbl)
!
      call set_surface_id_4_surf_group(ele%numele, surf%isf_4_ele,      &
     &    surf_grp%num_grp, surf_grp%num_item,                          &
     &    surf_grp%istack_grp, surf_grp%item_sf_grp,                    &
     &    sf_grp_tbl%isurf_grp, sf_grp_tbl%isurf_grp_n)
!
      end subroutine set_surf_id_4_surf_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_surf_grp_type(surf, edge, surf_grp,         &
     &          sf_grp_tbl)
!
      use m_geometry_constants
      use t_edge_data
!
      type(surface_data),        intent(in) :: surf
      type(edge_data),           intent(in) :: edge
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call allocate_imark_sf_grp(edge%numedge)
      call alloc_num_other_grp(surf_grp%num_grp, sf_grp_tbl%edge)
!
      call count_nod_4_ele_grp(edge%numedge, surf%numsurf,              &
     &    nedge_4_surf, edge%iedge_4_sf,                                &
     &    surf_grp%num_grp, surf_grp%num_item,                          &
     &    surf_grp%istack_grp, sf_grp_tbl%isurf_grp,                    &
     &    sf_grp_tbl%edge%ntot_e_grp, sf_grp_tbl%edge%nitem_e_grp,      &
     &    sf_grp_tbl%edge%istack_e_grp, imark_surf_grp)
!
      call alloc_item_other_grp(sf_grp_tbl%edge)
!
      call set_nod_4_ele_grp(edge%numedge, surf%numsurf,                &
     &    nedge_4_surf, edge%iedge_4_sf,                                &
     &    surf_grp%num_grp, surf_grp%num_item,                          &
     &    surf_grp%istack_grp, sf_grp_tbl%isurf_grp,                    &
     &    sf_grp_tbl%edge%ntot_e_grp, sf_grp_tbl%edge%nitem_e_grp,      &
     &    sf_grp_tbl%edge%istack_e_grp, sf_grp_tbl%edge%item_e_grp,     &
     &    imark_surf_grp)
!
      call deallocate_imark_sf_grp
!
      end subroutine set_edge_4_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_surf_grp_type(surf, nod, surf_grp,          &
     &          sf_grp_tbl)
!
      type(surface_data),        intent(in) :: surf
      type(node_data),           intent(in) :: nod
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call allocate_imark_sf_grp(nod%numnod)
!
      call alloc_num_other_grp(surf_grp%num_grp, sf_grp_tbl%node)
!
      call count_nod_4_ele_grp(nod%numnod, surf%numsurf,                &
     &    surf%nnod_4_surf, surf%ie_surf,                               &
     &    surf_grp%num_grp, surf_grp%num_item,                          &
     &    surf_grp%istack_grp, sf_grp_tbl%isurf_grp,                    &
     &    sf_grp_tbl%node%ntot_e_grp, sf_grp_tbl%node%nitem_e_grp,      &
     &    sf_grp_tbl%node%istack_e_grp, imark_surf_grp)
!
!
      call alloc_item_other_grp(sf_grp_tbl%node)
!
!
      call set_nod_4_ele_grp(nod%numnod, surf%numsurf,                  &
     &    surf%nnod_4_surf, surf%ie_surf,                               &
     &    surf_grp%num_grp, surf_grp%num_item,                          &
     &    surf_grp%istack_grp, sf_grp_tbl%isurf_grp,                    &
     &    sf_grp_tbl%node%ntot_e_grp, sf_grp_tbl%node%nitem_e_grp,      &
     &    sf_grp_tbl%node%istack_e_grp, sf_grp_tbl%node%item_e_grp,     &
     &    imark_surf_grp)
!
      call deallocate_imark_sf_grp
!
      end subroutine set_node_4_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_sf_ed_nod_surf_grp_type(surf_grp, sf_grp_tbl)
!
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_table), intent(inout) :: sf_grp_tbl
!
!
      call alloc_num_other_grp(surf_grp%num_grp, sf_grp_tbl%edge)
      call alloc_num_other_grp(surf_grp%num_grp, sf_grp_tbl%node)
!
      sf_grp_tbl%edge%ntot_e_grp = 0
      sf_grp_tbl%node%ntot_e_grp = 0
      call alloc_surf_item_sf_grp_type(surf_grp%num_item, sf_grp_tbl)
      call alloc_item_other_grp(sf_grp_tbl%edge)
      call alloc_item_other_grp(sf_grp_tbl%node)
!
      end subroutine empty_sf_ed_nod_surf_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_imark_sf_grp(num)
!
      integer(kind = kint), intent(in) :: num
!
      allocate( imark_surf_grp(num) )
      imark_surf_grp = 0
!
      end subroutine allocate_imark_sf_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_imark_sf_grp
!
      deallocate(imark_surf_grp)
!
      end subroutine deallocate_imark_sf_grp
!
!-----------------------------------------------------------------------
!
      end module set_tables_4_surf_grp_type
