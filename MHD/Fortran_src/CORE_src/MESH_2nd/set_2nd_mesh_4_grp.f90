!
!     module set_2nd_mesh_4_grp
!
!     Writteg by H.Matsui on Aug., 2006
!     Modified by H.Matsui on Dec., 2008
!
!      subroutine set_surf_4_ele_grp_2nd
!      subroutine set_edge_4_ele_grp_2nd
!      subroutine set_node_4_ele_grp_2nd
!
!      subroutine set_edge_4_surf_grp_2nd
!      subroutine set_node_4_surf_grp_2nd
!
!      subroutine set_surf_id_4_surf_grp_2nd
!
      module set_2nd_mesh_4_grp
!
      use m_precision
!
      use m_geometry_parameter
      use m_2nd_geometry_data
      use m_2nd_group_data
!
      use set_node_4_group
!
      implicit  none
!
      integer(kind=kint), allocatable :: imark_4_grp(:)
      private :: imark_4_grp
      private :: allocate_imark_4_grp, deallocate_imark_4_grp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_4_ele_grp_2nd
!
      use m_geometry_constants
!
!
      call allocate_imark_4_grp(surf_2nd%numsurf)
      call alloc_num_other_grp(num_mat_2nd, ele_grp_tbl_2nd%surf)
!
      call count_nod_4_ele_grp(surf_2nd%numsurf, ele_2nd%numele, nsurf_4_ele, &
     &    surf_2nd%isf_4_ele, num_mat_2nd, num_mat_bc_2nd,              &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ele_grp_tbl_2nd%surf%ntot_e_grp, ele_grp_tbl_2nd%surf%nitem_e_grp, &
     &    ele_grp_tbl_2nd%surf%istack_e_grp, imark_4_grp)
!
      call alloc_item_other_grp(ele_grp_tbl_2nd%surf)
!
      call set_nod_4_ele_grp(surf_2nd%numsurf, ele_2nd%numele, nsurf_4_ele, &
     &    surf_2nd%isf_4_ele, num_mat_2nd, num_mat_bc_2nd,              &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ele_grp_tbl_2nd%surf%ntot_e_grp, ele_grp_tbl_2nd%surf%nitem_e_grp, &
     &    ele_grp_tbl_2nd%surf%istack_e_grp, ele_grp_tbl_2nd%surf%item_e_grp,  &
     &    imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_surf_4_ele_grp_2nd
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_ele_grp_2nd
!
      use m_geometry_constants
!
!
      call allocate_imark_4_grp(edge_2nd%numedge)
      call alloc_num_other_grp(num_mat_2nd, ele_grp_tbl_2nd%edge)
!
      call count_nod_4_ele_grp(edge_2nd%numedge, ele_2nd%numele, nedge_4_ele, &
     &    edge_2nd%iedge_4_ele, num_mat_2nd, num_mat_bc_2nd,            &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ele_grp_tbl_2nd%edge%ntot_e_grp, ele_grp_tbl_2nd%edge%nitem_e_grp,  &
     &    ele_grp_tbl_2nd%edge%istack_e_grp, imark_4_grp)
!
      call alloc_item_other_grp(ele_grp_tbl_2nd%edge)
!
      call set_nod_4_ele_grp(edge_2nd%numedge, ele_2nd%numele, nedge_4_ele,   &
     &    edge_2nd%iedge_4_ele, num_mat_2nd, num_mat_bc_2nd,            &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ele_grp_tbl_2nd%edge%ntot_e_grp, ele_grp_tbl_2nd%edge%nitem_e_grp,  &
     &    ele_grp_tbl_2nd%edge%istack_e_grp, ele_grp_tbl_2nd%edge%item_e_grp,  &
     &    imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_edge_4_ele_grp_2nd
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_ele_grp_2nd
!
!
      call allocate_imark_4_grp(node_2nd%numnod)
      call alloc_num_other_grp(num_mat_2nd, ele_grp_tbl_2nd%node)
!
      call count_nod_4_ele_grp(node_2nd%numnod, ele_2nd%numele,  &
     &    ele_2nd%nnod_4_ele, ele_2nd%ie, num_mat_2nd, num_mat_bc_2nd,  &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ele_grp_tbl_2nd%node%ntot_e_grp, ele_grp_tbl_2nd%node%nitem_e_grp, &
     &    ele_grp_tbl_2nd%node%istack_e_grp, imark_4_grp)
!
      call alloc_item_other_grp(ele_grp_tbl_2nd%node)
!
      call set_nod_4_ele_grp(node_2nd%numnod, ele_2nd%numele, &
     &    ele_2nd%nnod_4_ele, ele_2nd%ie, num_mat_2nd, num_mat_bc_2nd,  &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ele_grp_tbl_2nd%node%ntot_e_grp, ele_grp_tbl_2nd%node%nitem_e_grp, &
     &    ele_grp_tbl_2nd%node%istack_e_grp, ele_grp_tbl_2nd%node%item_e_grp,  &
     &    imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_node_4_ele_grp_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_surf_grp_2nd
!
      use m_geometry_constants
!
!
      call allocate_imark_4_grp(edge_2nd%numedge)
      call alloc_num_other_grp(num_surf_2nd, sf_grp_tbl_2nd%edge)
!
      call count_nod_4_ele_grp(edge_2nd%numedge, surf_2nd%numsurf,             &
     &    nedge_4_surf, edge_2nd%iedge_4_sf, num_surf_2nd, num_surf_bc_2nd,  &
     &    surf_istack_2nd, sf_grp_tbl_2nd%isurf_grp, sf_grp_tbl_2nd%edge%ntot_e_grp, &
     &    sf_grp_tbl_2nd%edge%nitem_e_grp, sf_grp_tbl_2nd%edge%istack_e_grp, &
     &    imark_4_grp)
!
!
      call alloc_item_other_grp(sf_grp_tbl_2nd%edge)
!
      call set_nod_4_ele_grp(edge_2nd%numedge, surf_2nd%numsurf, nedge_4_surf, &
     &    edge_2nd%iedge_4_sf, num_surf_2nd, num_surf_bc_2nd,           &
     &    surf_istack_2nd, sf_grp_tbl_2nd%isurf_grp, sf_grp_tbl_2nd%edge%ntot_e_grp, &
     &    sf_grp_tbl_2nd%edge%nitem_e_grp, sf_grp_tbl_2nd%edge%istack_e_grp, &
     &    sf_grp_tbl_2nd%edge%item_e_grp, imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_edge_4_surf_grp_2nd
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_surf_grp_2nd
!
!
      call allocate_imark_4_grp(node_2nd%numnod)
      call alloc_num_other_grp(num_surf_2nd, sf_grp_tbl_2nd%node)
!
      call count_nod_4_ele_grp(node_2nd%numnod, surf_2nd%numsurf,       &
     &    surf_2nd%nnod_4_surf, surf_2nd%ie_surf, num_surf_2nd,         &
     &    num_surf_bc_2nd, surf_istack_2nd, sf_grp_tbl_2nd%isurf_grp,              &
     &    sf_grp_tbl_2nd%node%ntot_e_grp, sf_grp_tbl_2nd%node%nitem_e_grp, &
     &    sf_grp_tbl_2nd%node%istack_e_grp, &
     &    imark_4_grp)
!
      call alloc_item_other_grp(sf_grp_tbl_2nd%node)
!
      call set_nod_4_ele_grp(node_2nd%numnod, surf_2nd%numsurf,         &
     &    surf_2nd%nnod_4_surf, surf_2nd%ie_surf, num_surf_2nd,         &
     &    num_surf_bc_2nd, surf_istack_2nd, sf_grp_tbl_2nd%isurf_grp,              &
     &    sf_grp_tbl_2nd%node%ntot_e_grp, sf_grp_tbl_2nd%node%nitem_e_grp, &
     &    sf_grp_tbl_2nd%node%istack_e_grp, &
     &    sf_grp_tbl_2nd%node%item_e_grp, imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_node_4_surf_grp_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_surf_grp_2nd
!
      use set_surface_id_4_surf_grp
!
!
      call alloc_surf_item_sf_grp_type(num_surf_bc_2nd, sf_grp_tbl_2nd)
      call set_surface_id_4_surf_group(ele_2nd%numele, surf_2nd%isf_4_ele,    &
     &    num_surf_2nd, num_surf_bc_2nd, surf_istack_2nd,               &
     &    surf_item_2nd, sf_grp_tbl_2nd%isurf_grp, sf_grp_tbl_2nd%isurf_grp_n)
!
      end subroutine set_surf_id_4_surf_grp_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_imark_4_grp(num)
!
      integer(kind = kint) :: num
!
      allocate(imark_4_grp(num))
      imark_4_grp = 0
!
      end subroutine allocate_imark_4_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_imark_4_grp
!
      deallocate(imark_4_grp)
!
      end subroutine deallocate_imark_4_grp
!
!-----------------------------------------------------------------------
!
      end module set_2nd_mesh_4_grp
