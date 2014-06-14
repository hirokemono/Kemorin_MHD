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
      use m_2nd_geometry_param
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
      use m_2nd_ele_group_data
!
!
      call allocate_imark_4_grp(nsurf_2nd)
      call allocate_sf_stack_4_ele_g_2nd
!
      call count_nod_4_ele_grp(nsurf_2nd, nele_2nd, nsurf_4_ele,        &
     &    isf_4_ele_2nd, num_mat_2nd, num_mat_bc_2nd,                   &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ntot_surf_ele_grp_2nd, nsurf_ele_grp_2nd,                     &
     &    isurf_stack_ele_grp_2nd, imark_4_grp)
!
      call allocate_sf_id_4_ele_g_2nd
!
      call set_nod_4_ele_grp(nsurf_2nd, nele_2nd, nsurf_4_ele,          &
     &    isf_4_ele_2nd, num_mat_2nd, num_mat_bc_2nd,                   &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ntot_surf_ele_grp_2nd, nsurf_ele_grp_2nd,                     &
     &    isurf_stack_ele_grp_2nd, isurf_ele_grp_2nd,                   &
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
      use m_2nd_ele_group_data
!
!
      call allocate_imark_4_grp(edge_2nd%numedge)
      call allocate_eg_stack_4_ele_g_2nd
!
      call count_nod_4_ele_grp(edge_2nd%numedge, nele_2nd, nedge_4_ele, &
     &    edge_2nd%iedge_4_ele, num_mat_2nd, num_mat_bc_2nd,            &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ntot_edge_ele_grp_2nd, nedge_ele_grp_2nd,                     &
     &    iedge_stack_ele_grp_2nd, imark_4_grp)
!
      call allocate_eg_id_4_ele_g_2nd
!
      call set_nod_4_ele_grp(edge_2nd%numedge, nele_2nd, nedge_4_ele,   &
     &    edge_2nd%iedge_4_ele, num_mat_2nd, num_mat_bc_2nd,            &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ntot_edge_ele_grp_2nd, nedge_ele_grp_2nd,                     &
     &    iedge_stack_ele_grp_2nd, iedge_ele_grp_2nd,                   &
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
      use m_2nd_ele_group_data
!
!
      call allocate_imark_4_grp(nnod_2nd)
      call allocate_nd_stack_4_ele_g_2nd
!
      call count_nod_4_ele_grp(nnod_2nd, nele_2nd, nnod_4_ele_2nd,      &
     &    ie_2nd, num_mat_2nd, num_mat_bc_2nd,                          &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ntot_node_ele_grp_2nd, nnod_ele_grp_2nd,                      &
     &    inod_stack_ele_grp_2nd, imark_4_grp)
!
      call allocate_nd_id_4_ele_g_2nd
!
      call set_nod_4_ele_grp(nnod_2nd, nele_2nd, nnod_4_ele_2nd,        &
     &    ie_2nd, num_mat_2nd, num_mat_bc_2nd,                          &
     &    mat_istack_2nd, mat_item_2nd,                                 &
     &    ntot_node_ele_grp_2nd, nnod_ele_grp_2nd,                      &
     &    inod_stack_ele_grp_2nd, inod_ele_grp_2nd,                     &
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
      use m_2nd_surf_group_data
!
!
      call allocate_imark_4_grp(edge_2nd%numedge)
      call allocate_eg_stack_4_sf_g_2nd
!
      call count_nod_4_ele_grp(edge_2nd%numedge, nsurf_2nd,             &
     &    nedge_4_surf, edge_2nd%iedge_4_sf, num_surf_2nd, num_surf_bc_2nd,  &
     &    surf_istack_2nd, isurf_grp_2nd, ntot_edge_sf_grp_2nd,         &
     &    nedge_sf_grp_2nd, iedge_stack_sf_grp_2nd,                     &
     &    imark_4_grp)
!
!
      call allocate_eg_id_4_sf_g_2nd
!
      call set_nod_4_ele_grp(edge_2nd%numedge, nsurf_2nd, nedge_4_surf, &
     &    edge_2nd%iedge_4_sf, num_surf_2nd, num_surf_bc_2nd,           &
     &    surf_istack_2nd, isurf_grp_2nd, ntot_edge_sf_grp_2nd,         &
     &    nedge_sf_grp_2nd, iedge_stack_sf_grp_2nd,                     &
     &    iedge_surf_grp_2nd, imark_4_grp)
!
      call deallocate_imark_4_grp
!
      end subroutine set_edge_4_surf_grp_2nd
!
!-----------------------------------------------------------------------
!
      subroutine set_node_4_surf_grp_2nd
!
      use m_2nd_surf_group_data
!
!
      call allocate_imark_4_grp(nnod_2nd)
      call allocate_nd_stack_4_sf_g_2nd
!
      call count_nod_4_ele_grp(nnod_2nd, nsurf_2nd, nnod_4_surf_2nd,    &
     &    ie_surf_2nd, num_surf_2nd, num_surf_bc_2nd,                   &
     &    surf_istack_2nd, isurf_grp_2nd,                               &
     &    ntot_node_sf_grp_2nd, nnod_sf_grp_2nd, inod_stack_sf_grp_2nd, &
     &    imark_4_grp)
!
      call allocate_nd_id_4_sf_g_2nd
!
      call set_nod_4_ele_grp(nnod_2nd, nsurf_2nd, nnod_4_surf_2nd,      &
     &    ie_surf_2nd, num_surf_2nd, num_surf_bc_2nd,                   &
     &    surf_istack_2nd, isurf_grp_2nd,                               &
     &    ntot_node_sf_grp_2nd, nnod_sf_grp_2nd, inod_stack_sf_grp_2nd, &
     &    inod_surf_grp_2nd, imark_4_grp)
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
      use m_2nd_surf_group_data
      use set_surface_id_4_surf_grp
!
!
      call set_surface_id_4_surf_group(nele_2nd, isf_4_ele_2nd,         &
     &    num_surf_2nd, num_surf_bc_2nd, surf_istack_2nd,               &
     &    surf_item_2nd, isurf_grp_2nd, isurf_grp_n_2nd)
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
