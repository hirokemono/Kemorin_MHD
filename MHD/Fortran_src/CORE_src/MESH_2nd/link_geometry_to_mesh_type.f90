!link_geometry_to_mesh_type.f90
!     module link_geometry_to_mesh_type
!
!      Written by H. Matsui on Sep., 2006
!
!       subroutine link_2nd_1ele_list_type(surf, edge)
!       subroutine link_2nd_node_type(node)
!       subroutine link_2nd_ele_connect_type(ele)
!       subroutine link_2nd_surf_connect_type(surf)
!       subroutine link_2nd_edge_connect_type(edge)
!
!       subroutine link_2nd_ele_geom_to_type(ele)
!         type(node_data), intent(in) :: node
!         type(element_data), intent(in) :: ele
!         type(surface_data), intent(in) :: surf
!         type(edge_data), intent(in) :: edge
!
      module link_geometry_to_mesh_type
!
      use m_precision
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_1ele_list_type(surf, edge)
!
       use t_surface_data
       use t_edge_data
!
       type(surface_data), intent(in) :: surf
       type(edge_data), intent(in) :: edge
!
       node_on_sf_2nd =>   surf%node_on_sf
       node_on_sf_n_2nd => surf%node_on_sf_n
!
       node_on_edge_2nd =>    edge%node_on_edge
       node_on_edge_sf_2nd => edge%node_on_edge_sf
!
       end subroutine link_2nd_1ele_list_type
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_node_type(node)
!
       use t_geometry_data
!
       type(node_data), intent(in) :: node
!
!
       nnod_2nd =         node%numnod
       internal_nod_2nd = node%internal_node
!
       maxnod_4_smp_2nd =     node%max_nod_smp
       max_in_nod_4_smp_2nd = node%max_internal_nod_smp
       inod_smp_stack_2nd =>  node%istack_nod_smp
       inter_smp_stack_2nd => node%istack_internal_smp
!
       globalnodid_2nd => node%inod_global
       xx_2nd =>          node%xx
!
       radius_2nd =>   node%rr
       a_radius_2nd => node%a_r
       theta_2nd =>    node%theta
       phi_2nd =>      node%phi
       s_cyl_2nd =>    node%ss
       a_s_cyl_2nd =>  node%a_s
!
       end subroutine link_2nd_node_type
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_ele_connect_type(ele)
!
       use t_geometry_data
!
       type(element_data), intent(in) :: ele
!
!
       nele_2nd =            ele%numele
       nnod_4_ele_2nd =      ele%nnod_4_ele
       internal_ele_2nd =    ele%internal_ele
!
       maxele_4_smp_2nd =    ele%max_ele_smp
       iele_smp_stack_2nd => ele%istack_ele_smp
!
       ie_2nd =>             ele%ie
       interior_ele_2nd =>   ele%interior_ele
       e_multi_2nd =>        ele%e_multi
!
       globalelmid_2nd =>    ele%iele_global
       elmtyp_2nd =>         ele%elmtyp
       nodelm_2nd =>         ele%nodelm
!
       end subroutine link_2nd_ele_connect_type
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_surf_connect_type(surf)
!
       use t_surface_data
!
       type(surface_data), intent(in) :: surf
!
!
       nsurf_2nd =         surf%numsurf
       nnod_4_surf_2nd =   surf%nnod_4_surf
       internal_surf_2nd = surf%internal_surf
!
       maxsurf_4_smp_2nd =    surf%max_surf_smp
       isurf_smp_stack_2nd => surf%istack_surf_smp
!
       globalsurfid_2nd =>  surf%isurf_global
       ie_surf_2nd =>       surf%ie_surf
       isf_4_ele_2nd =>     surf%isf_4_ele
       interior_surf_2nd => surf%interior_surf
!
       end subroutine link_2nd_surf_connect_type
!
! ----------------------------------------------------------------------
!
       subroutine link_2nd_edge_connect_type(edge)
!
       use t_edge_data
!
       type(edge_data), intent(in) :: edge
!
!
       nedge_2nd =         edge%numedge
       nnod_4_edge_2nd =   edge%nnod_4_edge
       internal_edge_2nd = edge%internal_edge
!
       maxedge_4_smp_2nd =    edge%max_edge_smp
       iedge_smp_stack_2nd => edge%istack_edge_smp
!
       globaledgeid_2nd =>  edge%iedge_global
       ie_edge_2nd =>       edge%ie_edge
       iedge_4_sf_2nd =>    edge%iedge_4_sf
       iedge_4_ele_2nd =>   edge%iedge_4_ele
       interior_edge_2nd => edge%interior_edge
!
       end subroutine link_2nd_edge_connect_type
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_ele_geom_to_type(ele)
!
       use m_2nd_element_geometry_data
!
       use t_geometry_data
!
       type(element_data), intent(in) :: ele
!
!
       nele_2nd =    ele%numele
       x_ele_2nd =>  ele%x_ele
!
       r_ele_2nd =>     ele%r_ele
       ar_ele_2nd =>    ele%ar_ele
       phi_ele_2nd =>   ele%phi_ele
       theta_ele_2nd => ele%theta_ele
       s_ele_2nd =>     ele%s_ele
       as_ele_2nd =>    ele%as_ele
!
       volume_ele_2nd => ele%volume_ele
       a_vol_ele_2nd =>  ele%a_vol_ele
!
       volume_2nd =      ele%volume
!
       end subroutine link_2nd_ele_geom_to_type
!
!  ---------------------------------------------------------------------
!
      end module link_geometry_to_mesh_type
