!
!     module link_data_type_to_1st_mesh
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine link_single_ele_list_type(surf, edge)
!
!      subroutine link_node_data_type(node_new)
!      subroutine link_element_data_type(ele_new)
!      subroutine link_surface_data_type(surf)
!      subroutine link_edge_data_type(edge_new)
!
!      subroutine link_ele_geometry_type(ele)
!
!      subroutine link_nodal_fld_type_names(nod_fld)
!      subroutine link_nodal_fld_type(nod_fld)
!
      module link_data_type_to_1st_mesh
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine link_single_ele_list_type(surf, edge)
!
      use m_geometry_data
      use t_surface_data
      use t_edge_data
!
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
      surf%node_on_sf =>   surf1%node_on_sf
      surf%node_on_sf_n => surf1%node_on_sf_n
!
      edge%node_on_edge =>    edge1%node_on_edge
      edge%node_on_edge_sf => edge1%node_on_edge_sf
!
      end subroutine link_single_ele_list_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_node_data_type(node_new)
!
      use t_geometry_data
      use m_geometry_data
!
      type(node_data), intent(inout) :: node_new
!
!
      node_new%numnod =        node1%numnod
      node_new%internal_node = node1%internal_node
!
      node_new%inod_global => node1%inod_global
      node_new%xx =>  node1%xx
!
      node_new%rr =>    node1%rr
      node_new%a_r =>   node1%a_r
      node_new%theta => node1%theta
      node_new%phi =>   node1%phi
      node_new%ss =>    node1%ss
      node_new%a_s =>   node1%a_s
!
      node_new%max_nod_smp =          node1%max_nod_smp
      node_new%max_internal_nod_smp = node1%max_internal_nod_smp
!
      node_new%istack_nod_smp =>      node1%istack_nod_smp
      node_new%istack_internal_smp => node1%istack_internal_smp
!
      end subroutine link_node_data_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_element_data_type(ele_new)
!
      use t_geometry_data
      use m_geometry_data
!
      type(element_data), intent(inout) :: ele_new
!
!
      ele_new%numele = ele1%numele
      ele_new%nnod_4_ele = ele1%nnod_4_ele
!
      ele_new%ie =>           ele1%ie
      ele_new%interior_ele => ele1%interior_ele
!
      ele_new%iele_global => ele1%iele_global
      ele_new%elmtyp =>      ele1%elmtyp
      ele_new%nodelm =>      ele1%nodelm
!
      ele_new%istack_ele_smp =>  ele1%istack_ele_smp
      ele_new%max_ele_smp =      ele1%max_ele_smp
!
      end subroutine link_element_data_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_surface_data_type(surf_new)
!
      use t_surface_data
      use m_geometry_data
!
      type(surface_data), intent(inout) :: surf_new
!
!
      surf_new%numsurf =     surf1%numsurf
      surf_new%nnod_4_surf = surf1%nnod_4_surf
!
      surf_new%ie_surf =>       surf1%ie_surf
      surf_new%isf_4_ele =>     surf1%isf_4_ele
      surf_new%interior_surf => surf1%interior_surf
!
      surf_new%istack_surf_smp => surf1%istack_surf_smp
      surf_new%max_surf_smp =     surf1%max_surf_smp
!
      end subroutine link_surface_data_type
!
! ----------------------------------------------------------------------
!
      subroutine link_edge_data_type(edge_new)
!
      use t_edge_data
      use m_geometry_data
!
      type(edge_data), intent(inout) :: edge_new
!
!
      edge_new%numedge =       edge1%numedge
      edge_new%nnod_4_edge =   edge1%nnod_4_edge
!
      edge_new%ie_edge =>       edge1%ie_edge
      edge_new%iedge_4_sf =>    edge1%iedge_4_sf
      edge_new%iedge_4_ele =>   edge1%iedge_4_ele
      edge_new%interior_edge => edge1%interior_edge
!
      edge_new%istack_edge_smp => edge1%istack_edge_smp
      edge_new%max_edge_smp =     edge1%max_edge_smp
!
      end subroutine link_edge_data_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine link_ele_geometry_type(ele)
!
      use t_geometry_data
      use m_geometry_data
!
      type(element_data), intent(inout) :: ele
!

      ele%max_ele_smp =     ele1%max_ele_smp
      ele%istack_ele_smp => ele1%istack_ele_smp
!
      ele%x_ele =>     ele1%x_ele
      ele%r_ele =>     ele1%r_ele
      ele%ar_ele =>    ele1%ar_ele
      ele%phi_ele =>   ele1%phi_ele
      ele%theta_ele => ele1%theta_ele
      ele%s_ele =>     ele1%s_ele
      ele%as_ele =>    ele1%as_ele
!
      ele%volume_ele => volume_ele
      ele%a_vol_ele =>  a_vol_ele
!
      ele%volume =       volume
      ele%a_vol =        a_vol
!
      end subroutine link_ele_geometry_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_nodal_fld_type_names(nod_fld)
!
      use m_node_phys_data
      use t_phys_data
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      nod_fld%num_phys =  num_nod_phys
      nod_fld%ntot_phys = num_tot_nod_phys
!
      nod_fld%num_phys_viz =  num_nod_phys_vis
      nod_fld%ntot_phys_viz = num_tot_nod_phys_vis
!
      nod_fld%num_component =>    num_nod_component
      nod_fld%istack_component => istack_nod_component
      nod_fld%iorder_eletype =>   iorder_nod_phys
      nod_fld%iflag_monitor =>    iflag_nod_fld_monitor
      nod_fld%phys_name =>        phys_nod_name
!
      end subroutine link_nodal_fld_type_names
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine link_nodal_fld_type(nod_fld)
!
      use m_node_phys_data
      use t_phys_data
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call link_nodal_fld_type_names(nod_fld)
!
      nod_fld%d_fld => d_nod
!
      end subroutine link_nodal_fld_type
!
! -------------------------------------------------------------------
!
      end module link_data_type_to_1st_mesh
