!
!     module link_data_type_to_1st_mesh
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine link_single_ele_list_type(surf, edge)
!
!      subroutine link_node_data_type(node)
!      subroutine link_element_data_type(ele)
!      subroutine link_surface_data_type(surf)
!      subroutine link_edge_data_type(edge)
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
      use m_geometry_parameter
      use t_surface_data
      use t_edge_data
!
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
      surf%node_on_sf =>   node_on_sf
      surf%node_on_sf_n => node_on_sf_n
!
      edge%node_on_edge =>    node_on_edge
      edge%node_on_edge_sf => node_on_edge_sf
!
      end subroutine link_single_ele_list_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_node_data_type(node)
!
      use t_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      type(node_data), intent(inout) :: node
!
!
      node%numnod =        node1%numnod
      node%internal_node = internal_node
!
      node%inod_global => inod_global
      node%xx =>  xx
!
      node%rr =>    radius
      node%a_r =>   a_radius
      node%theta => colatitude
      node%phi =>   longitude
      node%ss =>    s_cylinder
      node%a_s =>   a_s_cylinder
!
      node%max_nod_smp =          maxnod_4_smp
      node%max_internal_nod_smp = max_in_nod_4_smp
!
      node%istack_nod_smp =>      node1%istack_nod_smp
      node%istack_internal_smp => inter_smp_stack
!
      end subroutine link_node_data_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_element_data_type(ele)
!
      use t_geometry_data
      use m_geometry_data
!
      type(element_data), intent(inout) :: ele
!
!
      ele%numele = ele1%numele
      ele%nnod_4_ele = ele1%nnod_4_ele
!
      ele%ie =>           ie
      ele%interior_ele => interior_ele
      ele%e_multi =>      e_multi
!
      ele%iele_global => iele_global
      ele%elmtyp => elmtyp
      ele%nodelm => nodelm
!
      ele%istack_ele_smp =>  iele_smp_stack
      ele%max_ele_smp =     maxele_4_smp
!
      end subroutine link_element_data_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_surface_data_type(surf)
!
      use t_surface_data
      use m_geometry_parameter
      use m_geometry_data
!
      type(surface_data), intent(inout) :: surf
!
!
      surf%numsurf =     numsurf
      surf%nnod_4_surf = nnod_4_surf
!
      surf%ie_surf =>       ie_surf
      surf%isf_4_ele =>     isf_4_ele
      surf%interior_surf => interior_surf
!
      surf%istack_surf_smp => isurf_smp_stack
      surf%max_surf_smp =    maxsurf_4_smp
!
      end subroutine link_surface_data_type
!
! ----------------------------------------------------------------------
!
      subroutine link_edge_data_type(edge)
!
      use t_edge_data
      use m_geometry_parameter
      use m_geometry_data
!
      type(edge_data), intent(inout) :: edge
!
!
      edge%numedge =       numedge
      edge%nnod_4_edge =   nnod_4_edge
!
      edge%ie_edge =>       ie_edge
      edge%iedge_4_sf =>    iedge_4_sf
      edge%iedge_4_ele =>   iedge_4_ele
      edge%interior_edge => interior_edge
!
      edge%istack_edge_smp => iedge_smp_stack
      edge%max_edge_smp =    maxedge_4_smp
!
      end subroutine link_edge_data_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine link_ele_geometry_type(ele)
!
      use t_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      type(element_data), intent(inout) :: ele
!

      ele%max_ele_smp =     maxele_4_smp
      ele%istack_ele_smp => iele_smp_stack
!
      ele%x_ele =>     x_ele
      ele%r_ele =>     r_ele
      ele%ar_ele =>    ar_ele
      ele%phi_ele =>   phi_ele
      ele%theta_ele => theta_ele
      ele%s_ele =>     s_ele
      ele%as_ele =>    as_ele
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
