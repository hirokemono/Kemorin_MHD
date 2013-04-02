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
!      subroutine link_smp_param_type(node, ele, surf, edge)
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
      node%numnod =         numnod
      node%internal_node = internal_node
!
      node%inod_global => globalnodid
      node%xx =>  xx
!
      node%rr =>    radius
      node%a_r =>   a_radius
      node%theta => colatitude
      node%phi =>   longitude
      node%ss =>    s_cylinder
      node%a_s =>   a_s_cylinder
!
      end subroutine link_node_data_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_element_data_type(ele)
!
      use t_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      type(element_data), intent(inout) :: ele
!
!
      ele%numele = numele
      ele%nnod_4_ele = nnod_4_ele
!
      ele%ie =>           ie
      ele%interior_ele => interior_ele
      ele%e_multi =>      e_multi
!
      ele%iele_global => globalelmid
      ele%elmtyp => elmtyp
      ele%nodelm => nodelm
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
      end subroutine link_edge_data_type
!
! ----------------------------------------------------------------------
!
      subroutine link_smp_param_type(node, ele, surf, edge)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use m_geometry_parameter
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      node%istack_nod_smp =>  inod_smp_stack
      node%istack_internal_smp => inter_smp_stack
      ele%istack_ele_smp =>  iele_smp_stack
      surf%istack_surf_smp => isurf_smp_stack
      edge%istack_edge_smp => iedge_smp_stack
!
      node%max_nod_smp =     maxnod_4_smp
      node%max_internal_nod_smp = max_in_nod_4_smp
      ele%max_ele_smp =     maxele_4_smp
      surf%max_surf_smp =    maxsurf_4_smp
      edge%max_edge_smp =    maxedge_4_smp
!
      end subroutine link_smp_param_type
!
! -------------------------------------------------------------------
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
      nod_fld%iorder_ele =>       iorder_nod_phys
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
      call link_nodal_field_names
!
      nod_fld%d_fld => d_nod
!
      end subroutine link_nodal_fld_type
!
! -------------------------------------------------------------------
!
      end module link_data_type_to_1st_mesh
