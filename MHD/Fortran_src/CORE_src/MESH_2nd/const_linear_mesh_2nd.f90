!const_linear_mesh_2nd.f90
!      module const_linear_mesh_2nd
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine set_2nd_geometry(my_rank)
!      subroutine set_linear_phys_data_2nd
!
      module const_linear_mesh_2nd
!
      use m_precision
!
      implicit none
!
      private :: link_data_4_linear_grid
      private :: set_linear_data_by_quad_data
      private :: set_linear_data_by_lag_data
      private :: const_2nd_group_info
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_2nd_geometry(my_rank)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_2nd_geometry_data
      use const_mesh_types_info
!
      integer(kind = kint), intent(in)  :: my_rank
!
!
      ele_2nd%nnod_4_ele =    num_t_linear
      surf_2nd%nnod_4_surf =  num_linear_sf
      edge_2nd%nnod_4_edge =  num_linear_edge
!
      if      (nnod_4_ele .eq. num_t_linear) then
        call link_data_4_linear_grid
      else if (nnod_4_ele .eq. num_t_quad) then
        call set_local_element_type_info(surf_2nd, edge_2nd)
        call set_linear_data_by_quad_data(my_rank)
      else if (nnod_4_ele .eq. num_t_lag) then
        call set_local_element_type_info(surf_2nd, edge_2nd)
        call set_linear_data_by_lag_data(my_rank)
      end if 
!
      end subroutine set_2nd_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_phys_data_2nd
!
      use m_geometry_constants
      use m_geometry_parameter
      use cvt_quad_2_linear_2nd_mesh
!
!
      if (nnod_4_ele .eq. num_t_quad) then
        call copy_2nd_data_on_vertex
        call generate_2nd_data_on_surf
      end if
!
      end subroutine set_linear_phys_data_2nd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_data_4_linear_grid
!
      use m_2nd_geometry_data
      use m_2nd_group_data
      use m_2nd_phys_data
      use link_group_type_2_1st_mesh
      use link_data_type_to_1st_mesh
!
!
      call link_single_ele_list_type(surf_2nd, edge_2nd)
!
      call link_node_data_type(node_2nd)
      call link_element_data_type(ele_2nd)
!
      call link_node_group_to_type(nod_grp_2nd)
      call link_element_group_to_type(ele_grp_2nd)
      call link_surface_group_to_type(sf_grp_2nd)
!
      call link_surface_data_type(surf_2nd)
      call link_edge_data_type(edge_2nd)
!
      call link_1st_ele_grp_connect_type(ele_grp_tbl_2nd)
      call link_1st_surf_grp_conn_type(sf_grp_tbl_2nd)
!
      call link_smp_param_type(node_2nd, ele_2nd, surf_2nd, edge_2nd)
!
      call link_nodal_fld_type(phys_2nd)
!
      end subroutine link_data_4_linear_grid
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_quad_data(my_rank)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_data
      use link_group_type_2_1st_mesh
      use cvt_quad_2_linear_2nd_mesh
      use const_surface_type_data
      use const_edge_type_data
      use set_size_4_smp_types
!
      integer(kind = kint), intent(in)  :: my_rank
!
!
      call generate_2nd_nod_on_surf
!
      if (iflag_debug.eq.1)                                             &
     &      write(*,*) 'connect_quad_4_sim_2_linear_2nd'
      call connect_quad_4_sim_2_linear_2nd
!
      if (iflag_debug.eq.1) write(*,*) 'generate_2nd_linear_group'
      call generate_2nd_linear_group
!
      if(iflag_debug.eq.1) write(*,*) 'const_surface_type_data surf_2nd'
      call s_const_surface_type_data(node_2nd, ele_2nd, surf_2nd)
      call s_const_edge_type_data(node_2nd, ele_2nd, surf_2nd, edge_2nd)
!
      if (iflag_debug.eq.1) write(*,*) 'const_2nd_group_info'
      call const_2nd_group_info
!
      call count_size_4_smp_mesh_type(node_2nd, ele_2nd)
      call count_surf_size_smp_type(surf_2nd)
      call count_edge_size_smp_type(edge_2nd)
      if ( iflag_debug.eq.1 ) then
        call check_smp_size_2nd(my_rank)
        call check_smp_size_2nd_surf_edge
      end if
!
!
      call set_internal_list_4_linear_20(numnod, internal_node,         &
     &          numele, numsurf, interior_ele, interior_surf,           &
     &          node_2nd%numnod, ele_2nd%numele, surf_2nd%numsurf,      &
     &          edge_2nd%numedge, ele_2nd%ie, surf_2nd%ie_surf,         &
     &          edge_2nd%ie_edge, ele_2nd%interior_ele,                 &
     &          surf_2nd%interior_surf, edge_2nd%interior_edge)
!
      call init_2nd_data_on_surf
!
      end subroutine set_linear_data_by_quad_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_lag_data(my_rank)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_data
      use m_2nd_group_data
      use link_data_type_to_1st_mesh
      use cvt_quad_2_linear_2nd_mesh
      use const_surface_type_data
      use const_edge_type_data
      use set_size_4_smp_types
      use m_2nd_phys_data
!
      integer(kind = kint), intent(in)  :: my_rank
!
!
      call link_node_data_type(node_2nd)
!
      if (iflag_debug.eq.1) write(*,*) 'connect_lag_4_sim_2_linear_2nd'
      call connect_lag_4_sim_2_linear_2nd
!
      if (iflag_debug.eq.1) write(*,*) 'generate_2nd_linear_group'
      call generate_2nd_linear_group
!
      if(iflag_debug.eq.1) write(*,*) 'const_surface_type_data surf_2nd'
      call s_const_surface_type_data(node_2nd, ele_2nd, surf_2nd)
      call s_const_edge_type_data(node_2nd, ele_2nd, surf_2nd, edge_2nd)
!
      if (iflag_debug.eq.1) write(*,*) 'const_2nd_group_info'
      call const_2nd_group_info
!
      call count_size_4_smp_mesh_type(node_2nd, ele_2nd)
      call count_surf_size_smp_type(surf_2nd)
      call count_edge_size_smp_type(edge_2nd)
!
!
      call set_internal_list_4_linear_27(internal_node,                 &
     &    node_2nd%numnod, ele_2nd%numele, surf_2nd%numsurf,            &
     &    edge_2nd%numedge, ele_2nd%ie, surf_2nd%ie_surf,               &
     &    edge_2nd%ie_edge, ele_2nd%interior_ele,                       &
     &    surf_2nd%interior_surf, edge_2nd%interior_edge)
!
      if ( iflag_debug.eq.1 ) then
        call check_smp_size_2nd(my_rank)
        call check_smp_size_2nd_surf_edge
      end if
!
      call link_nodal_fld_type(phys_2nd)
!
      end subroutine set_linear_data_by_lag_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_2nd_group_info
!
      use m_machine_parameter
      use m_2nd_geometry_data
      use m_2nd_group_data
      use set_tables_4_ele_grp_type
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_4_ele_grp_type'
      call set_surf_4_ele_grp_type(ele_2nd, surf_2nd, ele_grp_2nd,      &
     &    ele_grp_tbl_2nd)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_ele_grp_type'
      call set_edge_4_ele_grp_type(ele_2nd, edge_2nd, ele_grp_2nd,      &
     &    ele_grp_tbl_2nd)
!
       if (iflag_debug.eq.1) write(*,*) 'set_nod_4_ele_grp_type'
      call set_nod_4_ele_grp_type(ele_2nd, node_2nd, ele_grp_2nd,       &
     &    ele_grp_tbl_2nd)
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_id_4_surf_grp_type'
      call set_surf_id_4_surf_grp_type(ele_2nd, surf_2nd, sf_grp_2nd,   &
     &    sf_grp_tbl_2nd)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_surf_grp_type'
      call set_edge_4_surf_grp_type(surf_2nd, edge_2nd, sf_grp_2nd,     &
     &   sf_grp_tbl_2nd)
!
       if (iflag_debug.eq.1) write(*,*) 'set_node_4_surf_grp_type'
      call set_node_4_surf_grp_type(surf_2nd, node_2nd, sf_grp_2nd,     &
     &    sf_grp_tbl_2nd)
!
      end subroutine const_2nd_group_info
!
! ----------------------------------------------------------------------
!
      end module const_linear_mesh_2nd
