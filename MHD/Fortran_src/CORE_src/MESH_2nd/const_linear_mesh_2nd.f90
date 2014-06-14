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
      private :: link_data_4_linear_grid, set_2nd_local_element_info
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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      integer(kind = kint), intent(in)  :: my_rank
!
!
      nnod_4_ele_2nd =   num_t_linear
      surf_2nd%nnod_4_surf =  num_linear_sf
      edge_2nd%nnod_4_edge =  num_linear_edge
!
      if      (nnod_4_ele .eq. num_t_linear) then
        call link_data_4_linear_grid
      else if (nnod_4_ele .eq. num_t_quad) then
        call set_2nd_local_element_info
        call set_linear_data_by_quad_data(my_rank)
      else if (nnod_4_ele .eq. num_t_lag) then
        call set_2nd_local_element_info
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
      use link_geometry_to_1st_mesh
      use link_group_to_1st_mesh
      use m_2nd_phys_data
!
!
      call link_single_ele_list
!
      call link_node_data
      call link_element_data
!
      call link_node_group
      call link_element_group
      call link_surface_group
!
      call link_surface_data
      call link_edge_data
!
      call link_element_group_data
      call link_surface_group_data
!
      call link_mesh_parameter_4_smp
!
      call link_nodal_field_data
!
      end subroutine link_data_4_linear_grid
!
!  ---------------------------------------------------------------------
!
      subroutine set_2nd_local_element_info
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use set_local_id_table_4_1ele
!
!
      call allocate_inod_in_surf_type(surf_2nd)
      call set_inod_in_surf(surf_2nd%nnod_4_surf,                            &
     &    surf_2nd%node_on_sf, surf_2nd%node_on_sf_n)
!
      call allocate_inod_in_edge_type(edge_2nd)
      call copy_inod_in_edge(edge_2nd%nnod_4_edge,                      &
     &    edge_2nd%node_on_edge, edge_2nd%node_on_edge_sf)
!
      end subroutine set_2nd_local_element_info
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_quad_data(my_rank)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use link_group_to_1st_mesh
      use cvt_quad_2_linear_2nd_mesh
      use const_2nd_edge_and_surface
      use set_smp_size_4_2nd
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
      if (iflag_debug.eq.1) write(*,*) 'const_2nd_surface_data'
      call const_2nd_surface_data
      call const_2nd_edge_data
!
      if (iflag_debug.eq.1) write(*,*) 'const_2nd_group_info'
      call const_2nd_group_info
!
      call s_count_all_smp_size_4_2nd
      if ( iflag_debug.eq.1 ) then
        call check_smp_size_2nd(my_rank)
        call check_smp_size_2nd_surf_edge
      end if
!
!
      call set_internal_list_4_linear_20(numnod, internal_node,         &
     &          numele, numsurf, interior_ele, interior_surf,           &
     &          nnod_2nd, nele_2nd, surf_2nd%numsurf, edge_2nd%numedge, &
     &          ie_2nd, surf_2nd%ie_surf, edge_2nd%ie_edge,             &
     &          interior_ele_2nd, surf_2nd%interior_surf, &
     &          edge_2nd%interior_edge)
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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use link_geometry_to_1st_mesh
      use cvt_quad_2_linear_2nd_mesh
      use const_2nd_edge_and_surface
      use set_smp_size_4_2nd
      use m_2nd_phys_data
!
      integer(kind = kint), intent(in)  :: my_rank
!
!
      call link_node_data
!
      if (iflag_debug.eq.1) write(*,*) 'connect_lag_4_sim_2_linear_2nd'
      call connect_lag_4_sim_2_linear_2nd
!
      if (iflag_debug.eq.1) write(*,*) 'generate_2nd_linear_group'
      call generate_2nd_linear_group
!
      if (iflag_debug.eq.1) write(*,*) 'const_2nd_surface_data'
      call const_2nd_surface_data
      call const_2nd_edge_data
!
      if (iflag_debug.eq.1) write(*,*) 'const_2nd_group_info'
      call const_2nd_group_info
!
      call s_count_all_smp_size_4_2nd
!
!
      call set_internal_list_4_linear_27(internal_node,                 &
     &    nnod_2nd, nele_2nd, surf_2nd%numsurf, edge_2nd%numedge,       &
     &    ie_2nd, surf_2nd%ie_surf, edge_2nd%ie_edge,                   &
     &    interior_ele_2nd,                                   &
     &    surf_2nd%interior_surf, edge_2nd%interior_edge)
!
      if ( iflag_debug.eq.1 ) then
        call check_smp_size_2nd(my_rank)
        call check_smp_size_2nd_surf_edge
      end if
!
      call link_nodal_field_data
!
      end subroutine set_linear_data_by_lag_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_2nd_group_info
!
      use m_machine_parameter
      use set_2nd_mesh_4_grp
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_4_ele_grp_2nd'
      call set_surf_4_ele_grp_2nd
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_ele_grp_2nd'
      call set_edge_4_ele_grp_2nd
!
       if (iflag_debug.eq.1) write(*,*) 'set_node_4_ele_grp_2nd'
      call set_node_4_ele_grp_2nd
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_id_4_surf_grp_2nd'
      call set_surf_id_4_surf_grp_2nd
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_surf_grp_2nd'
      call set_edge_4_surf_grp_2nd
!
       if (iflag_debug.eq.1) write(*,*) 'set_node_4_surf_grp_2nd'
      call set_node_4_surf_grp_2nd
!
      end subroutine const_2nd_group_info
!
! ----------------------------------------------------------------------
!
      end module const_linear_mesh_2nd
