!
!      module const_mesh_types_info
!
!      Written by H. Matsui on Dec., 2008
!
!
!      subroutine s_const_mesh_types_info(femmesh, surf_mesh, edge_mesh)
!        type(mesh_data), intent(in) :: femmesh
!        type(surface_geometry), intent(inout) :: surf_mesh
!        type(edge_geometry),    intent(inout) :: edge_mesh
!
!      subroutine empty_mesh_types_info(femmesh, surf_mesh, edge_mesh)
!        type(mesh_data),      intent(inout) :: femmesh
!        type(surface_geometry), intent(inout) :: surf_mesh
!        type(edge_geometry),    intent(inout) :: edge_mesh
!
!      subroutine set_nod_and_ele_type_infos(geom)
!        type(mesh_geometry), intent(inout) :: geom
!      subroutine set_surf_and_edge_types(femmesh, surf_mesh, edge_mesh)
!        type(mesh_data), intent(in) :: femmesh
!        type(surface_geometry), intent(inout) :: surf_mesh
!        type(edge_geometry),    intent(inout) :: edge_mesh
!
!      subroutine empty_nod_and_ele_type_infos(geom)
!        type(mesh_geometry), intent(inout) :: geom
!      subroutine empty_surf_edge_types(femmesh, surf_mesh, edge_mesh)
!        type(mesh_data), intent(in) :: femmesh
!        type(surface_geometry), intent(inout) :: surf_mesh
!        type(edge_geometry),    intent(inout) :: edge_mesh
!
      module const_mesh_types_info
!
      use m_precision
!
      use t_mesh_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_mesh_types_info(femmesh, surf_mesh, edge_mesh)
!
      use m_machine_parameter
      use set_smp_4_group_types
      use set_surface_node_grp_type
      use const_group_type_info
!
      type(mesh_data),      intent(inout) :: femmesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry),    intent(inout) :: edge_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_type_info'
      call set_local_element_type_info(surf_mesh%surf, edge_mesh%edge)
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_type_infos'
      call set_nod_and_ele_type_infos(femmesh%mesh)
!
!
      call set_surf_and_edge_types(femmesh, surf_mesh, edge_mesh)
!
       if (iflag_debug.eq.1) write(*,*) 'count_num_groups_type_smp'
      call count_num_groups_type_smp( femmesh%group )
!
       if (iflag_debug.eq.1) write(*,*) 's_set_surface_node_grp_type'
      call s_set_surface_node_grp_type(femmesh%mesh%node,               &
     &    femmesh%mesh%ele, surf_mesh%surf,                             &
     &    femmesh%group%surf_grp, femmesh%group%surf_nod_grp)
!
        call s_const_group_type_info                                    &
     &      (femmesh%mesh, surf_mesh, edge_mesh, femmesh%group)
!
!
      end subroutine s_const_mesh_types_info
!
! ----------------------------------------------------------------------
!
      subroutine empty_mesh_types_info(femmesh, surf_mesh, edge_mesh)
!
      use m_machine_parameter
      use set_smp_4_group_types
      use const_group_type_info
      use set_surface_node_grp_type
      use set_tables_4_ele_grp_type
      use set_tables_4_surf_grp_type
!
      type(mesh_data),      intent(inout) :: femmesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry),    intent(inout) :: edge_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'empty_nod_and_ele_type_infos'
      call empty_nod_and_ele_type_infos(femmesh%mesh)
!
!
      call empty_surf_edge_types(femmesh, surf_mesh, edge_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_groups_type_smp'
      femmesh%group%nod_grp%num_grp_smp =  0
      femmesh%group%ele_grp%num_grp_smp =  0
      femmesh%group%surf_grp%num_grp_smp = 0
      call count_num_groups_type_smp( femmesh%group )
      call count_grp_type_smp(femmesh%group%nod_grp)
      call count_grp_type_smp(femmesh%group%ele_grp)
!
     if (iflag_debug.eq.1) write(*,*) 's_set_surface_node_grp_type'
      call empty_surface_node_grp_type(femmesh%group%surf_grp,          &
     &    femmesh%group%surf_nod_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'empty_sf_ed_nod_ele_grp_type'
      call empty_sf_ed_nod_ele_grp_type(femmesh%group%ele_grp,          &
     &    femmesh%group%tbls_ele_grp)
!
      femmesh%group%surf_grp%num_item = 0
      if (iflag_debug.eq.1) write(*,*) 'empty_sf_ed_nod_surf_grp_type'
      call empty_sf_ed_nod_surf_grp_type(femmesh%group%surf_grp,        &
     &     femmesh%group%tbls_surf_grp)
!
!
      end subroutine empty_mesh_types_info
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_local_element_type_info(surf, edge)
!
      use m_machine_parameter
      use t_surface_data
      use t_edge_data
      use set_local_id_table_4_1ele
!
      type(surface_data), intent(inout) :: surf
      type(edge_data),    intent(inout) :: edge
!
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_inod_in_surf_type'
      call allocate_inod_in_surf_type(surf)
      call set_inod_in_surf(surf%nnod_4_surf,                           &
     &    surf%node_on_sf, surf%node_on_sf_n)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_inod_in_edge_type'
      call allocate_inod_in_edge_type(edge)
      call copy_inod_in_edge(edge%nnod_4_edge,                          &
     &    edge%node_on_edge, edge%node_on_edge_sf)
!
      end subroutine set_local_element_type_info
!
! ----------------------------------------------------------------------
!
      subroutine set_nod_and_ele_type_infos(geom)
!
      use m_machine_parameter
      use set_size_4_smp_types
      use cal_mesh_position_type
!
      type(mesh_geometry), intent(inout) :: geom
!
!
      call allocate_ele_geometry_type(geom%ele)
!
       if (iflag_debug.eq.1) write(*,*) 'count_size_4_smp_mesh_type'
      call count_size_4_smp_mesh_type(geom%node, geom%ele)
!
       if (iflag_debug.eq.1) write(*,*) 'set_spherical_position_type'
      call set_spherical_position_type(geom%node)
!
       if (iflag_debug.eq.1) write(*,*) 'set_center_of_ele_type'
      call set_center_of_ele_type(geom%node, geom%ele)
!
       if (iflag_debug.eq.1) write(*,*) 'count_overlap_ele_type'
      call count_overlap_ele_type(geom%node, geom%ele)
!
      end subroutine set_nod_and_ele_type_infos
!
! ----------------------------------------------------------------------
!
      subroutine set_surf_and_edge_types(femmesh, surf_mesh, edge_mesh)
!
      use set_surf_edge_mesh_types
!
      type(mesh_data), intent(in) :: femmesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry),    intent(inout) :: edge_mesh
!
!
      call set_surf_connect_type(femmesh%mesh, surf_mesh )
      call set_edge_connect_type(femmesh%mesh, surf_mesh, edge_mesh )
!
      call set_surf_geometry_type(femmesh%mesh, surf_mesh)
      call set_edge_geometry_type(femmesh%mesh, edge_mesh)
!
      end subroutine set_surf_and_edge_types
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine empty_nod_and_ele_type_infos(geom)
!
      use m_machine_parameter
      use t_geometry_data
!
      type(mesh_geometry), intent(inout) :: geom
!
!
      geom%ele%numele = 0
      call allocate_ele_geometry_type(geom%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_node_param_smp_type'
      call allocate_node_param_smp_type(geom%node)
      call allocate_ele_param_smp_type(geom%ele)
!
      end subroutine empty_nod_and_ele_type_infos
!
! ----------------------------------------------------------------------
!
      subroutine empty_surf_edge_types(femmesh, surf_mesh, edge_mesh)
!
      use t_surface_data
      use t_edge_data
      use const_surface_type_data
      use const_edge_type_data
!
      type(mesh_data), intent(in) :: femmesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry),    intent(inout) :: edge_mesh
!
!
      call empty_surf_connect_type(femmesh%mesh%ele, surf_mesh%surf)
      call empty_edge_connect_type(femmesh%mesh%ele, surf_mesh%surf,    &
     &    edge_mesh%edge)
!
      call allocate_surface_geom_type(surf_mesh%surf)
      call allocate_edge_geom_type(edge_mesh%edge)
!
      end subroutine empty_surf_edge_types
!
! ----------------------------------------------------------------------
!
      end module const_mesh_types_info
