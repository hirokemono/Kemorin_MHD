!const_linear_mesh_type.f90
!      module const_linear_mesh_type
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine s_const_linear_mesh_type(femmesh_l, surf_mesh_l,      &
!     &          edge_mesh_l, nod_fld_l)
!      subroutine set_linear_phys_data_type(femmesh_l, nod_fld_l)
!
      module const_linear_mesh_type
!
      use m_precision
!
      implicit none
!
      private :: link_data_4_linear_grid
      private :: set_linear_data_by_quad_data
      private :: set_linear_data_by_lag_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_linear_mesh_type(femmesh_l, surf_mesh_l,       &
     &          edge_mesh_l, nod_fld_l)
!
      use m_geometry_constants
      use m_geometry_parameter
      use t_mesh_data
      use t_phys_data
      use const_mesh_types_info
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      femmesh_l%mesh%ele%nnod_4_ele = num_t_linear
      surf_mesh_l%surf%nnod_4_surf =  num_linear_sf
      edge_mesh_l%edge%nnod_4_edge =  num_linear_edge
!
      if      (nnod_4_ele .eq. num_t_linear) then
        call link_data_4_linear_grid(femmesh_l,                         &
     &          surf_mesh_l, edge_mesh_l, nod_fld_l)
      else if (nnod_4_ele .eq. num_t_quad) then
        call set_linear_data_by_quad_data(femmesh_l,                    &
     &          surf_mesh_l, edge_mesh_l, nod_fld_l)
      else if (nnod_4_ele .eq. num_t_lag) then
        call set_linear_data_by_lag_data(femmesh_l,                     &
     &          surf_mesh_l, edge_mesh_l, nod_fld_l)
      end if 
!
      end subroutine s_const_linear_mesh_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_phys_data_type(femmesh_l, nod_fld_l)
!
      use m_geometry_constants
      use m_geometry_parameter
      use t_mesh_data
      use t_phys_data
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      if (nnod_4_ele .eq. num_t_quad) then
        call copy_nod_phys_2_linear_t(femmesh_l%mesh, nod_fld_l)
        call generate_phys_on_surf_t(femmesh_l%mesh, nod_fld_l)
      end if
!
      end subroutine set_linear_phys_data_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_data_4_linear_grid(femmesh_l,                   &
     &          surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use m_surface_group_connect
      use m_element_group_connect
      use t_mesh_data
      use t_phys_data
      use link_data_type_to_1st_mesh
      use link_group_type_2_1st_mesh
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call link_single_ele_list_type(surf_mesh_l%surf,                  &
     &    edge_mesh_l%edge)
!
      call link_node_data_type(femmesh_l%mesh%node)
      call link_element_data_type(femmesh_l%mesh%ele)
!
      call link_node_group_to_type(femmesh_l%group%nod_grp)
      call link_element_group_to_type(femmesh_l%group%ele_grp)
      call link_surface_group_to_type(femmesh_l%group%surf_grp)
!
      call link_surface_data_type(surf_mesh_l%surf)
      call link_edge_data_type(edge_mesh_l%edge)
!
      call link_ele_grp_connect_type                                    &
     &    (ele_grp_data1, femmesh_l%group%tbls_ele_grp)
      call link_surf_grp_connect_type                                   &
     &    (sf_grp_data1, femmesh_l%group%tbls_surf_grp)
!
      call link_nodal_fld_type(nod_fld_l)
!
      end subroutine link_data_4_linear_grid
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_quad_data(femmesh_l,                &
     &          surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use t_mesh_data
      use t_phys_data
!
      use const_group_type_info
      use cvt_quad_2_linear_mesh_t
      use set_size_4_smp_types
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_type_info(surf_mesh_l%surf,                &
     &      edge_mesh_l%edge)
!
      call generate_linear_nod_t_by_1st(femmesh_l%mesh)
!
      if (iflag_debug.eq.1)                                             &
     &      write(*,*) 'connect_quad_mesh_2_linear_t'
      call connect_quad_mesh_2_linear_t(femmesh_l%mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_group_type'
      call gen_linear_group_type(femmesh_l%group)
!
      if (iflag_debug.eq.1) write(*,*) 's_const_surface_type_data'
      call s_const_surface_type_data(femmesh_l%mesh%node,               &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf)
      call s_const_edge_type_data(femmesh_l%mesh%node,                  &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf, edge_mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 's_const_group_type_info'
      call s_const_group_type_info(femmesh_l%mesh, surf_mesh_l,         &
     &    edge_mesh_l, femmesh_l%group)
!
      call count_ele_4_smp_mesh_type(femmesh_l%mesh%ele)
      call count_surf_size_smp_type(surf_mesh_l%surf)
      call count_edge_size_smp_type(edge_mesh_l%edge)
!
      call set_internal_list_4_ltype_20(femmesh_l%mesh,                 &
     &    surf_mesh_l%surf, edge_mesh_l%edge)
!
      call init_linear_phys_type_by_1st(femmesh_l%mesh, nod_fld_l)
!
      end subroutine set_linear_data_by_quad_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_lag_data(femmesh_l,                 &
     &          surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use t_mesh_data
      use t_phys_data
!
      use cvt_quad_2_linear_mesh_t
      use const_surface_type_data
      use const_edge_type_data
      use const_group_type_info
      use set_size_4_smp_types
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_type_info(surf_mesh_l%surf,                &
     &      edge_mesh_l%edge)
!
      call link_node_data_type(femmesh_l%mesh%node)
!
      if (iflag_debug.eq.1) write(*,*) 'connect_lag_mesh_2_linear_t'
      call connect_lag_mesh_2_linear_t(femmesh_l%mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'generate_2nd_linear_ele_group'
      call gen_linear_ele_grp_type(femmesh_l%group)
!
      if (iflag_debug.eq.1) write(*,*) 's_const_surface_type_data'
      call s_const_surface_type_data(femmesh_l%mesh%node,               &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf)
      call s_const_edge_type_data(femmesh_l%mesh%node,                  &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf, edge_mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 's_const_group_type_info'
      call s_const_group_type_info(femmesh_l%mesh, surf_mesh_l,         &
     &    edge_mesh_l, femmesh_l%group)
!
      call count_size_4_smp_mesh_type(femmesh_l%mesh%node,              &
     &    femmesh_l%mesh%ele)
      call count_surf_size_smp_type(surf_mesh_l%surf)
      call count_edge_size_smp_type(edge_mesh_l%edge)
!
      call set_internal_list_4_ltype_27(femmesh_l%mesh,                 &
     &    surf_mesh_l%surf, edge_mesh_l%edge)
!
      call link_nodal_fld_type(nod_fld_l)
!
      end subroutine set_linear_data_by_lag_data
!
! ----------------------------------------------------------------------
!
      end module const_linear_mesh_type
