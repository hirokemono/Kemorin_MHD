!const_linear_mesh_by_quad.f90
!      module const_linear_mesh_by_quad
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine link_data_4_linear_grid                              &
!!     &         (node_q, ele_q, surf_q, edge_q, nod_grp_q, ele_grp_q,  &
!!     &          sf_grp_q, ele_grp_tbl_q, sf_grp_tbl_q, nod_fld_q,     &
!!     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!!      subroutine set_linear_data_by_quad_data(node_q, ele_q, surf_q,  &
!!     &          nod_grp_q, ele_grp_q, sf_grp_q, nod_fld_q,            &
!!     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!!      subroutine set_linear_data_by_lag_data(node_q, ele_q,           &
!!     &          nod_grp_q, ele_grp_q, sf_grp_q, nod_fld_q,            &
!!     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      module const_linear_mesh_by_quad
!
      use m_precision
      use m_machine_parameter
      use t_geometry_data
      use t_mesh_data
      use t_group_data
      use t_phys_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine link_data_4_linear_grid                                &
     &         (node_q, ele_q, surf_q, edge_q, nod_grp_q, ele_grp_q,    &
     &          sf_grp_q, ele_grp_tbl_q, sf_grp_tbl_q, nod_fld_q,       &
     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use t_surface_data
      use t_edge_data
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
      type(edge_data), intent(in) ::    edge_q
!
      type(group_data), intent(in) :: nod_grp_q
      type(group_data), intent(in) :: ele_grp_q
      type(surface_group_data), intent(in) :: sf_grp_q
      type(element_group_table), intent(in) :: ele_grp_tbl_q
      type(surface_group_table), intent(in) :: sf_grp_tbl_q
!
      type(phys_data), intent(in) ::     nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call link_new_nod_geometry_type(node_q, femmesh_l%mesh%node)
      call link_new_ele_connect_type(ele_q, femmesh_l%mesh%ele)
      call link_new_overlaped_ele_type(ele_q, femmesh_l%mesh%ele)
!
      call link_group_type(nod_grp_q, femmesh_l%group%nod_grp)
      call link_group_type(ele_grp_q, femmesh_l%group%ele_grp)
      call link_surf_group_type(sf_grp_q, femmesh_l%group%surf_grp)
!
      call link_new_surf_connect_type(surf_q, surf_mesh_l%surf)
      call link_new_edge_connect_type(edge_q, edge_mesh_l%edge)
!
      call link_ele_grp_connect_type                                    &
     &    (ele_grp_tbl_q, femmesh_l%group%tbls_ele_grp)
      call link_surf_grp_connect_type                                   &
     &    (sf_grp_tbl_q, femmesh_l%group%tbls_surf_grp)
!
      call link_field_data_type(nod_fld_q, nod_fld_l)
!
      end subroutine link_data_4_linear_grid
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_quad_data(node_q, ele_q, surf_q,    &
     &          nod_grp_q, ele_grp_q, sf_grp_q, nod_fld_q,              &
     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use t_surface_data
!
      use const_mesh_information
      use cvt_quad_2_linear_mesh
      use set_size_4_smp_types
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
!
      type(group_data), intent(in) :: nod_grp_q
      type(group_data), intent(in) :: ele_grp_q
      type(surface_group_data), intent(in) :: sf_grp_q
!
      type(phys_data), intent(in) ::     nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_info(surf_mesh_l%surf, edge_mesh_l%edge)
!
      call generate_linear_nod_by_quad                                  &
     &   (node_q, ele_q, surf_q, femmesh_l%mesh)
!
      if (iflag_debug.eq.1)                                             &
     &      write(*,*) 'connect_quad_mesh_2_linear'
      call connect_quad_mesh_2_linear                                   &
     &   (node_q, ele_q, surf_q, femmesh_l%mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_group_info'
      call gen_linear_group_info(nod_grp_q, ele_grp_q, sf_grp_q,        &
     &                           femmesh_l%group)
!
      if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
      call construct_surface_data(femmesh_l%mesh%node,                  &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf)
      call construct_edge_data(femmesh_l%mesh%node,                     &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf, edge_mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_group_type_info'
      call const_group_type_info                                        &
     &   (femmesh_l%mesh%node, femmesh_l%mesh%ele,                      &
     &    surf_mesh_l%surf, edge_mesh_l%edge,                           &
     &    femmesh_l%group%ele_grp, femmesh_l%group%surf_grp,            &
     &    femmesh_l%group%tbls_ele_grp, femmesh_l%group%tbls_surf_grp)
!
      call count_ele_4_smp_mesh_type(femmesh_l%mesh%ele)
      call count_surf_size_smp_type(surf_mesh_l%surf)
      call count_edge_size_smp_type(edge_mesh_l%edge)
!
      call set_internal_list_lin_20(node_q, ele_q, surf_q,              &
     &    femmesh_l%mesh, surf_mesh_l%surf, edge_mesh_l%edge)
!
      call init_linear_nod_phys(femmesh_l%mesh, nod_fld_q, nod_fld_l)
!
      end subroutine set_linear_data_by_quad_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_lag_data(node_q, ele_q,             &
     &          nod_grp_q, ele_grp_q, sf_grp_q, nod_fld_q,              &
     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use const_surface_data
      use const_edge_data
      use const_mesh_information
      use set_size_4_smp_types
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
!
      type(group_data), intent(in) :: nod_grp_q
      type(group_data), intent(in) :: ele_grp_q
      type(surface_group_data), intent(in) :: sf_grp_q
!
      type(phys_data), intent(in) ::     nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_info(surf_mesh_l%surf, edge_mesh_l%edge)
!
      call link_new_nod_geometry_type(node_q, femmesh_l%mesh%node)
!
      if (iflag_debug.eq.1) write(*,*) 'connect_lag_mesh_2_linear'
      call connect_lag_mesh_2_linear(ele_q, femmesh_l%mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_group_info'
      call gen_linear_group_info(nod_grp_q, ele_grp_q, sf_grp_q,        &
     &                           femmesh_l%group)
!
      if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
      call construct_surface_data(femmesh_l%mesh%node,                  &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf)
      call construct_edge_data(femmesh_l%mesh%node,                     &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf, edge_mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_group_type_info'
      call const_group_type_info                                        &
     &   (femmesh_l%mesh%node, femmesh_l%mesh%ele,                      &
     &    surf_mesh_l%surf, edge_mesh_l%edge,                           &
     &    femmesh_l%group%ele_grp, femmesh_l%group%surf_grp,            &
     &    femmesh_l%group%tbls_ele_grp, femmesh_l%group%tbls_surf_grp)
!
      call count_size_4_smp_mesh_type(femmesh_l%mesh%node,              &
     &    femmesh_l%mesh%ele)
      call count_surf_size_smp_type(surf_mesh_l%surf)
      call count_edge_size_smp_type(edge_mesh_l%edge)
!
      call set_internal_list_lin_27(node_q, femmesh_l%mesh,             &
     &    surf_mesh_l%surf, edge_mesh_l%edge)
!
      call link_field_data_type(nod_fld_q, nod_fld_l)
!
      end subroutine set_linear_data_by_lag_data
!
! ----------------------------------------------------------------------
!
      end module const_linear_mesh_by_quad
