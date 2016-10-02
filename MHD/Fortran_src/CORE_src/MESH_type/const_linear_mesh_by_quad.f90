!const_linear_mesh_by_quad.f90
!      module const_linear_mesh_by_quad
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine link_data_4_linear_grid                              &
!!     &         (mesh_q, ele_mesh_q, group_q, nod_fld_q,               &
!!     &          mesh_l, ele_mesh_l, group_l, nod_fld_l)
!!      subroutine set_linear_data_by_quad_data                         &
!!     &         (mesh_q, ele_mesh_q, group_q, nod_fld_q,               &
!!     &          mesh_l, ele_mesh_l, group_l, nod_fld_l)
!!      subroutine set_linear_data_by_lag_data                          &
!!     &         (mesh_q, group_q, nod_fld_q,                           &
!!     &          mesh_l, ele_mesh_l, group_l, nod_fld_l)
!!        type(node_data), intent(in) ::    node_q
!!        type(element_data), intent(in) :: ele_q
!!        type(mesh_groups_p), intent(in) :: group_q
!!        type(surface_group_data), intent(in) :: sf_grp_q
!!        type(phys_data), intent(in) ::     nod_fld_q
!!        type(mesh_geometry), intent(in) :: mesh_l
!!        type(mesh_groups_p), intent(in) :: group_l
!!        type(element_geometry), intent(inout) :: ele_mesh_l
!!        type(phys_data), intent(inout) :: nod_fld_l
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
     &         (mesh_q, ele_mesh_q, group_q, nod_fld_q,                 &
     &          mesh_l, ele_mesh_l, group_l, nod_fld_l)
!
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(element_geometry_p), intent(in) :: ele_mesh_q
      type(mesh_groups_p), intent(in) :: group_q
!
      type(phys_data), intent(in) ::     nod_fld_q
!
      type(mesh_geometry), intent(inout) :: mesh_l
      type(mesh_groups_p), intent(inout) :: group_l
      type(element_geometry_p), intent(inout) :: ele_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call link_new_nod_geometry_type(mesh_q%node, mesh_l%node)
      call link_new_ele_connect_type(mesh_q%ele, mesh_l%ele)
      call link_new_overlaped_ele_type(mesh_q%ele, mesh_l%ele)
!
      ele_mesh_l%surf => ele_mesh_q%surf
      ele_mesh_l%edge => ele_mesh_q%edge
!
      group_l%nod_grp =>  group_q%nod_grp
      group_l%ele_grp =>  group_q%ele_grp
      group_l%surf_grp => group_q%surf_grp
!
      group_l%tbls_ele_grp =>  group_q%tbls_ele_grp
      group_l%tbls_surf_grp => group_q%tbls_surf_grp
!
      call link_field_data_type(nod_fld_q, nod_fld_l)
!
      end subroutine link_data_4_linear_grid
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_quad_data                           &
     &         (mesh_q, ele_mesh_q, group_q, nod_fld_q,                 &
     &          mesh_l, ele_mesh_l, group_l, nod_fld_l)
!
      use t_surface_data
!
      use const_mesh_information
      use cvt_quad_2_linear_mesh
      use set_size_4_smp_types
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(element_geometry_p), intent(in) :: ele_mesh_q
      type(mesh_groups_p), intent(in) :: group_q
!
      type(phys_data), intent(in) ::     nod_fld_q
!
      type(mesh_geometry), intent(inout) :: mesh_l
      type(mesh_groups_p), intent(inout) :: group_l
      type(element_geometry_p), intent(inout) :: ele_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_info(ele_mesh_l%surf, ele_mesh_l%edge)
!
      call generate_linear_nod_by_quad                                  &
     &   (mesh_q%node, mesh_q%ele, ele_mesh_q%surf, mesh_l)
!
      if (iflag_debug.eq.1)                                             &
     &      write(*,*) 'connect_quad_mesh_2_linear'
      call connect_quad_mesh_2_linear                                   &
     &   (mesh_q%node, mesh_q%ele, ele_mesh_q%surf, mesh_l)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_group_info'
      group_l%nod_grp => group_q%nod_grp
      allocate(group_l%ele_grp)
      allocate(group_l%surf_grp)
!
      call gen_linear_group_info(group_q%ele_grp, group_q%surf_grp,     &
     &   group_l%ele_grp, group_l%surf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
      call construct_surface_data(mesh_l%node,                          &
     &    mesh_l%ele, ele_mesh_l%surf)
      call construct_edge_data(mesh_l%node,                             &
     &    mesh_l%ele, ele_mesh_l%surf, ele_mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_group_type_info'
      call const_group_type_info                                        &
     &   (mesh_l, ele_mesh_l%surf, ele_mesh_l%edge,                     &
     &    group_l%ele_grp, group_l%surf_grp,                            &
     &    group_l%tbls_ele_grp, group_l%tbls_surf_grp)
!
      call count_ele_4_smp_mesh_type(mesh_l%ele)
      call count_surf_size_smp_type(ele_mesh_l%surf)
      call count_edge_size_smp_type(ele_mesh_l%edge)
!
      call set_internal_list_lin_20                                     &
     &   (mesh_q%node, mesh_q%ele, ele_mesh_q%surf,                     &
     &    mesh_l, ele_mesh_l%surf, ele_mesh_l%edge)
!
      call init_linear_nod_phys(mesh_l, nod_fld_q, nod_fld_l)
!
      end subroutine set_linear_data_by_quad_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_lag_data                            &
     &         (mesh_q, group_q, nod_fld_q,                             &
     &          mesh_l, ele_mesh_l, group_l, nod_fld_l)
!
      use const_surface_data
      use const_edge_data
      use const_mesh_information
      use set_size_4_smp_types
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(mesh_groups_p), intent(in) :: group_q
!
      type(phys_data), intent(in) ::     nod_fld_q
!
      type(mesh_geometry), intent(inout) :: mesh_l
      type(mesh_groups_p), intent(inout) :: group_l
      type(element_geometry_p), intent(inout) :: ele_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_info(ele_mesh_l%surf, ele_mesh_l%edge)
!
      call link_new_nod_geometry_type(mesh_q%node, mesh_l%node)
!
      if (iflag_debug.eq.1) write(*,*) 'connect_lag_mesh_2_linear'
      call connect_lag_mesh_2_linear(mesh_q%ele, mesh_l)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_group_info'
      group_l%nod_grp => group_q%nod_grp
      allocate(group_l%ele_grp)
      allocate(group_l%surf_grp)
!
      call gen_linear_group_info(group_q%ele_grp, group_q%surf_grp,     &
     &   group_l%ele_grp, group_l%surf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
      call construct_surface_data(mesh_l%node,                          &
     &    mesh_l%ele, ele_mesh_l%surf)
      call construct_edge_data(mesh_l%node,                             &
     &    mesh_l%ele, ele_mesh_l%surf, ele_mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_group_type_info'
      call const_group_type_info                                        &
     &   (mesh_l, ele_mesh_l%surf, ele_mesh_l%edge,                     &
     &    group_l%ele_grp, group_l%surf_grp,                            &
     &    group_l%tbls_ele_grp, group_l%tbls_surf_grp)
!
      call count_size_4_smp_mesh_type(mesh_l%node, mesh_l%ele)
      call count_surf_size_smp_type(ele_mesh_l%surf)
      call count_edge_size_smp_type(ele_mesh_l%edge)
!
      call set_internal_list_lin_27(mesh_q%node, mesh_l,                &
     &    ele_mesh_l%surf, ele_mesh_l%edge)
!
      call link_field_data_type(nod_fld_q, nod_fld_l)
!
      end subroutine set_linear_data_by_lag_data
!
! ----------------------------------------------------------------------
!
      end module const_linear_mesh_by_quad
