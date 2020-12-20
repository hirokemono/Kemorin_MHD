!>@file   const_linear_mesh_by_quad.f90
!!@brief  module const_linear_mesh_by_quad
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!> @brief set numbers for SMP parallelization
!!
!!@verbatim
!!      subroutine set_linear_data_by_quad_data                         &
!!     &         (mesh_q, group_q, nod_fld_q,                           &
!!     &          mesh_l, group_l, nod_fld_l)
!!      subroutine const_linear_data_by_lag_data                        &
!!     &         (mesh_q, group_q, mesh_l, group_l)
!!        type(mesh_geometry), intent(in), target :: mesh_q
!!        type(mesh_groups), intent(in), target :: group_q
!!        type(surface_group_data), intent(in), target :: sf_grp_q
!!        type(phys_data), intent(in) ::     nod_fld_q
!!        type(mesh_geometry_p), intent(inout) :: mesh_l
!!        type(mesh_groups_p), intent(inout) :: group_l
!!        type(phys_data), intent(inout) :: nod_fld_l
!!
!!      subroutine dealloc_linear_data_by_quad                          &
!!     &         (mesh_l, group_l, nod_fld_l)
!!@endverbatim
!
      module const_linear_mesh_by_quad
!
      use m_precision
      use m_machine_parameter
      use t_geometry_data
      use t_mesh_data
      use t_group_data
      use t_phys_data
      use t_mesh_data_with_pointer
!
      implicit none
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_quad_data                           &
     &         (mesh_q, group_q, nod_fld_q,                             &
     &          mesh_l, group_l, nod_fld_l)
!
      use t_surface_data
!
      use const_mesh_information
      use cvt_quad_2_linear_mesh
      use set_size_4_smp_types
!
      type(mesh_geometry), intent(in), target :: mesh_q
      type(mesh_groups), intent(in), target :: group_q
!
      type(phys_data), intent(in), target ::     nod_fld_q
!
      type(mesh_geometry_p), intent(inout) :: mesh_l
      type(mesh_groups_p), intent(inout) :: group_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_info(mesh_l%surf, mesh_l%edge)
!
      allocate(mesh_l%node)
      allocate(mesh_l%ele)
      allocate(group_l%ele_grp)
      allocate(group_l%surf_grp)
      call generate_linear_nod_by_quad                                  &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf, mesh_l%node)
!
      if (iflag_debug.eq.1)                                             &
     &      write(*,*) 'connect_quad_mesh_2_linear'
      call connect_quad_mesh_2_linear                                   &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf,                         &
     &    mesh_l%node, mesh_l%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_group_info'
      group_l%nod_grp => group_q%nod_grp
!
      call gen_linear_group_info(group_q%ele_grp, group_q%surf_grp,     &
     &   group_l%ele_grp, group_l%surf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
      call construct_surface_data(mesh_l%node,                          &
     &    mesh_l%ele, mesh_l%surf)
      call construct_edge_data(mesh_l%node,                             &
     &    mesh_l%ele, mesh_l%surf, mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_group_type_info'
      call const_group_type_info                                        &
     &   (mesh_l%node, mesh_l%ele, mesh_l%surf, mesh_l%edge,            &
     &    group_l%ele_grp, group_l%surf_grp,                            &
     &    group_l%tbls_ele_grp, group_l%tbls_surf_grp)
!
      call count_size_4_smp_mesh(mesh_l%node, mesh_l%ele)
      call count_size_4_smp_surf_edge(mesh_l%surf, mesh_l%edge)
!
      call set_internal_list_lin_20                                     &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf,                         &
     &    mesh_l%node, mesh_l%ele, mesh_l%surf, mesh_l%edge)
!
      call init_linear_nod_phys(mesh_l%node, nod_fld_q, nod_fld_l)
!
      end subroutine set_linear_data_by_quad_data
!
!  ---------------------------------------------------------------------
!
      subroutine const_linear_data_by_lag_data                          &
     &         (mesh_q, group_q, mesh_l, group_l)
!
      use const_surface_data
      use const_edge_data
      use const_mesh_information
      use set_size_4_smp_types
!
      type(mesh_geometry), intent(in), target :: mesh_q
      type(mesh_groups), intent(in), target :: group_q
!
      type(mesh_geometry_p), intent(inout) :: mesh_l
      type(mesh_groups_p), intent(inout) :: group_l
!
!
      allocate(mesh_l%ele)
      allocate(group_l%ele_grp)
      allocate(group_l%surf_grp)
      call set_local_element_info(mesh_l%surf, mesh_l%edge)
!
      mesh_l%node => mesh_q%node
!
      if (iflag_debug.eq.1) write(*,*) 'connect_lag_mesh_2_linear'
      call connect_lag_mesh_2_linear                                    &
     &   (mesh_q%ele, mesh_l%node, mesh_l%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_group_info'
      group_l%nod_grp => group_q%nod_grp
!
      call gen_linear_group_info(group_q%ele_grp, group_q%surf_grp,     &
     &   group_l%ele_grp, group_l%surf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
      call construct_surface_data(mesh_l%node,                          &
     &    mesh_l%ele, mesh_l%surf)
      call construct_edge_data(mesh_l%node,                             &
     &    mesh_l%ele, mesh_l%surf, mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_group_type_info'
      call const_group_type_info                                        &
     &   (mesh_l%node, mesh_l%ele, mesh_l%surf, mesh_l%edge,            &
     &    group_l%ele_grp, group_l%surf_grp,                            &
     &    group_l%tbls_ele_grp, group_l%tbls_surf_grp)
!
      call count_size_4_smp_mesh(mesh_l%node, mesh_l%ele)
      call count_size_4_smp_surf_edge(mesh_l%surf, mesh_l%edge)
!
      call set_internal_list_lin_27                                     &
     &   (mesh_q%node, mesh_l%node, mesh_l%ele,                         &
     &    mesh_l%surf, mesh_l%edge)
!
      end subroutine const_linear_data_by_lag_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_linear_data_by_quad                            &
     &         (mesh_l, group_l, nod_fld_l)
!
      use set_size_4_smp_types
!
      type(mesh_geometry_p), intent(inout) :: mesh_l
      type(mesh_groups_p), intent(inout) :: group_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call finalize_size_4_smp_mesh(mesh_l%node, mesh_l%ele)
      call finalize_size_4_smp_surf_edge(mesh_l%surf, mesh_l%edge)
!
      end subroutine dealloc_linear_data_by_quad
!
! ----------------------------------------------------------------------
!
      end module const_linear_mesh_by_quad
