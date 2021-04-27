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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_quad_data                           &
     &         (mesh_q, group_q, ele_comm_q, surf_comm_q, nod_fld_q,    &
     &          mesh_l, group_l, nod_fld_l)
!
      use t_surface_data
      use t_element_group_table
      use t_quad_to_linear_list
!
      use const_node_and_element_q2l
      use const_node_comm_table_q2l
      use const_node_group_item_q2l
      use cvt_quad_2_linear_mesh
!
      type(mesh_geometry), intent(in), target :: mesh_q
      type(mesh_groups), intent(in), target :: group_q
      type(communication_table), intent(in) :: ele_comm_q
      type(communication_table), intent(in) :: surf_comm_q
!
      type(phys_data), intent(in), target :: nod_fld_q
!
      type(mesh_geometry_p), intent(inout) :: mesh_l
      type(mesh_groups_p), intent(inout) :: group_l
      type(phys_data), intent(inout) :: nod_fld_l
!
      type(quad_to_linear_list) :: q_to_l
!
!
      allocate(mesh_l%node)
      call init_quad_to_linear_list(mesh_q, ele_comm_q, surf_comm_q,    &
     &    mesh_l%node%numnod, mesh_l%node%internal_node, q_to_l)
!
      allocate(mesh_l%ele)
      call s_const_node_element_q2l(mesh_q, q_to_l,                     &
     &                              mesh_l%node, mesh_l%ele)
!
      allocate(mesh_l%nod_comm)
      call s_const_node_comm_table_q2l                                  &
     &   (mesh_q, ele_comm_q, surf_comm_q, q_to_l, mesh_l%nod_comm)
!
      allocate(group_l%nod_grp)
      call s_const_node_group_item_q2l                                  &
     &   (mesh_q, group_q%nod_grp, q_to_l, group_l%nod_grp)
!
      call dealloc_quad_to_linear_list(q_to_l)
!
      allocate(group_l%ele_grp)
      allocate(group_l%surf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_group_info'
      call gen_linear_group_info(group_q%ele_grp, group_q%surf_grp,     &
     &   group_l%ele_grp, group_l%surf_grp)
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
      use t_element_group_table
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
!
      mesh_l%nod_comm => mesh_q%nod_comm
      mesh_l%node =>     mesh_q%node
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
      call count_size_4_smp_mesh(mesh_l%node, mesh_l%ele)
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
      use single_edge_information
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
