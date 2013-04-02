!position_2_each_surf_type.f90
!      module position_2_each_surf_type
!
!      Written by H. Matsui on Sep., 2005
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine s_position_2_each_surf_type(mesh, surf,               &
!     &          sf_grp, sf_grp_int)
!        type(mesh_geometry),      intent(in) :: mesh
!        type(surface_data),           intent(in) :: surf
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surf_grp_geom_4_fem_int), intent(inout) :: sf_grp_int
!
!      subroutine delta_x_2_each_surf_type(mesh, surf,                  &
!                sf_grp, sf_grp_int)
!        type(mesh_geometry),      intent(in) :: mesh
!        type(surface_data),           intent(in) :: surf
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surf_grp_geom_4_fem_int), intent(inout) :: sf_grp_int
!
      module position_2_each_surf_type
!
      use m_precision
!
      use position_of_each_surface
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_position_2_each_surf_type(mesh, surf,                &
     &          sf_grp, sf_grp_int)
!
      use m_machine_parameter
      use t_mesh_data
      use t_group_data
      use t_surface_group_geometry
!
      type(mesh_geometry),      intent(in) :: mesh
      type(surface_data),       intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surf_grp_geom_4_fem_int), intent(inout) :: sf_grp_int
!
      call position_2_each_surf_grp(np_smp, mesh%node%numnod,           &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, surf%nnod_4_surf,       &
     &    surf%node_on_sf, mesh%ele%ie, mesh%node%xx, mesh%node%a_r,    &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%istack_grp,           &
     &    sf_grp%item_sf_grp, sf_grp%num_grp_smp,                       &
     &    sf_grp%istack_grp_smp, sf_grp_int%xe_sf)
!
      end subroutine s_position_2_each_surf_type
!
! ----------------------------------------------------------------------
!
      subroutine delta_x_2_each_surf_type(mesh, surf,                   &
     &          sf_grp, sf_grp_int)
!
      use m_machine_parameter
      use t_mesh_data
      use t_group_data
      use t_surface_group_geometry
!
      type(mesh_geometry),      intent(in) :: mesh
      type(surface_data),       intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surf_grp_geom_4_fem_int), intent(inout) :: sf_grp_int
!
      call delta_x_2_each_surf_grp(np_smp, mesh%node%numnod,            &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, surf%nnod_4_surf,       &
     &    surf%node_on_sf, surf%node_on_sf_n, mesh%ele%ie,              &
     &    mesh%node%xx, sf_grp%num_grp, sf_grp%num_item,                &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp, sf_grp%num_grp_smp,    &
     &    sf_grp%istack_grp_smp, sf_grp_int%dxe_sf)
!
      end subroutine delta_x_2_each_surf_type
!
! ----------------------------------------------------------------------
!
      end module position_2_each_surf_type
