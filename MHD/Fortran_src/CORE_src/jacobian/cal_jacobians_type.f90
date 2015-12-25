!
!     module cal_jacobians_type
!
!      Written by H. Matsui on Dec., 2008
!
!      subroutine const_jacobian_type(mesh, group, jac_3d)
!        type(mesh_geometry), intent(in) :: mesh
!        type(mesh_groups), intent(in) :: group
!        type(jacobians_3d), intent(inout) :: jac_3d
!      subroutine const_jacobian_surface_type(mesh, surf_mesh, jac_2d)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_geometry), intent(in) :: surf_mesh
!        type(jacobians_2d), intent(inout) :: jac_2d
!      subroutine const_jacobian_edge_type(mesh, edge_mesh, jac_1d)
!        type(edge_geometry), intent(in) :: edge_mesh
!        type(jacobians_1d), intent(inout) :: jac_1d
!      subroutine cal_jacobian_surf_grp_type(mesh, surf_mesh,           &
!                group, jac_sf_grp)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_geometry), intent(in) :: surf_mesh
!        type(mesh_groups),      intent(in) :: group
!        type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!      subroutine const_linear_jac_3d_type(mesh, group, jacobians)
!        type(mesh_geometry), intent(in) :: mesh
!        type(mesh_groups), intent(in) ::  group
!        type(jacobians_type), intent(inout) :: jacobians
!
!      subroutine cal_linear_jac_surf_type(mesh, surf_mesh, jac_2d_l)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_geometry), intent(in) :: surf_mesh
!        type(jacobians_2d), intent(inout) :: jac_2d_l
!
!      subroutine cal_linear_jac_edge_type(mesh, edge_mesh, jac_1d_l)
!        type(mesh_geometry), intent(in) :: mesh
!        type(edge_geometry), intent(in) :: edge_mesh
!        type(jacobians_1d), intent(inout) :: jac_1d_l
!
!      subroutine cal_linear_jac_surf_grp_type(mesh, surf_mesh, group,  &
!     &          jac_sf_grp_l)
!        type(mesh_geometry), intent(in) :: mesh
!        type(mesh_groups), intent(in) ::  group
!        type(surface_geometry), intent(in) :: surf_mesh
!        type(jacobians_2d), intent(inout) :: jac_sf_grp_l
!
!
!      subroutine empty_jacobian_type(mesh, jac_3d)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(inout) :: jac_3d
!
!      subroutine empty_jacobian_surface_type(surf_mesh, jac_2d)
!        type(surface_geometry), intent(in) :: surf_mesh
!        type(jacobians_2d), intent(inout) :: jac_2d
!
!      subroutine empty_jacobian_surf_grp_type(surf_mesh,            &
!     &          group, jac_sf_grp)
!        type(surface_geometry), intent(in) :: surf_mesh
!        type(mesh_groups),      intent(in) :: group
!        type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!      subroutine empty_linear_jac_3d_type(mesh, jacobians)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_type), intent(inout) :: jacobians
!
      module cal_jacobians_type
!
      use m_precision
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use t_mesh_data
      use t_jacobians
      use t_jacobian_3d
      use t_jacobian_2d
      use t_jacobian_1d
!
      implicit  none
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_type(mesh, group, jac_3d)
!
      use const_jacobians_3d
      use const_jacobians_infty_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
!  data allocation
!
      call alloc_jacobians_type(mesh%ele%numele, mesh%ele%nnod_4_ele,   &
     &    maxtot_int_3d, jac_3d)
      call alloc_dxi_dx_type(mesh%ele%numele, jac_3d)
!
!  set jacobians
!
      if (mesh%ele%nnod_4_ele .eq. num_t_linear) then
        call cal_jacobian_trilinear(mesh%node, mesh%ele, jac_3d)
!
        if (group%infty_grp%ngrp_sf .gt. 0) then
        call cal_jacobian_infty_linear(mesh%node, mesh%ele,             &
     &      group%surf_grp, group%infty_grp, jac_3d)
        end if
!
      else if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call cal_jacobian_quad(mesh%node, mesh%ele, jac_3d)
!
        if (group%infty_grp%ngrp_sf .gt. 0) then
        call cal_jacobian_infty_quad(mesh%node, mesh%ele,               &
     &        group%surf_grp, group%infty_grp, jac_3d)
        end if
!
      else if (mesh%ele%nnod_4_ele .eq. num_t_lag) then
        call cal_jacobian_lag(mesh%node, mesh%ele, jac_3d)
!
        if (group%infty_grp%ngrp_sf .gt. 0) then
          call cal_jacobian_infty_lag(mesh%node, mesh%ele,              &
     &        group%surf_grp, group%infty_grp, jac_3d)
        end if
      end if
!
      call dealloc_inv_jac_type(jac_3d)
!
      end subroutine const_jacobian_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_surface_type(mesh, surf_mesh, jac_2d)
!
      use const_jacobians_2d
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call alloc_2d_jac_type(surf_mesh%surf%numsurf,                    &
     &    surf_mesh%surf%nnod_4_surf, maxtot_int_2d, jac_2d)
!
      if      (surf_mesh%surf%nnod_4_surf .eq. num_linear_sf) then
        call cal_jacobian_surface_linear                                &
     &     (mesh%node, surf_mesh%surf, jac_2d)
      else if (surf_mesh%surf%nnod_4_surf .eq. num_quad_sf)   then
        call cal_jacobian_surface_quad                                  &
     &     (mesh%node, surf_mesh%surf, jac_2d)
      else if (surf_mesh%surf%nnod_4_surf .eq. num_lag_sf)   then
        call cal_jacobian_surface_lag                                   &
     &     (mesh%node, surf_mesh%surf, jac_2d)
      end if
!
      end subroutine const_jacobian_surface_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_edge_type(mesh, edge_mesh, jac_1d)
!
      use const_jacobians_1d
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in) :: edge_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call alloc_1d_jac_type(edge_mesh%edge%numedge,                    &
     &    edge_mesh%edge%nnod_4_edge, maxtot_int_1d, jac_1d)
!
      if      (edge_mesh%edge%nnod_4_edge .eq. num_linear_edge) then
        call cal_jacobian_edge_linear                                   &
     &     (mesh%node, edge_mesh%edge, jac_1d)
      else if (edge_mesh%edge%nnod_4_edge .eq. num_quad_edge) then
        call cal_jacobian_edge_quad(mesh%node, edge_mesh%edge, jac_1d)
      end if
!
      end subroutine const_jacobian_edge_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surf_grp_type(mesh, surf_mesh,            &
     &          group, jac_sf_grp)
!
      use const_jacobians_sf_grp
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(mesh_groups),      intent(in) :: group
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call alloc_2d_jac_type(group%surf_grp%num_item,                   &
     &    surf_mesh%surf%nnod_4_surf, maxtot_int_2d, jac_sf_grp)
!
      if (group%surf_grp%num_grp .gt. 0) then
        if      (surf_mesh%surf%nnod_4_surf .eq. num_linear_sf) then
          call const_jacobian_sf_grp_linear(mesh%node, mesh%ele,        &
     &        group%surf_grp, jac_sf_grp)
        else if (surf_mesh%surf%nnod_4_surf .eq. num_quad_sf)   then
          call const_jacobian_sf_grp_quad(mesh%node, mesh%ele,          &
     &        group%surf_grp, jac_sf_grp)
        else if (surf_mesh%surf%nnod_4_surf .eq. num_lag_sf)   then
          call const_jacobian_sf_grp_lag(mesh%node, mesh%ele,           &
     &        group%surf_grp, jac_sf_grp)
        end if
      end if
!
      end subroutine cal_jacobian_surf_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_linear_jac_3d_type(mesh, group, jacobians)
!
      use const_jacobians_3d
      use const_jacobians_infty_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::  group
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call alloc_3d_linear_jac_type(jacobians)
!
      if (mesh%ele%nnod_4_ele .eq. num_t_linear) then
        call link_3d_linear_jac_type(jacobians)
      else
!
!  data allocation
!
        call alloc_jacobians_type(mesh%ele%numele, num_t_linear,        &
     &      maxtot_int_3d, jacobians%jac_3d_l)
        call alloc_dxi_dx_type(mesh%ele%numele, jacobians%jac_3d_l)
!
!  set jacobians
!
        call cal_jacobian_trilinear                                     &
     &     (mesh%node, mesh%ele, jacobians%jac_3d_l)
!
        if (group%infty_grp%ngrp_sf .gt. 0) then
          call cal_jacobian_infty_linear(mesh%node, mesh%ele,           &
     &        group%surf_grp, group%infty_grp, jacobians%jac_3d_l)
        end if
!
        call dealloc_inv_jac_type(jacobians%jac_3d_l)
      end if
!
!
      end subroutine const_linear_jac_3d_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_linear_jac_surf_type(mesh, surf_mesh, jac_2d_l)
!
      use const_jacobians_2d
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(jacobians_2d), intent(inout) :: jac_2d_l
!
!
      if(surf_mesh%surf%nnod_4_surf .ne. num_linear_sf) then
        call alloc_2d_jac_type(surf_mesh%surf%numsurf,                  &
     &      num_linear_sf, maxtot_int_2d, jac_2d_l)
        call cal_jacobian_surface_linear                                &
     &     (mesh%node, surf_mesh%surf, jac_2d_l)
      end if
!
      end subroutine cal_linear_jac_surf_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_linear_jac_edge_type(mesh, edge_mesh, jac_1d_l)
!
      use const_jacobians_1d
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in) :: edge_mesh
      type(jacobians_1d), intent(inout) :: jac_1d_l
!
!
      if (edge_mesh%edge%nnod_4_edge .ne. num_linear_edge) then
        call alloc_1d_jac_type(edge_mesh%edge%numedge, num_linear_edge, &
     &      maxtot_int_1d, jac_1d_l)
        call cal_jacobian_edge_linear                                   &
     &     (mesh%node, edge_mesh%edge, jac_1d_l)
      end if
!
      end subroutine cal_linear_jac_edge_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_linear_jac_surf_grp_type(mesh, surf_mesh, group,   &
     &          jac_sf_grp_l)
!
      use const_jacobians_sf_grp
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::  group
      type(surface_geometry), intent(in) :: surf_mesh
      type(jacobians_2d), intent(inout) :: jac_sf_grp_l
!
!
      if(surf_mesh%surf%nnod_4_surf .ne. num_linear_sf) then
        call const_jacobian_sf_grp_linear(mesh%node, mesh%ele,          &
     &      group%surf_grp, jac_sf_grp_l)
      end if
!
      end subroutine cal_linear_jac_surf_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine empty_jacobian_type(mesh, jac_3d)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
!  data allocation
!
      call alloc_jacobians_type(mesh%ele%numele, mesh%ele%nnod_4_ele,   &
     &    maxtot_int_3d, jac_3d)
      call alloc_dxi_dx_type(mesh%ele%numele, jac_3d)
!
      call dealloc_inv_jac_type(jac_3d)
!
      end subroutine empty_jacobian_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_jacobian_surface_type(surf_mesh, jac_2d)
!
      type(surface_geometry), intent(in) :: surf_mesh
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call alloc_2d_jac_type(surf_mesh%surf%numsurf,                    &
     &    surf_mesh%surf%nnod_4_surf, maxtot_int_2d, jac_2d)
!
      end subroutine empty_jacobian_surface_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_jacobian_surf_grp_type(surf_mesh,                &
     &          group, jac_sf_grp)
!
      type(surface_geometry), intent(in) :: surf_mesh
      type(mesh_groups),      intent(in) :: group
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call alloc_2d_jac_type(group%surf_grp%num_item,                   &
     &    surf_mesh%surf%nnod_4_surf, maxtot_int_2d, jac_sf_grp)
!
      end subroutine empty_jacobian_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_linear_jac_3d_type(mesh, jacobians)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call alloc_3d_linear_jac_type(jacobians)
!
!  data allocation
!
      call alloc_jacobians_type(mesh%ele%numele, num_t_linear,          &
     &    maxtot_int_3d, jacobians%jac_3d_l)
      call alloc_dxi_dx_type(mesh%ele%numele, jacobians%jac_3d_l)
!
      call dealloc_inv_jac_type(jacobians%jac_3d_l)
!
!
      end subroutine empty_linear_jac_3d_type
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_type
