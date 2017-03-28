!
!     module cal_jacobians_type
!
!      Written by H. Matsui on Dec., 2008
!
!      subroutine const_jacobian_surface_type(mesh, ele_mesh, jac_2d)
!        type(mesh_geometry), intent(in) :: mesh
!        type(element_geometry), intent(in) :: ele_mesh
!        type(jacobians_2d), intent(inout) :: jac_2d
!      subroutine const_jacobian_edge_type(mesh, ele_mesh, jac_1d)
!        type(element_geometry), intent(in) :: ele_mesh
!        type(jacobians_1d), intent(inout) :: jac_1d
!
!      subroutine cal_linear_jac_surf_type(mesh, ele_mesh, jac_2d_l)
!        type(mesh_geometry), intent(in) :: mesh
!        type(element_geometry), intent(in) :: ele_mesh
!        type(jacobians_2d), intent(inout) :: jac_2d_l
!
!      subroutine cal_linear_jac_edge_type(mesh, ele_mesh, jac_1d_l)
!        type(mesh_geometry), intent(in) :: mesh
!        type(element_geometry), intent(in) :: ele_mesh
!        type(jacobians_1d), intent(inout) :: jac_1d_l
!
!      subroutine empty_jacobian_surface_type(ele_mesh, jac_2d)
!        type(element_geometry), intent(in) :: ele_mesh
!        type(jacobians_2d), intent(inout) :: jac_2d
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
      private :: const_jacobian_type, const_linear_jac_3d_type
      private :: cal_jacobian_surf_grp_type
      private :: cal_linear_jac_surf_grp_type
      private :: empty_jacobian_type
      private :: empty_jacobian_surf_grp_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_element                                &
     &          (my_rank, nprocs, mesh, group, jacobians)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_3d)
      if(my_rank .lt. nprocs) then
        call const_jacobian_type(mesh, group, jacobians%jac_3d)
      else
        call empty_jacobian_type(mesh,jacobians%jac_3d)
      end if
!
      if(mesh%ele%nnod_4_ele .eq. num_t_linear) then
        jacobians%jac_3d_l => jacobians%jac_3d
      else if(my_rank .lt. nprocs) then
        allocate(jacobians%jac_3d_l)
        call const_linear_jac_3d_type(mesh, group, jacobians%jac_3d_l)
      else
        allocate(jacobians%jac_3d_l)
        call empty_jacobian_type(mesh,jacobians%jac_3d_l)
      end if
!
      end subroutine const_jacobians_element
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_surf_group                             &
     &          (my_rank, nprocs, mesh, ele_mesh, group, jacobians)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_groups), intent(in) :: group
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_sf_grp)
      if(my_rank .lt. nprocs) then
        call cal_jacobian_surf_grp_type                                 &
     &     (mesh, ele_mesh, group, jacobians%jac_sf_grp)
      else
        call empty_jacobian_surf_grp_type                               &
     &     (ele_mesh, group, jacobians%jac_sf_grp)
       end if
!
      if(mesh%ele%nnod_4_ele .eq. num_t_linear) then
        jacobians%jac_sf_grp_l => jacobians%jac_sf_grp
      else if(my_rank .lt. nprocs) then
        allocate(jacobians%jac_sf_grp_l)
        call cal_linear_jac_surf_grp_type(mesh, ele_mesh, group,        &
     &      jacobians%jac_sf_grp_l)
      else
        allocate(jacobians%jac_sf_grp_l)
        call empty_jacobian_surf_grp_type                               &
     &     (ele_mesh, group, jacobians%jac_sf_grp_l)
      end if
!
      end subroutine const_jacobians_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_surface                                &
     &          (my_rank, nprocs, mesh, ele_mesh, jacobians)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_2d)
      if(my_rank .lt. nprocs) then
        call const_jacobian_surface_type                                &
     &     (mesh, ele_mesh, jacobians%jac_2d)
      else
        call empty_jacobian_surface_type(ele_mesh, jacobians%jac_2d)
      end if
!
      if(mesh%ele%nnod_4_ele .eq. num_t_linear) then
        jacobians%jac_2d_l => jacobians%jac_2d
      else if(my_rank .lt. nprocs) then
        allocate(jacobians%jac_2d_l)
        call cal_linear_jac_surf_type                                   &
     &     (mesh, ele_mesh, jacobians%jac_2d_l)
      else
        allocate(jacobians%jac_2d_l)
        call empty_jacobian_surface_type(ele_mesh, jacobians%jac_2d_l)
      end if
!
      end subroutine const_jacobians_surface
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_edge                                   &
     &         (my_rank, nprocs, mesh, ele_mesh, jacobians)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_1d)
      if(my_rank .lt. nprocs) then
        call const_jacobian_edge_type                                   &
     &     (mesh, ele_mesh, jacobians%jac_1d)
      else
        call empty_jacobian_edge_type(ele_mesh, jacobians%jac_1d)
      end if
!
      if(mesh%ele%nnod_4_ele .eq. num_t_linear) then
        jacobians%jac_1d_l => jacobians%jac_1d
      else if(my_rank .lt. nprocs) then
        allocate(jacobians%jac_1d_l)
        call cal_linear_jac_edge_type                                   &
     &     (mesh, ele_mesh, jacobians%jac_1d_l)
      else
        allocate(jacobians%jac_1d_l)
        call empty_jacobian_edge_type(ele_mesh, jacobians%jac_1d_l)
      end if
!
      end subroutine const_jacobians_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_type(mesh, group, jac_3d)
!
      use const_jacobians_3d
      use const_jacobians_infinity
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
      subroutine const_jacobian_surface_type(mesh, ele_mesh, jac_2d)
!
      use const_jacobians_2d
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call alloc_2d_jac_type(ele_mesh%surf%numsurf,                     &
     &    ele_mesh%surf%nnod_4_surf, maxtot_int_2d, jac_2d)
!
      if      (ele_mesh%surf%nnod_4_surf .eq. num_linear_sf) then
        call cal_jacobian_surface_linear                                &
     &     (mesh%node, ele_mesh%surf, jac_2d)
      else if (ele_mesh%surf%nnod_4_surf .eq. num_quad_sf)   then
        call cal_jacobian_surface_quad                                  &
     &     (mesh%node, ele_mesh%surf, jac_2d)
      else if (ele_mesh%surf%nnod_4_surf .eq. num_lag_sf)   then
        call cal_jacobian_surface_lag                                   &
     &     (mesh%node, ele_mesh%surf, jac_2d)
      end if
!
      end subroutine const_jacobian_surface_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_edge_type(mesh, ele_mesh, jac_1d)
!
      use const_jacobians_1d
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call alloc_1d_jac_type(ele_mesh%edge%numedge,                     &
     &    ele_mesh%edge%nnod_4_edge, maxtot_int_1d, jac_1d)
!
      if      (ele_mesh%edge%nnod_4_edge .eq. num_linear_edge) then
        call cal_jacobian_edge_linear                                   &
     &     (mesh%node, ele_mesh%edge, jac_1d)
      else if (ele_mesh%edge%nnod_4_edge .eq. num_quad_edge) then
        call cal_jacobian_edge_quad(mesh%node, ele_mesh%edge, jac_1d)
      end if
!
      end subroutine const_jacobian_edge_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surf_grp_type(mesh, ele_mesh,             &
     &          group, jac_sf_grp)
!
      use const_jacobians_sf_grp
!
      type(mesh_geometry),    intent(in) :: mesh
      type(mesh_groups),      intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call alloc_2d_jac_type(group%surf_grp%num_item,                   &
     &    ele_mesh%surf%nnod_4_surf, maxtot_int_2d, jac_sf_grp)
!
      if (group%surf_grp%num_grp .gt. 0) then
        if      (ele_mesh%surf%nnod_4_surf .eq. num_linear_sf) then
          call const_jacobian_sf_grp_linear(mesh%node, mesh%ele,        &
     &        group%surf_grp, jac_sf_grp)
        else if (ele_mesh%surf%nnod_4_surf .eq. num_quad_sf)   then
          call const_jacobian_sf_grp_quad(mesh%node, mesh%ele,          &
     &        group%surf_grp, jac_sf_grp)
        else if (ele_mesh%surf%nnod_4_surf .eq. num_lag_sf)   then
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
      subroutine const_linear_jac_3d_type(mesh, group, jac_3d_l)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::  group
      type(jacobians_3d), intent(inout) :: jac_3d_l
!
!
      call alloc_jacobians_type(mesh%ele%numele, num_t_linear,          &
     &      maxtot_int_3d, jac_3d_l)
      call alloc_dxi_dx_type(mesh%ele%numele, jac_3d_l)
!
!  set jacobians
!
      call cal_jacobian_trilinear                                       &
     &     (mesh%node, mesh%ele, jac_3d_l)
!
      if (group%infty_grp%ngrp_sf .gt. 0) then
        call cal_jacobian_infty_linear(mesh%node, mesh%ele,             &
     &        group%surf_grp, group%infty_grp, jac_3d_l)
      end if
!
      call dealloc_inv_jac_type(jac_3d_l)
!
      end subroutine const_linear_jac_3d_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_linear_jac_surf_type(mesh, ele_mesh, jac_2d_l)
!
      use const_jacobians_2d
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_2d), intent(inout) :: jac_2d_l
!
!
      if(ele_mesh%surf%nnod_4_surf .eq. num_linear_sf) return
!
      call alloc_2d_jac_type(ele_mesh%surf%numsurf,                     &
     &    num_linear_sf, maxtot_int_2d, jac_2d_l)
      call cal_jacobian_surface_linear                                  &
     &   (mesh%node, ele_mesh%surf, jac_2d_l)
!
      end subroutine cal_linear_jac_surf_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_linear_jac_edge_type(mesh, ele_mesh, jac_1d_l)
!
      use const_jacobians_1d
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_1d), intent(inout) :: jac_1d_l
!
!
      if (ele_mesh%edge%nnod_4_edge .eq. num_linear_edge) return
!
      call alloc_1d_jac_type(ele_mesh%edge%numedge, num_linear_edge,    &
     &    maxtot_int_1d, jac_1d_l)
      call cal_jacobian_edge_linear                                     &
     &   (mesh%node, ele_mesh%edge, jac_1d_l)
!
      end subroutine cal_linear_jac_edge_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_linear_jac_surf_grp_type(mesh, ele_mesh, group,    &
     &          jac_sf_grp_l)
!
      use const_jacobians_sf_grp
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::  group
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_2d), intent(inout) :: jac_sf_grp_l
!
!
      if(ele_mesh%surf%nnod_4_surf .ne. num_linear_sf) then
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
      subroutine empty_jacobian_surf_grp_type(ele_mesh,                 &
     &          group, jac_sf_grp)
!
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_groups),      intent(in) :: group
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call alloc_2d_jac_type(group%surf_grp%num_item,                   &
     &    ele_mesh%surf%nnod_4_surf, maxtot_int_2d, jac_sf_grp)
!
      end subroutine empty_jacobian_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_jacobian_surface_type(ele_mesh, jac_2d)
!
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call alloc_2d_jac_type(ele_mesh%surf%numsurf,                     &
     &    ele_mesh%surf%nnod_4_surf, maxtot_int_2d, jac_2d)
!
      end subroutine empty_jacobian_surface_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_jacobian_edge_type(ele_mesh, jac_1d)
!
      type(element_geometry), intent(in) :: ele_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call alloc_1d_jac_type(ele_mesh%edge%numedge,                     &
     &    ele_mesh%edge%nnod_4_edge, maxtot_int_1d, jac_1d)
!
      end subroutine empty_jacobian_edge_type
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_type
