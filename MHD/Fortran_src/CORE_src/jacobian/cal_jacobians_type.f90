!
!     module cal_jacobians_type
!
!      Written by H. Matsui on Dec., 2008
!
!!      subroutine const_jacobians_element(my_rank, nprocs,             &
!!     &          node, ele, surf_grp, infinity_list, jacobians)
!!      subroutine const_jacobians_surf_group                           &
!!     &         (my_rank, nprocs, node, ele, surf, surf_grp, jacobians)
!!      subroutine const_jacobians_surface                              &
!!     &          (my_rank, nprocs, node, surf, jacobians)
!!      subroutine const_jacobians_edge                                 &
!!     &         (my_rank, nprocs, node, edge, jacobians)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in)  :: surf
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(scalar_surf_BC_list), intent(in) :: infinity_list
!!        type(jacobians_type), intent(inout) :: jacobians
!
      module cal_jacobians_type
!
      use m_precision
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_jacobians
      use t_jacobian_3d
      use t_jacobian_2d
      use t_jacobian_1d
!
      implicit  none
!
      private :: const_jacobian_type, const_linear_jac_3d_type
      private :: cal_jacobian_surf_grp_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_element(my_rank, nprocs,               &
     &          node, ele, surf_grp, infinity_list, jacobians)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_3d)
      call alloc_jacobians_type(ele%numele, ele%nnod_4_ele,             &
     &    maxtot_int_3d, jacobians%jac_3d)
      call alloc_dxi_dx_type(ele%numele, jacobians%jac_3d)
!
      if(my_rank .lt. nprocs) then
        call const_jacobian_type                                        &
     &     (node, ele, surf_grp, infinity_list, jacobians%jac_3d)
      end if
      call dealloc_inv_jac_type(jacobians%jac_3d)
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        jacobians%jac_3d_l => jacobians%jac_3d
      else
        allocate(jacobians%jac_3d_l)
        call alloc_jacobians_type(ele%numele, num_t_linear,             &
     &      maxtot_int_3d, jacobians%jac_3d_l)
        call alloc_dxi_dx_type(ele%numele, jacobians%jac_3d_l)
!
       if(my_rank .lt. nprocs) then
          call const_linear_jac_3d_type                                 &
     &       (node, ele, surf_grp, infinity_list, jacobians%jac_3d_l)
        end if
!
        call dealloc_inv_jac_type(jacobians%jac_3d_l)
      end if
!
      end subroutine const_jacobians_element
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_surf_group                             &
     &         (my_rank, nprocs, node, ele, surf, surf_grp, jacobians)
!
      use const_jacobians_sf_grp
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in)  :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_sf_grp)
      call alloc_2d_jac_type(surf_grp%num_item,                         &
     &    surf%nnod_4_surf, maxtot_int_2d, jacobians%jac_sf_grp)
!
      if(my_rank .lt. nprocs) then
        call cal_jacobian_surf_grp_type                                 &
     &     (node, ele, surf, surf_grp, jacobians%jac_sf_grp)
       end if
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        jacobians%jac_sf_grp_l => jacobians%jac_sf_grp
      else
        allocate(jacobians%jac_sf_grp_l)
        call alloc_2d_jac_type(surf_grp%num_item, num_linear_sf,        &
     &      maxtot_int_2d, jacobians%jac_sf_grp_l)
!
        if(my_rank .lt. nprocs) then
          call const_jacobian_sf_grp_linear(node, ele, surf_grp,        &
     &        jacobians%jac_sf_grp_l)
        end if
      end if
!
      end subroutine const_jacobians_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_surface                                &
     &          (my_rank, nprocs, node, surf, jacobians)
!
      use const_jacobians_2d
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_2d)
      call alloc_2d_jac_type(surf%numsurf,                              &
     &    surf%nnod_4_surf, maxtot_int_2d, jacobians%jac_2d)
!
      if(my_rank .lt. nprocs) then
        call const_jacobian_surface_type(node, surf, jacobians%jac_2d)
      end if
!
      if(surf%nnod_4_surf .eq. num_linear_sf) then
        jacobians%jac_2d_l => jacobians%jac_2d
      else
        allocate(jacobians%jac_2d_l)
        call alloc_2d_jac_type(surf%numsurf, num_linear_sf,             &
     &      maxtot_int_2d, jacobians%jac_2d_l)
        if(my_rank .lt. nprocs) then
          call cal_jacobian_surface_linear                              &
     &       (node, surf, jacobians%jac_2d_l)
        end if
      end if
!
      end subroutine const_jacobians_surface
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobians_edge                                   &
     &         (my_rank, nprocs, node, edge, jacobians)
!
      use const_jacobians_1d
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(jacobians_type), intent(inout) :: jacobians
!
!
      allocate(jacobians%jac_1d)
      call alloc_1d_jac_type(edge%numedge, edge%nnod_4_edge,            &
     &    maxtot_int_1d, jacobians%jac_1d)
!
      if(my_rank .lt. nprocs) then
        call const_jacobian_edge_type(node, edge, jacobians%jac_1d)
      end if
!
      if(edge%nnod_4_edge .eq. num_linear_edge) then
        jacobians%jac_1d_l => jacobians%jac_1d
      else
        allocate(jacobians%jac_1d_l)
        call alloc_1d_jac_type(edge%numedge, num_linear_edge,           &
     &      maxtot_int_1d, jacobians%jac_1d_l)
        if(my_rank .lt. nprocs) then
          call cal_jacobian_edge_linear(node, edge, jacobians%jac_1d_l)
        end if
      end if
!
      end subroutine const_jacobians_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_type                                    &
     &         (node, ele, surf_grp, infinity_list, jac_3d)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
      type(jacobians_3d), intent(inout) :: jac_3d
!
!  set jacobians
!
      if (ele%nnod_4_ele .eq. num_t_linear) then
        call cal_jacobian_trilinear(node, ele, jac_3d)
!
        if (infinity_list%ngrp_sf .gt. 0) then
        call cal_jacobian_infty_linear                                  &
     &     (node, ele, surf_grp, infinity_list, jac_3d)
        end if
!
      else if (ele%nnod_4_ele .eq. num_t_quad) then
        call cal_jacobian_quad(node, ele, jac_3d)
!
        if (infinity_list%ngrp_sf .gt. 0) then
        call cal_jacobian_infty_quad                                    &
     &     (node, ele, surf_grp, infinity_list, jac_3d)
        end if
!
      else if (ele%nnod_4_ele .eq. num_t_lag) then
        call cal_jacobian_lag(node, ele, jac_3d)
!
        if (infinity_list%ngrp_sf .gt. 0) then
          call cal_jacobian_infty_lag                                   &
     &      (node, ele, surf_grp, infinity_list, jac_3d)
        end if
      end if
!
      end subroutine const_jacobian_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_surface_type(node, surf, jac_2d)
!
      use const_jacobians_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      if      (surf%nnod_4_surf .eq. num_linear_sf) then
        call cal_jacobian_surface_linear(node, surf, jac_2d)
      else if (surf%nnod_4_surf .eq. num_quad_sf)   then
        call cal_jacobian_surface_quad(node, surf, jac_2d)
      else if (surf%nnod_4_surf .eq. num_lag_sf)   then
        call cal_jacobian_surface_lag(node, surf, jac_2d)
      end if
!
      end subroutine const_jacobian_surface_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_edge_type(node, edge, jac_1d)
!
      use const_jacobians_1d
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      if      (edge%nnod_4_edge .eq. num_linear_edge) then
        call cal_jacobian_edge_linear(node, edge, jac_1d)
      else if (edge%nnod_4_edge .eq. num_quad_edge) then
        call cal_jacobian_edge_quad(node, edge, jac_1d)
      end if
!
      end subroutine const_jacobian_edge_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surf_grp_type(node, ele, surf,            &
     &          surf_grp, jac_sf_grp)
!
      use const_jacobians_sf_grp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      if (surf_grp%num_grp .gt. 0) then
        if      (surf%nnod_4_surf .eq. num_linear_sf) then
          call const_jacobian_sf_grp_linear(node, ele,                  &
     &        surf_grp, jac_sf_grp)
        else if (surf%nnod_4_surf .eq. num_quad_sf)   then
          call const_jacobian_sf_grp_quad(node, ele,                    &
     &        surf_grp, jac_sf_grp)
        else if (surf%nnod_4_surf .eq. num_lag_sf)   then
          call const_jacobian_sf_grp_lag(node, ele,                     &
     &        surf_grp, jac_sf_grp)
        end if
      end if
!
      end subroutine cal_jacobian_surf_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_linear_jac_3d_type                               &
     &         (node, ele, surf_grp, infinity_list, jac_3d_l)
!
      use const_jacobians_3d
      use const_jacobians_infinity
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(in) :: infinity_list
      type(jacobians_3d), intent(inout) :: jac_3d_l
!
!  set jacobians
!
      call cal_jacobian_trilinear(node, ele, jac_3d_l)
!
      if(infinity_list%ngrp_sf .gt. 0) then
        call cal_jacobian_infty_linear(node, ele,                       &
     &      surf_grp, infinity_list, jac_3d_l)
      end if
!
      end subroutine const_linear_jac_3d_type
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_type
