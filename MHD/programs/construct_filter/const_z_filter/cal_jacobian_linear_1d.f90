!
!      module cal_jacobian_linear_1d
!
!        programmed by H. Matsui on June, 2007
!
!!      subroutine s_cal_jacobian_linear_1d(num_int,                    &
!!     &          node, surf, edge, spf_1d, jacs)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(inout)  :: surf
!!        type(edge_data), intent(inout)  :: edge
!!        type(edge_shape_function), intent(inout) :: spf_1d
!!        type(jacobians_type), intent(inout) :: jacs
!
      module cal_jacobian_linear_1d
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_linear_1d(num_int,                      &
     &          node, surf, edge, spf_1d, jacs)
!
      use calypso_mpi
      use t_shape_functions
      use t_jacobians
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      integer(kind = kint), intent(in) :: num_int
      type(node_data), intent(in) :: node
!
      type(surface_data), intent(inout)  :: surf
      type(edge_data), intent(inout)  :: edge
      type(edge_shape_function), intent(inout) :: spf_1d
      type(jacobians_type), intent(inout) :: jacs
!
!
      allocate(jacs%g_FEM)
      call init_jacobian_linear_1d(num_int,                             &
     &    node, surf, edge, jacs%g_FEM, spf_1d)
!
      call alloc_edge_shape_func(edge%nnod_4_edge, jacs%g_FEM, spf_1d)
      call const_jacobians_edge                                         &
     &   (my_rank, nprocs, node, edge, spf_1d, jacs)
!
      end subroutine s_cal_jacobian_linear_1d
!
!-----------------------------------------------------------------------
!
      subroutine init_jacobian_linear_1d(num_int,                       &
     &          node, surf, edge, g_FEM, spf_1d)
!
      use calypso_mpi
      use m_gauss_int_parameters
      use t_fem_gauss_int_coefs
      use t_shape_functions
      use t_jacobians
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use set_size_4_smp_types
      use set_integration_indices
      use set_gauss_int_parameters
      use const_jacobians_1d
!
      integer(kind = kint), intent(in) :: num_int
      type(node_data), intent(in) :: node
!
      type(surface_data), intent(inout)  :: surf
      type(edge_data), intent(inout)  :: edge
      type(FEM_gauss_int_coefs), intent(inout) :: g_FEM
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
!  set constant for gauss integration with roots
!
      call init_gauss_int_parameters
!
!  data allocation
!
      call count_surf_size_smp_type(surf)
      call count_edge_size_smp_type(edge)
!
      call set_max_integration_points(num_int, g_FEM)
      call num_of_int_points(g_FEM)
!
!  set indices for gauss integration
!
      call alloc_1d_gauss_point_id(g_FEM, spf_1d)
      call set_integration_indices_1d_mesh                              &
     &   (g_FEM%maxtot_int_1d, g_FEM%max_int_point, spf_1d%l_int)
!
!  set weighting for integration
!
      call alloc_gauss_coef_4_fem(g_FEM)
      call set_start_addres_4_FEM_int(g_FEM)
!
      call set_gauss_coefs_4_1d                                         &
     &   (g_FEM%max_int_point, g_FEM%maxtot_int_1d, g_FEM%int_start1,   &
     &    spf_1d%xi, g_FEM%owe)
!
      end subroutine init_jacobian_linear_1d
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_linear_1d
