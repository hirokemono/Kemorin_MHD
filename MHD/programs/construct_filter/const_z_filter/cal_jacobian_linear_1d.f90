!
!      module cal_jacobian_linear_1d
!
!        programmed by H. Matsui on June, 2007
!
!      subroutine s_cal_jacobian_linear_1d(num_int,                     &
!     &          node, surf, edge, spf_1d, jacobians)
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
     &          node, surf, edge, spf_1d, jacobians)
!
      use calypso_mpi
      use t_shape_functions
      use t_jacobians
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use m_fem_gauss_int_coefs
      use m_gauss_int_parameters
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
      type(edge_shape_function), intent(inout) :: spf_1d
      type(jacobians_type), intent(inout) :: jacobians
!
!  data allocation
!
      call count_surf_size_smp_type(surf)
      call count_edge_size_smp_type(edge)
!
      call maximum_integration_points(num_int)
      call set_num_of_int_points
!
      call allocate_gauss_coef_4_fem
!
!  set constant for gauss integration with roots
!
      call init_gauss_int_parameters
!
!  set indices for gauss integration
!
      call alloc_1d_gauss_point_id                                      &
     &   (maxtot_int_1d, max_int_point, spf_1d)
      call set_integration_indices_1d_mesh                              &
     &   (maxtot_int_1d, max_int_point, spf_1d%l_int)
!
!  set weighting for integration
!
      call set_gauss_coefs_4_1d(maxtot_int_1d, spf_1d%xi)
      call copy_fem_gauss_int_coef_type(g_FEM1)
!
      allocate(jacobians%g_FEM)
      call copy_fem_gauss_int_coefs(g_FEM1, jacobians%g_FEM)
!
      call alloc_edge_shape_func                                        &
     &   (edge%nnod_4_edge, maxtot_int_1d, spf_1d)
      call const_jacobians_edge                                         &
     &   (my_rank, nprocs, node, edge, spf_1d, jacobians)
!
      end subroutine s_cal_jacobian_linear_1d
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_linear_1d
