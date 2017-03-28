!
!      module cal_jacobian_linear_1d
!
!        programmed by H. Matsui on June, 2007
!
!      subroutine s_cal_jacobian_linear_1d(num_int,                     &
!     &          node, ele, surf, edge, jacobians)
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
     &          node, ele, surf, edge, jacobians)
!
      use calypso_mpi
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
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout)  :: surf
      type(edge_data), intent(inout)  :: edge
      type(jacobians_type), intent(inout) :: jacobians
!
!  data allocation
!
      call count_surf_size_smp_type(surf)
      call count_edge_size_smp_type(edge)
!
      call maximum_integration_points(num_int)
      call allocate_integrate_parameters
!
      call allocate_gauss_coef_4_fem
!
!  set constant for gauss integration with roots
!
      call init_gauss_int_parameters
!
!  set indices for gauss integration
!
      call set_integration_indices_1d_mesh
!
!  set weighting for integration
!
      call set_gauss_coefs_4_1d
!
      call const_jacobians_edge(my_rank, nprocs, node, edge, jacobians)
!
      end subroutine s_cal_jacobian_linear_1d
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_linear_1d
