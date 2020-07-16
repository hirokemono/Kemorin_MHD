!
!      module set_edge_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine const_edge_vector                                    &
!!     &         (id_rank, nprocs, node, edge, spf_1d, jacs)
!!        type(node_data), intent(in) :: node
!!        type(edge_data), intent(inout) :: edge
!!        type(edge_shape_function), intent(inout) :: spf_1d
!!        type(jacobians_type), intent(inout) :: jacs
!
      module set_edge_vectors
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_edge_data
      use t_shape_functions
      use t_jacobians
      use t_jacobian_1d
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_edge_vector                                      &
     &         (id_rank, nprocs, node, edge, spf_1d, jacs)
!
      use int_edge_vector
!
      integer, intent(in) :: id_rank, nprocs
      type(node_data), intent(in) :: node
      type(edge_data), intent(inout) :: edge
      type(edge_shape_function), intent(inout) :: spf_1d
      type(jacobians_type), intent(inout) :: jacs
!
!
      call alloc_edge_shape_func(num_linear_edge, jacs%g_FEM, spf_1d)
      call const_jacobians_edge                                         &
     &   (id_rank, nprocs, node, edge, spf_1d, jacs)
      call dealloc_edge_shape_func(spf_1d)
!
      call alloc_edge_vect(edge)
      call s_int_edge_vector                                            &
     &   (jacs%g_FEM%max_int_point, jacs%g_FEM, jacs%jac_1d, edge)
!
      call dealloc_jacobians_edge(edge, jacs)
!
      end subroutine const_edge_vector
!
! -----------------------------------------------------------------------
!
      end module set_edge_vectors
