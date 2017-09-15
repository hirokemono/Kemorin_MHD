!
!      module set_edge_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine const_edge_vector                                    &
!!     &         (my_rank, nprocs, node, edge, spf_1d, jacobians)
!!        type(node_data), intent(in) :: node
!!        type(edge_data), intent(inout) :: edge
!!        type(edge_shape_function), intent(inout) :: spf_1d
!!        type(jacobians_type), intent(inout) :: jacobians
!!      subroutine s_cal_edge_vector_spherical(edge)
!!      subroutine s_cal_edge_vector_cylindrical(edge)
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
     &         (my_rank, nprocs, node, edge, spf_1d, jacobians)
!
      use m_fem_gauss_int_coefs
      use int_edge_vector
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(edge_data), intent(inout) :: edge
      type(edge_shape_function), intent(inout) :: spf_1d
      type(jacobians_type), intent(inout) :: jacobians
!
!
      call alloc_edge_shape_func                                        &
     &   (num_linear_edge, maxtot_int_1d, spf_1d)
      call const_jacobians_edge                                         &
     &   (my_rank, nprocs, node, edge, spf_1d, jacobians)
      call dealloc_edge_shape_func(spf_1d)
!
      call alloc_edge_vect(edge)
      call s_int_edge_vector                                            &
     &   (max_int_point, g_FEM1, jacobians%jac_1d, edge)
!
      call dealloc_jacobians_edge(edge, jacobians)
!
      end subroutine const_edge_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_cal_edge_vector_spherical(edge)
!
      use cvt_xyz_vector_2_sph_smp
!
      type(edge_data), intent(inout) :: edge
!
!
      call alloc_edge_vect_sph(edge)
!
!$omp parallel
      call cvt_vector_2_sph_smp                                         &
     &   (np_smp, edge%numedge, edge%istack_edge_smp,                   &
     &    edge%edge_vect, edge%edge_vect_sph,                           &
     &    edge%x_edge(1:edge%numedge,1), edge%x_edge(1:edge%numedge,2), &
     &    edge%x_edge(1:edge%numedge,3), edge%r_edge, edge%s_edge,      &
     &    edge%ar_edge, edge%as_edge)
!$omp end parallel
!
      end subroutine s_cal_edge_vector_spherical
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_edge_vector_cylindrical(edge)
!
      use cvt_xyz_vector_2_cyl_smp
!
      type(edge_data), intent(inout) :: edge
!
!
      call alloc_edge_vect_cyl(edge)
!
!$omp parallel
      call cvt_vector_2_cyl_smp                                         &
     &   (np_smp, edge%numedge, edge%istack_edge_smp,                   &
     &    edge%edge_vect, edge%edge_vect_cyl,                           &
     &    edge%x_edge(1:edge%numedge,1), edge%x_edge(1:edge%numedge,2), &
     &    edge%s_edge, edge%as_edge)
!$omp end parallel
!
      end subroutine s_cal_edge_vector_cylindrical
!
! -----------------------------------------------------------------------
!
      end module set_edge_vectors
