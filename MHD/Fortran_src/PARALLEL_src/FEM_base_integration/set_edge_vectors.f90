!
!      module set_edge_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine const_edge_vector(node, edge)
!        type(node_data), intent(in) :: node
!        type(edge_data), intent(inout) :: edge
!      subroutine s_cal_edge_vector_spherical(edge)
!      subroutine s_cal_edge_vector_cylindrical(edge)
!
      module set_edge_vectors
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use t_edge_data
      use t_jacobian_1d
!
      implicit none
!
      type(jacobians_1d), save, private :: jac_1d_l
      type(jacobians_1d), save, private :: jac_1d_q
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_edge_vector(node, edge)
!
      use m_fem_gauss_int_coefs
      use const_jacobians_1d
      use int_edge_vector
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(inout) :: edge
!
!
      call cal_jacobian_edge(node, edge, jac_1d_l, jac_1d_q)
!
      call allocate_edge_vect_type(edge)
      call int_edge_vect(edge%numedge, edge%istack_edge_smp,            &
     &      jac_1d_q%ntot_int, max_int_point,                           &
     &      jac_1d_q%xj_edge, jac_1d_q%xeg_edge,                        &
     &      edge%edge_vect, edge%edge_length, edge%a_edge_length)
!
      call dealloc_1d_jac_type(jac_1d_q)
      call dealloc_1d_jac_type(jac_1d_l)
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
      call allocate_edge_vect_sph_type(edge)
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
      call allocate_edge_vect_cyl_type(edge)
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
