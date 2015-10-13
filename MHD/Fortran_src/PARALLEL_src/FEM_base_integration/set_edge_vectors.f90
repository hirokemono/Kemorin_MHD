!
!      module set_edge_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine s_cal_edge_vector(edge, jac_1d_q, jac_1d_l)
!      subroutine s_cal_edge_vector_spherical(edge)
!      subroutine s_cal_edge_vector_cylindrical(edge)
!
      module set_edge_vectors
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use t_edge_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_edge_vector(edge, jac_1d_q, jac_1d_l)
!
      use t_jacobian_1d
      use m_fem_gauss_int_coefs
      use int_edge_vector
!
      type(edge_data), intent(inout) :: edge
      type(jacobians_1d), intent(in) :: jac_1d_q, jac_1d_l
!
!
      call allocate_edge_vect_type(edge)
!
      if(edge%nnod_4_edge .eq. num_quad_edge) then
        call int_edge_vect(edge%numedge, edge%istack_edge_smp,          &
     &      jac_1d_q%ntot_int, max_int_point,                           &
     &      jac_1d_q%xj_edge, jac_1d_q%xeg_edge,                        &
     &      edge%edge_vect, edge%edge_length, edge%a_edge_length)
      else
        call int_edge_vect(edge%numedge, edge%istack_edge_smp,          &
     &      jac_1d_l%ntot_int, max_int_point,                           &
     &      jac_1d_l%xj_edge, jac_1d_l%xeg_edge,                        &
     &      edge%edge_vect, edge%edge_length, edge%a_edge_length)
      end if
!
      end subroutine s_cal_edge_vector
!
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
