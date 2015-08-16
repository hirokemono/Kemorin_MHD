!
!      module set_edge_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine s_cal_edge_vector
!      subroutine s_cal_edge_vector_spherical
!      subroutine s_cal_edge_vector_cylindrical
!
      module set_edge_vectors
!
      use m_precision
!
      use m_geometry_data
      use m_machine_parameter
!
      use m_edge_geometry_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_edge_vector
!
      use int_edge_vector
!
      call allocate_edge_vectors
!
      call s_int_edge_vector
!
      end subroutine s_cal_edge_vector
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_edge_vector_spherical
!
      use cvt_xyz_vector_2_sph_smp
!
!
      call allocate_edge_vector_sph
!
!$omp parallel
      call cvt_vector_2_sph_smp                                         &
     &   (np_smp, edge1%numedge, edge1%istack_edge_smp,                 &
     &    edge_vect, edge_vect_sph, x_edge(1,1), x_edge(1,2),           &
     &    x_edge(1,3), r_edge, s_edge, ar_edge, as_edge)
!$omp end parallel
!
      end subroutine s_cal_edge_vector_spherical
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_edge_vector_cylindrical
!
      use cvt_xyz_vector_2_cyl_smp
!
!
      call allocate_edge_vector_cyl
!
!$omp parallel
      call cvt_vector_2_cyl_smp                                         &
     &   (np_smp, edge1%numedge, edge1%istack_edge_smp,                 &
     &    edge_vect, edge_vect_cyl, x_edge(1,1), x_edge(1,2),           &
     &    s_edge, as_edge)
!$omp end parallel
!
      end subroutine s_cal_edge_vector_cylindrical
!
! -----------------------------------------------------------------------
!
      end module set_edge_vectors
