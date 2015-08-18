!
!      module set_edge_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine s_cal_edge_vector
!      subroutine s_cal_edge_vector_spherical
!      subroutine s_cal_edge_vector_cylindrical
!
!      subroutine deallocate_edge_vectors
!      subroutine deallocate_edge_vector_sph
!      subroutine deallocate_edge_vector_cyl
!
      module set_edge_vectors
!
      use m_precision
!
      use m_geometry_data
      use m_machine_parameter
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
      call allocate_edge_vect_type(edge1)
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
      call allocate_edge_vect_sph_type(edge1)
!
!$omp parallel
      call cvt_vector_2_sph_smp                                         &
     &   (np_smp, edge1%numedge, edge1%istack_edge_smp,                 &
     &    edge1%edge_vect, edge1%edge_vect_sph,                         &
     &    edge1%x_edge(1:edge1%numedge,1),                              &
     &    edge1%x_edge(1:edge1%numedge,2),                              &
     &    edge1%x_edge(1:edge1%numedge,3),                              &
     &    edge1%r_edge, edge1%s_edge, edge1%ar_edge, edge1%as_edge)
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
      call allocate_edge_vect_cyl_type(edge1)
!
!$omp parallel
      call cvt_vector_2_cyl_smp                                         &
     &   (np_smp, edge1%numedge, edge1%istack_edge_smp,                 &
     &    edge1%edge_vect, edge1%edge_vect_cyl,                         &
     &    edge1%x_edge(1:edge1%numedge,1),                              &
     &    edge1%x_edge(1:edge1%numedge,2), edge1%s_edge, edge1%as_edge)
!$omp end parallel
!
      end subroutine s_cal_edge_vector_cylindrical
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_edge_vectors
!
      call deallocate_edge_vect_type(edge1)
!
      end subroutine deallocate_edge_vectors
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_edge_vector_sph
!
      call deallocate_edge_vect_sph_type(edge1)
!
      end subroutine deallocate_edge_vector_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_edge_vector_cyl
!
      call deallocate_edge_vect_cyl_type(edge1)
!
      end subroutine deallocate_edge_vector_cyl
!
! -----------------------------------------------------------------------
!
      end module set_edge_vectors
