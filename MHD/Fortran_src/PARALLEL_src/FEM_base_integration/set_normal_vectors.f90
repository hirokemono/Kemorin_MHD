!
!      module set_normal_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine s_cal_normal_vector
!      subroutine s_cal_normal_vector_spherical
!      subroutine s_cal_normal_vector_cylindrical
!
      module set_normal_vectors
!
      use m_precision
!
      use m_geometry_parameter
      use m_machine_parameter
!
      use m_surface_geometry_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_normal_vector
!
      use m_geometry_data
      use m_jacobians_4_surface
      use m_fem_gauss_int_coefs
      use int_area_normal_4_surface
!
!
      call allocate_normal_vectors
!
      if (nnod_4_ele .eq. num_t_quad) then
        call int_normal_all_surf(numsurf, isurf_smp_stack,              &
     &     ntot_int_2d, max_int_point, xjq_surf, xsq_surf,              &
     &     area_surf, a_area_surf, vnorm_surf)
      else
        call int_normal_all_surf(numsurf, isurf_smp_stack,              &
     &     ntot_int_2d, max_int_point, xj_surf, xsf_surf,               &
     &     area_surf, a_area_surf, vnorm_surf)
      end if
!
      end subroutine s_cal_normal_vector
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_normal_vector_spherical
!
      use cvt_vector_2_spheric_smp
!
!
      call allocate_normal_vector_sph
!
      call cvt_vector_2_sph_smp(np_smp, numsurf, isurf_smp_stack,       &
     &    vnorm_surf, vnorm_surf_sph, x_surf, r_surf, s_surf,           &
     &    ar_surf, as_surf)
!
      end subroutine s_cal_normal_vector_spherical
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_normal_vector_cylindrical
!
      use cvt_vector_2_cylinder_smp
!
!
      call allocate_normal_vector_cyl
!
      call cvt_vector_2_cyl_smp(np_smp, numsurf, isurf_smp_stack,       &
     &    vnorm_surf, vnorm_surf_cyl, x_surf, s_surf, as_surf)
!
      end subroutine s_cal_normal_vector_cylindrical
!
! -----------------------------------------------------------------------
!
      end module set_normal_vectors
