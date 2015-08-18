!
!      module set_normal_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine s_cal_normal_vector
!      subroutine s_cal_normal_vector_spherical
!      subroutine s_cal_normal_vector_cylindrical
!
!      subroutine deallocate_normal_vectors
!      subroutine deallocate_normal_vector_sph
!      subroutine deallocate_normal_vector_cyl
!
      module set_normal_vectors
!
      use m_precision
!
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
      subroutine s_cal_normal_vector
!
      use m_geometry_data
      use m_jacobians_4_surface
      use m_fem_gauss_int_coefs
      use int_area_normal_4_surface
!
!
      call allocate_normal_vect_type(surf1)
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_normal_all_surf(surf1%numsurf, surf1%istack_surf_smp,  &
     &     jac1_2d_q%ntot_int, max_int_point,                           &
     &     jac1_2d_q%xj_sf, jac1_2d_q%xsf_sf,                           &
     &     surf1%area_surf, surf1%a_area_surf, surf1%vnorm_surf)
      else
        call int_normal_all_surf(surf1%numsurf, surf1%istack_surf_smp,  &
     &     jac1_2d_l%ntot_int, max_int_point,                           &
     &     jac1_2d_l%xj_sf, jac1_2d_l%xsf_sf,                           &
     &     surf1%area_surf, surf1%a_area_surf, surf1%vnorm_surf)
      end if
!
      end subroutine s_cal_normal_vector
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_normal_vector_spherical
!
      use m_geometry_data
      use cvt_xyz_vector_2_sph_smp
!
!
      call allocate_normal_vect_sph_type(surf1)
!
!$omp parallel
      call cvt_vector_2_sph_smp                                         &
     &   (np_smp, surf1%numsurf, surf1%istack_surf_smp,                 &
     &    surf1%vnorm_surf, surf1%vnorm_surf_sph,                       &
     &    surf1%x_surf(1:surf1%numsurf,1),                              &
     &    surf1%x_surf(1:surf1%numsurf,2),                              &
     &    surf1%x_surf(1:surf1%numsurf,3),                              &
     &    surf1%r_surf, surf1%s_surf, surf1%ar_surf, surf1%as_surf)
!$omp end parallel
!
      end subroutine s_cal_normal_vector_spherical
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_normal_vector_cylindrical
!
      use m_geometry_data
      use cvt_xyz_vector_2_cyl_smp
!
!
      call allocate_normal_vect_cyl_type(surf1)
!
!$omp parallel
      call cvt_vector_2_cyl_smp                                         &
     &   (np_smp, surf1%numsurf, surf1%istack_surf_smp,                 &
     &    surf1%vnorm_surf, surf1%vnorm_surf_cyl,                       &
     &    surf1%x_surf(1:surf1%numsurf,1),                              &
     &    surf1%x_surf(1:surf1%numsurf,2), surf1%s_surf, surf1%as_surf)
!$omp end parallel
!
      end subroutine s_cal_normal_vector_cylindrical
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_normal_vectors
!
      use m_geometry_data
!
      call deallocate_normal_vect_type(surf1)
!
      end subroutine deallocate_normal_vectors
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_normal_vector_sph
!
      use m_geometry_data
!
      call deallocate_normal_vect_sph_type(surf1)
!
      end subroutine deallocate_normal_vector_sph
!
! ------------------------------------------------------
!
      subroutine deallocate_normal_vector_cyl
!
      use m_geometry_data
!
      call deallocate_normal_vect_cyl_type(surf1)
!
      end subroutine deallocate_normal_vector_cyl
!
! ------------------------------------------------------
!
      end module set_normal_vectors
