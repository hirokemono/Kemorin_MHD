!
!      module set_normal_vectors
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine const_normal_vector(node, surf)
!      subroutine int_normal_4_all_surface(surf, jac_2d)
!        type(jacobians_2d), intent(in) :: jac_2d
!        type(surface_data), intent(inout) :: surf
!      subroutine s_cal_normal_vector_spherical(surf)
!      subroutine s_cal_normal_vector_cylindrical(surf)
!
      module set_normal_vectors
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
      use t_surface_data
      use t_jacobian_2d
!
      implicit none
!
      type(jacobians_2d), save, private :: jac_2d_l
      type(jacobians_2d), save, private :: jac_2d_q
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_normal_vector(node, surf)
!
      use m_fem_gauss_int_coefs
      use const_jacobians_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(inout) :: surf
!
!
      call cal_jacobian_surface(node, surf, jac_2d_q, jac_2d_l)
      call int_normal_4_all_surface(surf, jac_2d_q)
!
      call dealloc_2d_jac_type(jac_2d_l)
      call dealloc_2d_jac_type(jac_2d_q)
!
      end subroutine const_normal_vector
!
!-----------------------------------------------------------------------
!
      subroutine int_normal_4_all_surface(surf, jac_2d)
!
      use m_fem_gauss_int_coefs
      use int_area_normal_4_surface
!
      type(jacobians_2d), intent(in) :: jac_2d
      type(surface_data), intent(inout) :: surf
!
!
      call allocate_normal_vect_type(surf)
!
      call int_normal_all_surf(surf%numsurf, surf%istack_surf_smp,      &
     &    jac_2d%ntot_int, max_int_point, jac_2d%xj_sf,                 &
     &    jac_2d%xsf_sf, surf%area_surf, surf%a_area_surf,              &
     &    surf%vnorm_surf)
!
      end subroutine int_normal_4_all_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_cal_normal_vector_spherical(surf)
!
      use cvt_xyz_vector_2_sph_smp
!
      type(surface_data), intent(inout) :: surf
!
!
      call allocate_normal_vect_sph_type(surf)
!
!$omp parallel
      call cvt_vector_2_sph_smp                                         &
     &   (np_smp, surf%numsurf, surf%istack_surf_smp,                   &
     &    surf%vnorm_surf, surf%vnorm_surf_sph,                         &
     &    surf%x_surf(1:surf%numsurf,1), surf%x_surf(1:surf%numsurf,2), &
     &    surf%x_surf(1:surf%numsurf,3), surf%r_surf, surf%s_surf,      &
     &    surf%ar_surf, surf%as_surf)
!$omp end parallel
!
      end subroutine s_cal_normal_vector_spherical
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_normal_vector_cylindrical(surf)
!
      use cvt_xyz_vector_2_cyl_smp
!
      type(surface_data), intent(inout) :: surf
!
!
      call allocate_normal_vect_cyl_type(surf)
!
!$omp parallel
      call cvt_vector_2_cyl_smp                                         &
     &   (np_smp, surf%numsurf, surf%istack_surf_smp,                   &
     &    surf%vnorm_surf, surf%vnorm_surf_cyl,                         &
     &    surf%x_surf(1:surf%numsurf,1), surf%x_surf(1:surf%numsurf,2), &
     &    surf%s_surf, surf%as_surf)
!$omp end parallel
!
      end subroutine s_cal_normal_vector_cylindrical
!
! -----------------------------------------------------------------------
!
      end module set_normal_vectors
