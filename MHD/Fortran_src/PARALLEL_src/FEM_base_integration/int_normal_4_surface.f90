!
!     module int_normal_4_surface
!
!      Written by H. Matsui on Aug., 2006
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine s_int_normal_4_all_surface
!
      module int_normal_4_surface
!
      use m_precision
!
      use m_machine_parameter
      use m_fem_gauss_int_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_normal_4_all_surface
!
      use m_geometry_data
      use m_surface_geometry_data
      use m_jacobians_4_surface
      use int_area_normal_4_surface
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_normal_all_surf(surf1%numsurf, isurf_smp_stack,        &
     &     jac1_2d_q%ntot_int, max_int_point, jac1_2d_q%xj_sf,          &
     &     jac1_2d_q%xsf_sf, area_surf, a_area_surf, vnorm_surf)
      else
        call int_normal_all_surf(surf1%numsurf, isurf_smp_stack,        &
     &     jac1_2d_l%ntot_int, max_int_point, jac1_2d_l%xj_sf,          &
     &     jac1_2d_l%xsf_sf, area_surf, a_area_surf, vnorm_surf)
      end if
!
      end subroutine s_int_normal_4_all_surface
!
! ----------------------------------------------------------------------
!
      end module int_normal_4_surface
