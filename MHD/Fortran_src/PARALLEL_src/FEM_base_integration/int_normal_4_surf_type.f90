!
!     module int_normal_4_surf_type
!
!      Written by H. Matsui on Aug., 2006
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine s_int_normal_4_all_surf_type(surf, jac_2d)
!        type(jacobians_2d), intent(in) :: jac_2d
!        type(surface_data), intent(inout) :: surf
!
!      subroutine sum_normal_4_surf_grp_type(ele, sf_grp, sf_grp_v)
!        type(element_data),       intent(in) :: ele
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_group_geometry), intent(inout) :: sf_grp_v
!
      module int_normal_4_surf_type
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_normal_4_all_surf_type(surf, jac_2d)
!
      use t_surface_data
      use t_jacobians
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
      end subroutine s_int_normal_4_all_surf_type
!
!-----------------------------------------------------------------------
!
      end module int_normal_4_surf_type
