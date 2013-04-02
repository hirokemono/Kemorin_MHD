!
!      module cal_jac_2d
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine s_cal_jacobian_2d_linear(xjac, axjac, xsf, ysf, zsf,  &
!     &          dnxi, dnei)
!      subroutine s_cal_jacobian_2d_quad(xjac, axjac, xsf, ysf, zsf,    &
!     &          dnxi, dnei)
!      subroutine s_cal_jacobian_2d_lag(xjac, axjac, xsf, ysf, zsf,     &
!     &          dnxi, dnei)
!      subroutine s_cal_jacobian_2d_l_quad(xjac, axjac, xsf, ysf, zsf,  &
!     &          dnxi, dnei)
!
!      subroutine s_cal_jacobian_sf_grp_linear(xjac, axjac,             &
!     &          xsf, ysf, zsf, dnxi, dnei)
!      subroutine s_cal_jacobian_sf_grp_quad(xjac, axjac,               &
!     &          xsf, ysf, zsf, dnxi, dnei)
!      subroutine s_cal_jacobian_sf_grp_lag(xjac, axjac,                &
!     &          xsf, ysf, zsf, dnxi, dnei)
!      subroutine s_cal_jacobian_sf_grp_l_quad(xjac, axjac,             &
!     &          xsf, ysf, zsf, dnxi, dnei)
!
      module cal_jac_2d
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_2d_linear(xjac, axjac, xsf, ysf, zsf,   &
     &          dnxi, dnei)
!
      use cal_jacobian_2d_linear
!
      real(kind = kreal), intent(in) :: dnxi(num_linear_sf)
      real(kind = kreal), intent(in) :: dnei(num_linear_sf)
!
      real(kind = kreal), intent(inout) :: xjac(numsurf)
      real(kind = kreal), intent(inout) :: axjac(numsurf)
      real(kind = kreal), intent(inout) :: xsf(numsurf)
      real(kind = kreal), intent(inout) :: ysf(numsurf)
      real(kind = kreal), intent(inout) :: zsf(numsurf)
!
!
      call s_cal_jacobian_2d_4(numnod, numsurf,                         &
     &          ie_surf, xx, np_smp, isurf_smp_stack, xjac, axjac,      &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine s_cal_jacobian_2d_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_2d_quad(xjac, axjac, xsf, ysf, zsf,     &
     &          dnxi, dnei)
!
      use cal_jacobian_2d_quad
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind = kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind = kreal), intent(inout) :: xjac(numsurf)
      real(kind = kreal), intent(inout) :: axjac(numsurf)
      real(kind = kreal), intent(inout) :: xsf(numsurf)
      real(kind = kreal), intent(inout) :: ysf(numsurf)
      real(kind = kreal), intent(inout) :: zsf(numsurf)
!
!
      call s_cal_jacobian_2d_8(numnod, numsurf,                         &
     &          ie_surf, xx, np_smp, isurf_smp_stack, xjac, axjac,      &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine s_cal_jacobian_2d_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_2d_lag(xjac, axjac, xsf, ysf, zsf,     &
     &          dnxi, dnei)
!
      use cal_jacobian_2d_lag
!
      real(kind = kreal), intent(in) :: dnxi(num_lag_sf)
      real(kind = kreal), intent(in) :: dnei(num_lag_sf)
!
      real(kind = kreal), intent(inout) :: xjac(numsurf)
      real(kind = kreal), intent(inout) :: axjac(numsurf)
      real(kind = kreal), intent(inout) :: xsf(numsurf)
      real(kind = kreal), intent(inout) :: ysf(numsurf)
      real(kind = kreal), intent(inout) :: zsf(numsurf)
!
!
      call s_cal_jacobian_2d_9(numnod, numsurf,                         &
     &          ie_surf, xx, np_smp, isurf_smp_stack, xjac, axjac,      &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine s_cal_jacobian_2d_lag
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_2d_l_quad(xjac, axjac, xsf, ysf, zsf,   &
     &          dnxi, dnei)
!
      use cal_jacobian_2d_linear_quad
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind = kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind = kreal), intent(inout) :: xjac(numsurf)
      real(kind = kreal), intent(inout) :: axjac(numsurf)
      real(kind = kreal), intent(inout) :: xsf(numsurf)
      real(kind = kreal), intent(inout) :: ysf(numsurf)
      real(kind = kreal), intent(inout) :: zsf(numsurf)
!
!
      call s_cal_jacobian_2d_4_8(numnod, numsurf,                       &
     &          ie_surf, xx, np_smp, isurf_smp_stack, xjac, axjac,      &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine s_cal_jacobian_2d_l_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_sf_grp_linear(xjac, axjac,              &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      use m_surface_group
      use cal_jacobian_sf_grp_linear
!
      real(kind = kreal), intent(in) :: dnxi(num_linear_sf)
      real(kind = kreal), intent(in) :: dnei(num_linear_sf)
!
      real(kind = kreal), intent(inout) :: xjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: axjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: xsf(num_surf_bc)
      real(kind = kreal), intent(inout) :: ysf(num_surf_bc)
      real(kind = kreal), intent(inout) :: zsf(num_surf_bc)
!
!
      call s_cal_jacobian_sf_grp_4(numnod, numele,                      &
     &    ie, xx, num_surf, num_surf_bc, surf_item,                     &
     &    np_smp, num_surf_smp, isurf_grp_smp_stack,                    &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine s_cal_jacobian_sf_grp_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_sf_grp_quad(xjac, axjac,                &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      use m_surface_group
      use cal_jacobian_sf_grp_quad
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind = kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind = kreal), intent(inout) :: xjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: axjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: xsf(num_surf_bc)
      real(kind = kreal), intent(inout) :: ysf(num_surf_bc)
      real(kind = kreal), intent(inout) :: zsf(num_surf_bc)
!
!
      call s_cal_jacobian_sf_grp_8(numnod, numele,                      &
     &    ie, xx, num_surf, num_surf_bc, surf_item,                     &
     &    np_smp, num_surf_smp, isurf_grp_smp_stack,                    &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine s_cal_jacobian_sf_grp_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_sf_grp_lag(xjac, axjac,                 &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      use m_surface_group
      use cal_jacobian_sf_grp_lag
!
      real(kind = kreal), intent(in) :: dnxi(num_lag_sf)
      real(kind = kreal), intent(in) :: dnei(num_lag_sf)
!
      real(kind = kreal), intent(inout) :: xjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: axjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: xsf(num_surf_bc)
      real(kind = kreal), intent(inout) :: ysf(num_surf_bc)
      real(kind = kreal), intent(inout) :: zsf(num_surf_bc)
!
!
      call s_cal_jacobian_sf_grp_9(numnod, numele,                      &
     &    ie, xx, num_surf, num_surf_bc, surf_item,                     &
     &    np_smp, num_surf_smp, isurf_grp_smp_stack,                    &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine s_cal_jacobian_sf_grp_lag
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_sf_grp_l_quad(xjac, axjac,              &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      use m_surface_group
      use cal_jacobian_sf_grp_l_quad
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind = kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind = kreal), intent(inout) :: xjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: axjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: xsf(num_surf_bc)
      real(kind = kreal), intent(inout) :: ysf(num_surf_bc)
      real(kind = kreal), intent(inout) :: zsf(num_surf_bc)
!
!
      call s_cal_jacobian_sf_grp_4_8(numnod, numele,                    &
     &    ie, xx, num_surf, num_surf_bc, surf_item,                     &
     &    np_smp, num_surf_smp, isurf_grp_smp_stack,                    &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine s_cal_jacobian_sf_grp_l_quad
!
!-----------------------------------------------------------------------
!
      end module cal_jac_2d
