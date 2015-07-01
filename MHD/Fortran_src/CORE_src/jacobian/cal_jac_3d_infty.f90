!cal_jac_3d_infty.f90
!      module cal_jac_3d_infty
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine s_cal_jacobian_3d_inf_linear(xjac, axjac,             &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,             &
!     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,   &
!     &          dnxi_inf, dnei_inf, dnzi_inf)
!      subroutine s_cal_jacobian_3d_inf_quad(xjac, axjac,               &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,             &
!     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,   &
!     &          dnxi_inf, dnei_inf, dnzi_inf)
!      subroutine s_cal_jacobian_3d_inf_lag(xjac, axjac,                &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,             &
!     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,   &
!     &          dnxi_inf, dnei_inf, dnzi_inf)
!
!      subroutine s_cal_jacobian_3d_inf_l_quad(xjac, axjac,             &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,             &
!     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,   &
!     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      module cal_jac_3d_infty
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use m_surf_data_infinity
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_inf_linear(xjac, axjac,              &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,              &
     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,    &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_linear
!
      real(kind = kreal), intent(in) :: dnxi(num_t_linear)
      real(kind = kreal), intent(in) :: dnei(num_t_linear)
      real(kind = kreal), intent(in) :: dnzi(num_t_linear)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: dnxi_inf(num_t_linear,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnei_inf(num_t_linear,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnzi_inf(num_t_linear,nsurf_4_ele)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_linear)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_linear)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_linear)
!
!
      call s_cal_jacobian_3d_inf_8(numnod, numele,                      &
     &    np_smp, ie, xx, num_surf_bc, surf_item,                       &
     &    infty_list%ngrp_sf, infty_list%igrp_sf, num_surf_smp,         &
     &    isurf_grp_smp_stack, xjac, axjac, dnx, dny, dnz,              &
     &    dxidx, deidx, dzidx, dxidy, deidy, dzidy, dxidz,              &
     &    deidz, dzidz, dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      end subroutine s_cal_jacobian_3d_inf_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_inf_quad(xjac, axjac,                &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,              &
     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,    &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_quad
!
      real(kind = kreal), intent(in) :: dnxi(num_t_quad)
      real(kind = kreal), intent(in) :: dnei(num_t_quad)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: dnxi_inf(num_t_quad,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnei_inf(num_t_quad,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnzi_inf(num_t_quad,nsurf_4_ele)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_quad)
!
!
      call s_cal_jacobian_3d_inf_20(numnod, numele,                     &
     &    np_smp, ie, xx, num_surf_bc, surf_item,                       &
     &    infty_list%ngrp_sf, infty_list%igrp_sf, num_surf_smp,         &
     &    isurf_grp_smp_stack, xjac, axjac, dnx, dny, dnz,              &
     &    dxidx, deidx, dzidx, dxidy, deidy, dzidy, dxidz,              &
     &    deidz, dzidz, dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      end subroutine s_cal_jacobian_3d_inf_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_inf_lag(xjac, axjac,                 &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,              &
     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,    &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_lag
!
      real(kind = kreal), intent(in) :: dnxi(num_t_lag)
      real(kind = kreal), intent(in) :: dnei(num_t_lag)
      real(kind = kreal), intent(in) :: dnzi(num_t_lag)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: dnxi_inf(num_t_lag,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnei_inf(num_t_lag,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnzi_inf(num_t_lag,nsurf_4_ele)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_lag)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_lag)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_lag)
!
!
      call s_cal_jacobian_3d_inf_27(numnod, numele,                     &
     &          np_smp, ie, xx, num_surf_bc, surf_item,                 &
     &          infty_list%ngrp_sf, infty_list%igrp_sf, num_surf_smp,   &
     &          isurf_grp_smp_stack, xjac, axjac,                       &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,              &
     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,    &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      end subroutine s_cal_jacobian_3d_inf_lag
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_inf_l_quad(xjac, axjac,              &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy,              &
     &          deidy, dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi,    &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_l_quad
!
      real(kind = kreal), intent(in) :: dnxi(num_t_quad)
      real(kind = kreal), intent(in) :: dnei(num_t_quad)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: dnxi_inf(num_t_quad,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnei_inf(num_t_quad,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnzi_inf(num_t_quad,nsurf_4_ele)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_quad)
!
!
      call s_cal_jacobian_3d_inf_8_20(numnod, numele,                   &
     &    np_smp, ie, xx, num_surf_bc, surf_item,                       &
     &    infty_list%ngrp_sf, infty_list%igrp_sf, num_surf_smp,         &
     &    isurf_grp_smp_stack, xjac, axjac, dnx, dny, dnz,              &
     &    dxidx, deidx, dzidx, dxidy, deidy, dzidy, dxidz,              &
     &    deidz, dzidz, dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      end subroutine s_cal_jacobian_3d_inf_l_quad
!
!-----------------------------------------------------------------------
!
      end module cal_jac_3d_infty
