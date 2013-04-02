!cal_jac_3d_infty_type.f90
!      module cal_jac_3d_infty_type
!
!        programmed by H.Matsui on Dec., 2008
!
!      subroutine cal_jac_3d_inf_linear_type(mesh, group, xjac, axjac,  &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx,                    &
!     &          dxidy, deidy, dzidy, dxidz, deidz, dzidz,              &
!     &          dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!      subroutine cal_jac_3d_inf_quad_type(mesh, group, xjac, axjac,    &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx,                    &
!     &          dxidy, deidy, dzidy, dxidz, deidz, dzidz,              &
!     &          dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!      subroutine cal_jac_3d_inf_lag_type(mesh, group, xjac, axjac,     &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx,                    &
!     &          dxidy, deidy, dzidy, dxidz, deidz, dzidz,              &
!     &          dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!      subroutine cal_jac_3d_inf_l_quad_type(mesh, group, xjac, axjac,  &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx,                    &
!     &          dxidy, deidy, dzidy, dxidz, deidz, dzidz,              &
!     &          dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
!        type(mesh_geometry), intent(in) :: mesh
!        type(mesh_groups), intent(in) :: group
!
!        real(kind = kreal), intent(in) :: dnxi(mesh%ele%nnod_4_ele)
!        real(kind = kreal), intent(in) :: dnei(mesh%ele%nnod_4_ele)
!        real(kind = kreal), intent(in) :: dnzi(mesh%ele%nnod_4_ele)
!
!        real(kind = kreal), intent(in)                                 &
!     &                   :: dnxi_inf(mesh%ele%nnod_4_ele,nsurf_4_ele)
!        real(kind = kreal), intent(in)                                 &
!     &                   :: dnei_inf(mesh%ele%nnod_4_ele,nsurf_4_ele)
!        real(kind = kreal), intent(in)                                 &
!     &                   :: dnzi_inf(mesh%ele%nnod_4_ele,nsurf_4_ele)
!
!        real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
!        real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
!        real(kind = kreal), intent(inout)                              &
!     &           :: dnx(mesh%ele%numele,mesh%ele%nnod_4_ele)
!        real(kind = kreal), intent(inout)                              &
!     &           :: dny(mesh%ele%numele,mesh%ele%nnod_4_ele)
!        real(kind = kreal), intent(inout)                              &
!     &           :: dnz(mesh%ele%numele,mesh%ele%nnod_4_ele)
!
      module cal_jac_3d_infty_type
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use t_mesh_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_3d_inf_linear_type(mesh, group, xjac, axjac,   &
     &          dnx, dny, dnz, dxidx, deidx, dzidx,                     &
     &          dxidy, deidy, dzidy, dxidz, deidz, dzidz,               &
     &          dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_linear
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
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
      real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
      real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
      real(kind = kreal), intent(inout)                                 &
     &           :: dnx(mesh%ele%numele,num_t_linear)
      real(kind = kreal), intent(inout)                                 &
     &           :: dny(mesh%ele%numele,num_t_linear)
      real(kind = kreal), intent(inout)                                 &
     &           :: dnz(mesh%ele%numele,num_t_linear)
!
!
      call s_cal_jacobian_3d_inf_8(mesh%node%numnod, mesh%ele%numele,   &
     &    np_smp, mesh%ele%ie, mesh%node%xx,                            &
     &    group%surf_grp%num_item, group%surf_grp%item_sf_grp,          &
     &    group%infty_grp%ngrp_sf, group%infty_grp%igrp_sf,             &
     &    group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp,    &
     &    xjac, axjac, dnx, dny, dnz, dxidx, deidx, dzidx,              &
     &    dxidy, deidy, dzidy, dxidz, deidz, dzidz,                     &
     &    dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      end subroutine cal_jac_3d_inf_linear_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_3d_inf_quad_type(mesh, group, xjac, axjac,     &
     &          dnx, dny, dnz, dxidx, deidx, dzidx,                     &
     &          dxidy, deidy, dzidy, dxidz, deidz, dzidz,               &
     &          dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_quad
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
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
      real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
      real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
      real(kind = kreal), intent(inout)                                 &
     &           :: dnx(mesh%ele%numele,num_t_quad)
      real(kind = kreal), intent(inout)                                 &
     &           :: dny(mesh%ele%numele,num_t_quad)
      real(kind = kreal), intent(inout)                                 &
     &           :: dnz(mesh%ele%numele,num_t_quad)
!
!
      call s_cal_jacobian_3d_inf_20(mesh%node%numnod, mesh%ele%numele,  &
     &    np_smp, mesh%ele%ie, mesh%node%xx,                            &
     &    group%surf_grp%num_item, group%surf_grp%item_sf_grp,          &
     &    group%infty_grp%ngrp_sf, group%infty_grp%igrp_sf,             &
     &    group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp,    &
     &    xjac, axjac, dnx, dny, dnz, dxidx, deidx, dzidx,              &
     &    dxidy, deidy, dzidy, dxidz, deidz, dzidz,                     &
     &    dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      end subroutine cal_jac_3d_inf_quad_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_3d_inf_lag_type(mesh, group, xjac, axjac,      &
     &          dnx, dny, dnz, dxidx, deidx, dzidx,                     &
     &          dxidy, deidy, dzidy, dxidz, deidz, dzidz,               &
     &          dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_lag
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
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
      real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
      real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
      real(kind = kreal), intent(inout)                                 &
     &           :: dnx(mesh%ele%numele,num_t_lag)
      real(kind = kreal), intent(inout)                                 &
     &           :: dny(mesh%ele%numele,num_t_lag)
      real(kind = kreal), intent(inout)                                 &
     &           :: dnz(mesh%ele%numele,num_t_lag)
!
!
      call s_cal_jacobian_3d_inf_27(mesh%node%numnod, mesh%ele%numele,  &
     &    np_smp, mesh%ele%ie, mesh%node%xx,                            &
     &    group%surf_grp%num_item, group%surf_grp%item_sf_grp,          &
     &    group%infty_grp%ngrp_sf, group%infty_grp%igrp_sf,             &
     &    group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp,    &
     &    xjac, axjac, dnx, dny, dnz, dxidx, deidx, dzidx,              &
     &    dxidy, deidy, dzidy, dxidz, deidz, dzidz,                     &
     &    dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      end subroutine cal_jac_3d_inf_lag_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jac_3d_inf_l_quad_type(mesh, group, xjac, axjac,   &
     &          dnx, dny, dnz, dxidx, deidx, dzidx,                     &
     &          dxidy, deidy, dzidy, dxidz, deidz, dzidz,               &
     &          dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_l_quad
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
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
      real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
      real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
      real(kind = kreal), intent(inout)                                 &
     &           :: dnx(mesh%ele%numele,num_t_quad)
      real(kind = kreal), intent(inout)                                 &
     &           :: dny(mesh%ele%numele,num_t_quad)
      real(kind = kreal), intent(inout)                                 &
     &           :: dnz(mesh%ele%numele,num_t_quad)
!
!
      call s_cal_jacobian_3d_inf_8_20                                   &
     &    (mesh%node%numnod, mesh%ele%numele,                           &
     &    np_smp, mesh%ele%ie, mesh%node%xx,                            &
     &    group%surf_grp%num_item, group%surf_grp%item_sf_grp,          &
     &    group%infty_grp%ngrp_sf, group%infty_grp%igrp_sf,             &
     &    group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp,    &
     &    xjac, axjac, dnx, dny, dnz, dxidx, deidx, dzidx,              &
     &    dxidy, deidy, dzidy, dxidz, deidz, dzidz,                     &
     &    dnxi, dnei, dnzi, dnxi_inf, dnei_inf, dnzi_inf)
!
      end subroutine cal_jac_3d_inf_l_quad_type
!
!-----------------------------------------------------------------------
!
      end module cal_jac_3d_infty_type
