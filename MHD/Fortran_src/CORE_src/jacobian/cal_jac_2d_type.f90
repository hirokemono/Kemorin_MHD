!
!      module cal_jac_2d_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine cal_jac_2d_linear_type(mesh, surf_mesh,               &
!     &           xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!      subroutine cal_jac_2d_quad_type(mesh, surf_mesh,                 &
!     &           xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!      subroutine cal_jac_2d_lag_type(mesh, surf_mesh,                  &
!     &           xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!      subroutine cal_jac_2d_l_quad_type(mesh, surf_mesh,               &
!     &           xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_geometry), intent(in)  :: surf_mesh
!
!        real(kind=kreal), intent(in) :: dnxi(surf_mesh%surf%nnod_4_surf)
!        real(kind=kreal), intent(in) :: dnei(surf_mesh%surf%nnod_4_surf)
!
!        real(kind=kreal), intent(inout) :: xjac(surf_mesh%surf%numsurf)
!        real(kind=kreal), intent(inout) :: axjac(surf_mesh%surf%numsurf)
!        real(kind=kreal), intent(inout) :: xsf(surf_mesh%surf%numsurf)
!        real(kind=kreal), intent(inout) :: ysf(surf_mesh%surf%numsurf)
!        real(kind=kreal), intent(inout) :: zsf(surf_mesh%surf%numsurf)
!
!      subroutine cal_jac_sf_grp_linear_type(mesh, group,               &
!     &          xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!      subroutine cal_jac_sf_grp_quad_type(mesh, group,                 &
!     &          xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!      subroutine cal_jac_sf_grp_lag_type(mesh, group,                  &
!     &          xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!      subroutine cal_jac_sf_grp_l_quad_type(mesh, group,               &
!     &          xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!       type(mesh_geometry),    intent(in) :: mesh
!       type(mesh_groups),      intent(in) :: group
!
!       real(kind=kreal), intent(in) :: dnxi(surf_mesh%surf%nnod_4_surf)
!       real(kind=kreal), intent(in) :: dnei(surf_mesh%surf%nnod_4_surf)
!
!       real(kind=kreal), intent(inout):: xjac(group%surf_grp%nitem_grp)
!       real(kind=kreal), intent(inout):: axjac(group%surf_grp%nitem_grp)
!       real(kind=kreal), intent(inout):: xsf(group%surf_grp%nitem_grp)
!       real(kind=kreal), intent(inout):: ysf(group%surf_grp%nitem_grp)
!       real(kind=kreal), intent(inout):: zsf(group%surf_grp%nitem_grp)
!
      module cal_jac_2d_type
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_mesh_data
!
      implicit none
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_2d_linear_type(mesh, surf_mesh,                &
     &           xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      use cal_jacobian_2d_linear
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in)  :: surf_mesh
!
      real(kind=kreal), intent(in) :: dnxi(num_linear_sf)
      real(kind=kreal), intent(in) :: dnei(num_linear_sf)
!
      real(kind=kreal), intent(inout) :: xjac(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: axjac(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: xsf(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: ysf(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: zsf(surf_mesh%surf%numsurf)
!
!
      call s_cal_jacobian_2d_4(mesh%node%numnod,                        &
     &    surf_mesh%surf%numsurf, surf_mesh%surf%ie_surf, mesh%node%xx, &
     &    np_smp, surf_mesh%surf%istack_surf_smp,                       &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine cal_jac_2d_linear_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_2d_quad_type(mesh, surf_mesh,                  &
     &           xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      use cal_jacobian_2d_quad
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in)  :: surf_mesh
!
      real(kind=kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind=kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind=kreal), intent(inout) :: xjac(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: axjac(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: xsf(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: ysf(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: zsf(surf_mesh%surf%numsurf)
!
!
      call s_cal_jacobian_2d_8(mesh%node%numnod,                        &
     &    surf_mesh%surf%numsurf, surf_mesh%surf%ie_surf, mesh%node%xx, &
     &    np_smp, surf_mesh%surf%istack_surf_smp,                       &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine cal_jac_2d_quad_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_2d_lag_type(mesh, surf_mesh,                   &
     &           xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      use cal_jacobian_2d_lag
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in)  :: surf_mesh
!
      real(kind=kreal), intent(in) :: dnxi(num_lag_sf)
      real(kind=kreal), intent(in) :: dnei(num_lag_sf)
!
      real(kind=kreal), intent(inout) :: xjac(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: axjac(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: xsf(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: ysf(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: zsf(surf_mesh%surf%numsurf)
!
!
      call s_cal_jacobian_2d_9(mesh%node%numnod,                        &
     &    surf_mesh%surf%numsurf, surf_mesh%surf%ie_surf, mesh%node%xx, &
     &    np_smp, surf_mesh%surf%istack_surf_smp,                       &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)

!
      end subroutine cal_jac_2d_lag_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_2d_l_quad_type(mesh, surf_mesh,                &
     &           xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      use cal_jacobian_2d_linear_quad
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in)  :: surf_mesh
!
      real(kind=kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind=kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind=kreal), intent(inout) :: xjac(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: axjac(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: xsf(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: ysf(surf_mesh%surf%numsurf)
      real(kind=kreal), intent(inout) :: zsf(surf_mesh%surf%numsurf)
!
!
      call s_cal_jacobian_2d_4_8(mesh%node%numnod,                      &
     &    surf_mesh%surf%numsurf, surf_mesh%surf%ie_surf, mesh%node%xx, &
     &    np_smp, surf_mesh%surf%istack_surf_smp,                       &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine cal_jac_2d_l_quad_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jac_sf_grp_linear_type(mesh, group,                &
     &          xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      use cal_jacobian_sf_grp_linear
!
      type(mesh_geometry),    intent(in) :: mesh
      type(mesh_groups),      intent(in) :: group
!
      real(kind=kreal), intent(in) :: dnxi(num_linear_sf)
      real(kind=kreal), intent(in) :: dnei(num_linear_sf)
!
      real(kind=kreal), intent(inout):: xjac(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: axjac(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: xsf(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: ysf(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: zsf(group%surf_grp%num_item)
!
!
      call s_cal_jacobian_sf_grp_4(mesh%node%numnod, mesh%ele%numele,   &
     &    mesh%ele%ie, mesh%node%xx, group%surf_grp%num_grp,            &
     &    group%surf_grp%num_item, group%surf_grp%item_sf_grp, np_smp,  &
     &    group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp,    &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine cal_jac_sf_grp_linear_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_sf_grp_quad_type(mesh, group,                  &
     &          xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      use cal_jacobian_sf_grp_quad
!
      type(mesh_geometry),    intent(in) :: mesh
      type(mesh_groups),      intent(in) :: group
!
      real(kind=kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind=kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind=kreal), intent(inout):: xjac(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: axjac(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: xsf(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: ysf(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: zsf(group%surf_grp%num_item)
!
!
      call s_cal_jacobian_sf_grp_8(mesh%node%numnod, mesh%ele%numele,   &
     &    mesh%ele%ie, mesh%node%xx, group%surf_grp%num_grp,            &
     &    group%surf_grp%num_item, group%surf_grp%item_sf_grp, np_smp,  &
     &    group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp,    &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine cal_jac_sf_grp_quad_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_sf_grp_lag_type(mesh, group,                   &
     &          xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      use cal_jacobian_sf_grp_lag
!
      type(mesh_geometry),    intent(in) :: mesh
      type(mesh_groups),      intent(in) :: group
!
      real(kind=kreal), intent(in) :: dnxi(num_lag_sf)
      real(kind=kreal), intent(in) :: dnei(num_lag_sf)
!
      real(kind=kreal), intent(inout):: xjac(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: axjac(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: xsf(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: ysf(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: zsf(group%surf_grp%num_item)
!
!
      call s_cal_jacobian_sf_grp_9(mesh%node%numnod, mesh%ele%numele,   &
     &    mesh%ele%ie, mesh%node%xx, group%surf_grp%num_grp,            &
     &    group%surf_grp%num_item, group%surf_grp%item_sf_grp, np_smp,  &
     &    group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp,    &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine cal_jac_sf_grp_lag_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_sf_grp_l_quad_type(mesh, group,                &
     &          xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      use cal_jacobian_sf_grp_l_quad
!
      type(mesh_geometry),    intent(in) :: mesh
      type(mesh_groups),      intent(in) :: group
!
      real(kind=kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind=kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind=kreal), intent(inout):: xjac(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: axjac(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: xsf(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: ysf(group%surf_grp%num_item)
      real(kind=kreal), intent(inout):: zsf(group%surf_grp%num_item)
!
!
      call s_cal_jacobian_sf_grp_4_8(mesh%node%numnod, mesh%ele%numele, &
     &    mesh%ele%ie, mesh%node%xx, group%surf_grp%num_grp,            &
     &    group%surf_grp%num_item, group%surf_grp%item_sf_grp, np_smp,  &
     &    group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp,    &
     &    xjac, axjac, xsf, ysf, zsf, dnxi, dnei)
!
      end subroutine cal_jac_sf_grp_l_quad_type
!
!-----------------------------------------------------------------------
!
      end module cal_jac_2d_type
