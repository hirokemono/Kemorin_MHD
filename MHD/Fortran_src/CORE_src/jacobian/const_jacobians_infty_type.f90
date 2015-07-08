!const_jacobians_infty_type.f90
!      module const_jacobians_infty_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_infty_linear_type(mesh, jac_3d)
!      subroutine cal_jacobian_infty_quad_type(mesh, jac_3d)
!      subroutine cal_jacobian_infty_lag_type(mesh, jac_3d)
!
!      subroutine cal_jacobian_infty_l_quad_type(mesh, group, jac_3d)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(inout) :: jac_3d
!
      module const_jacobians_infty_type
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      implicit none
!
      private :: copy_shape_func_inf_from_array
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_linear_type(mesh, group, jac_3d)
!
      use t_mesh_data
      use t_jacobians
      use m_jacobians
      use cal_jacobian_3d_inf_linear
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(jacobians_3d), intent(inout) :: jac_3d
      integer (kind=kint) :: ii, ix, i0
!
!
      call copy_shape_func_inf_from_array(jac_3d%ntot_int,              &
     &    mesh%ele%nnod_4_ele, an_infty, jac_3d%an_infty)
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_8                                  &
     &      (mesh%node%numnod, mesh%ele%numele,                         &
     &       np_smp, mesh%ele%ie, mesh%node%xx,                         &
     &       group%surf_grp%num_item, group%surf_grp%item_sf_grp,       &
     &       group%infty_grp%ngrp_sf, group%infty_grp%igrp_sf,          &
     &       group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp, &
     &       jac_3d%xjac(1:mesh%ele%numele,ix),                         &
     &       jac_3d%axjac(1:mesh%ele%numele,ix),                        &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,1),  &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,2),  &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,3),  &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,3),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,3),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,3),                 &
     &       dnxi_1(1:mesh%ele%nnod_4_ele,ix),                          &
     &       dnei_1(1:mesh%ele%nnod_4_ele,ix),                          &
     &       dnzi_1(1:mesh%ele%nnod_4_ele,ix),                          &
     &       dnxi_infty(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix),        &
     &       dnei_infty(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix),        &
     &       dnzi_infty(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_infty_linear_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_quad_type(mesh, group, jac_3d)
!
      use t_mesh_data
      use t_jacobians
      use m_jacobians
      use cal_jacobian_3d_inf_quad
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(jacobians_3d), intent(inout) :: jac_3d
      integer (kind=kint) :: ii, ix, i0
!
!
      call copy_shape_func_inf_from_array(jac_3d%ntot_int,              &
     &    mesh%ele%nnod_4_ele, aw_infty, jac_3d%an_infty)
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_20                                 &
     &      (mesh%node%numnod, mesh%ele%numele,                         &
     &       np_smp, mesh%ele%ie, mesh%node%xx,                         &
     &       group%surf_grp%num_item, group%surf_grp%item_sf_grp,       &
     &       group%infty_grp%ngrp_sf, group%infty_grp%igrp_sf,          &
     &       group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp, &
     &       jac_3d%xjac(1:mesh%ele%numele,ix),                         &
     &       jac_3d%axjac(1:mesh%ele%numele,ix),                        &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,1),  &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,2),  &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,3),  &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,3),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,3),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,3),                 &
     &       dnxi_20(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnei_20(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnzi_20(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnxi_infty20(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix),      &
     &       dnei_infty20(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix),      &
     &       dnzi_infty20(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_infty_quad_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_lag_type(mesh, group, jac_3d)
!
      use t_mesh_data
      use t_jacobians
      use m_jacobians
      use cal_jacobian_3d_inf_lag
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(jacobians_3d), intent(inout) :: jac_3d
      integer (kind=kint) :: ii, ix, i0
!
!
      call copy_shape_func_inf_from_array(jac_3d%ntot_int,              &
     &    mesh%ele%nnod_4_ele, aw_infty, jac_3d%an_infty)
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_27                                 &
     &      (mesh%node%numnod, mesh%ele%numele,                         &
     &       np_smp, mesh%ele%ie, mesh%node%xx,                         &
     &       group%surf_grp%num_item, group%surf_grp%item_sf_grp,       &
     &       group%infty_grp%ngrp_sf, group%infty_grp%igrp_sf,          &
     &       group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp, &
     &       jac_3d%xjac(1:mesh%ele%numele,ix),                         &
     &       jac_3d%axjac(1:mesh%ele%numele,ix),                        &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,1),  &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,2),  &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,3),  &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,3),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,3),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,3),                 &
     &       dnxi_27(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnei_27(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnzi_27(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnxi_infty27(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix),      &
     &       dnei_infty27(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix),      &
     &       dnzi_infty27(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_infty_lag_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_l_quad_type(mesh, group, jac_3d)
!
      use t_mesh_data
      use t_jacobians
      use m_jacobians
      use cal_jacobian_3d_inf_l_quad
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(jacobians_3d), intent(inout) :: jac_3d
      integer (kind=kint) :: ii, ix, i0
!
!
      call copy_shape_func_inf_from_array(jac_3d%ntot_int,              &
     &    num_t_quad, aw_infty, jac_3d%an_infty)
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_8_20                               &
     &      (mesh%node%numnod, mesh%ele%numele,                         &
     &       np_smp, mesh%ele%ie, mesh%node%xx,                         &
     &       group%surf_grp%num_item, group%surf_grp%item_sf_grp,       &
     &       group%infty_grp%ngrp_sf, group%infty_grp%igrp_sf,          &
     &       group%surf_grp%num_grp_smp, group%surf_grp%istack_grp_smp, &
     &       jac_3d%xjac(1:mesh%ele%numele,ix),                         &
     &       jac_3d%axjac(1:mesh%ele%numele,ix),                        &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,1),  &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,2),  &
     &       jac_3d%dnx(1:mesh%ele%numele,1:mesh%ele%nnod_4_ele,ix,3),  &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,1),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,2),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,1,3),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,2,3),                 &
     &       jac_3d%dxidx_3d(1:mesh%ele%numele,ix,3,3),                 &
     &       dnxi_20(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnei_20(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnzi_20(1:mesh%ele%nnod_4_ele,ix),                         &
     &       dnxi_infty20(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix),      &
     &       dnei_infty20(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix),      &
     &       dnzi_infty20(1:mesh%ele%nnod_4_ele,1:nsurf_4_ele,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_infty_l_quad_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_shape_func_inf_from_array(ntot_int_3d,            &
     &          nnod_4_ele, an_infty_org, an_infty_dest)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, nnod_4_ele
      real(kind=kreal), intent(in)                                      &
     &      ::    an_infty_org(nnod_4_ele,nsurf_4_ele,ntot_int_3d)
      real(kind=kreal), intent(inout)                                   &
     &     :: an_infty_dest(nnod_4_ele,nsurf_4_ele,ntot_int_3d)
!
      integer (kind=kint) :: ix, isf, k1
!
      do ix = 1, ntot_int_3d
        do isf = 1, nsurf_4_ele
          do k1 = 1, nnod_4_ele
            an_infty_dest(k1,isf,ix) = an_infty_org(k1,isf,ix)
          end do
        end do
      end do
!
      end subroutine copy_shape_func_inf_from_array
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_infty_type
