!
!      module cal_jacobians_lag
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_lag
!      subroutine cal_jacobian_surface_lag(jac_2d_q)
!
!      subroutine cal_jacobian_dylag(sf_grp, jac_sf_grp_q)
!
      module cal_jacobians_lag
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_lag
!
      use m_jacobians
      use cal_jacobian_3d_lag
      use cal_shape_function_3d
!
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_lag(jac1_3d_q%ntot_int, aw,             &
     &    dnxi_27, dnei_27, dnzi_27, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_27(node1%numnod, ele1%numele, np_smp,  &
     &      ele1%istack_ele_smp, ele1%ie, node1%xx,                     &
     &      jac1_3d_q%xjac(1:ele1%numele,ix),                           &
     &      axjac_q(1,ix),                                              &
     &      dwx(1,1,ix,1), dwx(1,1,ix,2), dwx(1,1,ix,3),                &
     &      dxidx_20(1,ix,1,1), dxidx_20(1,ix,2,1), dxidx_20(1,ix,3,1), &
     &      dxidx_20(1,ix,1,2), dxidx_20(1,ix,2,2), dxidx_20(1,ix,3,2), &
     &      dxidx_20(1,ix,1,3), dxidx_20(1,ix,2,3), dxidx_20(1,ix,3,3), &
     &      dnxi_27(1,ix), dnei_27(1,ix), dnzi_27(1,ix) )
        end do
      end do
!
      xjac = jac1_3d_q%xjac
      axjac = axjac_q
!
      end subroutine cal_jacobian_lag
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_lag(jac_2d_q)
!
      use t_jacobian_2d
      use cal_jacobian_2d_lag
      use cal_shape_function_2d
!
      type(jacobians_2d), intent(inout) :: jac_2d_q
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_lag(jac_2d_q%ntot_int,               &
     &    jac_2d_q%an_sf, dnxi_sf27, dnei_sf27, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_2d_9(node1%numnod, surf1%numsurf,         &
     &        surf1%ie_surf, node1%xx, np_smp, surf1%istack_surf_smp,   &
     &        jac_2d_q%xj_sf(1:surf1%numsurf,ix),                       &
     &        jac_2d_q%axj_sf(1:surf1%numsurf,ix),                      &
     &        jac_2d_q%xsf_sf(1:surf1%numsurf,ix,1),                    &
     &        jac_2d_q%xsf_sf(1:surf1%numsurf,ix,2),                    &
     &        jac_2d_q%xsf_sf(1:surf1%numsurf,ix,3),                    &
     &        dnxi_sf27(1,ix), dnei_sf27(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_surface_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_dylag(sf_grp, jac_sf_grp_q)
!
      use t_group_data
      use t_jacobian_2d
      use cal_jacobian_sf_grp_lag
      use cal_shape_function_2d
!
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp_q
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_lag(jac_sf_grp_q%ntot_int,           &
     &    jac_sf_grp_q%an_sf, dnxi_sf27, dnei_sf27, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_9                                  &
     &       (node1%numnod, ele1%numele, ele1%ie, node1%xx,             &
     &        sf_grp%num_grp, sf_grp%num_item, sf_grp%item_sf_grp,      &
     &        np_smp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp,        &
     &        jac_sf_grp_q%xj_sf(1:sf_grp%num_item,ix),                 &
     &        jac_sf_grp_q%axj_sf(1:sf_grp%num_item,ix),                &
     &        jac_sf_grp_q%xsf_sf(1:sf_grp%num_item,ix,1),              &
     &        jac_sf_grp_q%xsf_sf(1:sf_grp%num_item,ix,2),              &
     &        jac_sf_grp_q%xsf_sf(1:sf_grp%num_item,ix,3),              &
     &        dnxi_sf27(1,ix), dnei_sf27(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_dylag
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_lag
