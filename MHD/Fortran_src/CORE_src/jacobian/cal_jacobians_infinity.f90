!cal_jacobians_infinity.f90
!      module cal_jacobians_infinity
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!!      subroutine cal_jacobian_infinity(sf_grp)
!!      subroutine cal_jacobian_infty_quad(sf_grp)
!!      subroutine cal_jacobian_infty_lag(sf_grp)
!!
!!      subroutine cal_jacobian_infty_linear_quad(sf_grp)
!
      module cal_jacobians_infinity
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_surf_data_infinity
      use m_fem_gauss_int_coefs
      use m_shape_functions
      use m_jacobians
!
      use t_group_data
!
      use cal_shape_func_infty_3d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infinity(sf_grp)
!
      use cal_jacobian_3d_inf_linear
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_func_infty_linear                                &
     &   (jac1_3d_l%ntot_int, infty_list%sf_apt(1),                     &
     &    an_infty, dnxi_infty, dnei_infty, dnzi_infty,                 &
     &    xi3, ei3, zi3)
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_8                                  &
     &       (node1%numnod, ele1%numele, np_smp, ele1%ie, node1%xx,     &
     &        sf_grp%num_item, sf_grp%item_sf_grp,                      &
     &        infty_list%ngrp_sf, infty_list%igrp_sf,                   &
     &        sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                &
     &        xjac(1,ix), axjac(1,ix),                                  &
     &        dnx(1,1,ix,1), dnx(1,1,ix,2), dnx(1,1,ix,3),              &
     &        dxidx_1(1,ix,1,1), dxidx_1(1,ix,2,1), dxidx_1(1,ix,3,1),  &
     &        dxidx_1(1,ix,1,2), dxidx_1(1,ix,2,2), dxidx_1(1,ix,3,2),  &
     &        dxidx_1(1,ix,1,3), dxidx_1(1,ix,2,3), dxidx_1(1,ix,3,3),  &
     &        dnxi_1(1,ix), dnei_1(1,ix), dnzi_1(1,ix),                 &
     &        dnxi_infty(1,1,ix), dnei_infty(1,1,ix),                   &
     &        dnzi_infty(1,1,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_infinity
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_quad(sf_grp)
!
      use cal_jacobian_3d_inf_quad
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_func_infty_quad                                  &
     &   (jac1_3d_q%ntot_int, infty_list%sf_apt(1),                     &
     &    aw_infty, dnxi_infty20, dnei_infty20, dnzi_infty20,           &
     &    xi3, ei3, zi3)
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_20                                 &
     &       (node1%numnod, ele1%numele, np_smp, ele1%ie, node1%xx,     &
     &        sf_grp%num_item, sf_grp%item_sf_grp,                      &
     &        infty_list%ngrp_sf, infty_list%igrp_sf,                   &
     &        sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                &
     &        xjac_q(1,ix), axjac_q(1,ix),                              &
     &      dwx(1,1,ix,1), dwx(1,1,ix,2), dwx(1,1,ix,3),                &
     &      dxidx_20(1,ix,1,1), dxidx_20(1,ix,2,1), dxidx_20(1,ix,3,1), &
     &      dxidx_20(1,ix,1,2), dxidx_20(1,ix,2,2), dxidx_20(1,ix,3,2), &
     &      dxidx_20(1,ix,1,3), dxidx_20(1,ix,2,3), dxidx_20(1,ix,3,3), &
     &      dnxi_20(1,ix), dnxi_20(1,ix), dnxi_20(1,ix),                &
     &      dnxi_infty20(1,1,ix), dnei_infty20(1,1,ix),                 &
     &      dnzi_infty20(1,1,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_infty_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_lag(sf_grp)
!
      use cal_jacobian_3d_inf_lag
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_func_infty_lag                                   &
     &   (jac1_3d_q%ntot_int, infty_list%sf_apt(1),                     &
     &    aw_infty, dnxi_infty27, dnei_infty27, dnzi_infty27,           &
     &    xi3, ei3, zi3)
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
!
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_27                                 &
     &       (node1%numnod, ele1%numele, np_smp, ele1%ie, node1%xx,     &
     &        sf_grp%num_item, sf_grp%item_sf_grp,                      &
     &        infty_list%ngrp_sf, infty_list%igrp_sf,                   &
     &        sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                &
     &        xjac_q(1,ix), axjac_q(1,ix),                              &
     &      dwx(1,1,ix,1), dwx(1,1,ix,2), dwx(1,1,ix,3),                &
     &      dxidx_20(1,ix,1,1), dxidx_20(1,ix,2,1), dxidx_20(1,ix,3,1), &
     &      dxidx_20(1,ix,1,2), dxidx_20(1,ix,2,2), dxidx_20(1,ix,3,2), &
     &      dxidx_20(1,ix,1,3), dxidx_20(1,ix,2,3), dxidx_20(1,ix,3,3), &
     &      dnxi_27(1,ix), dnxi_27(1,ix), dnxi_27(1,ix),                &
     &      dnxi_infty27(1,1,ix), dnei_infty27(1,1,ix),                 &
     &      dnzi_infty27(1,1,ix) )
!
        end do
      end do
!
!
      end subroutine cal_jacobian_infty_lag
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_infty_linear_quad(sf_grp)
!
      use cal_jacobian_3d_inf_l_quad
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_func_infty_quad                                  &
     &   (jac1_3d_lq%ntot_int, infty_list%sf_apt(1),                    &
     &    am_infty, dnxi_infty20, dnei_infty20, dnzi_infty20,           &
     &    xi3, ei3, zi3)
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_8_20                               &
     &       (node1%numnod, ele1%numele, np_smp, ele1%ie, node1%xx,     &
     &        sf_grp%num_item, sf_grp%item_sf_grp,                      &
     &        infty_list%ngrp_sf, infty_list%igrp_sf,                   &
     &        sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                &
     &        jac1_3d_lq%xjac(1:ele1%numele,ix),                        &
     &        axjac_lq(1,ix),                            &
     &      dmx(1,1,ix,1), dmx(1,1,ix,2), dmx(1,1,ix,3),                &
     &      dxidx_lq(1,ix,1,1), dxidx_lq(1,ix,2,1), dxidx_lq(1,ix,3,1), &
     &      dxidx_lq(1,ix,1,2), dxidx_lq(1,ix,2,2), dxidx_lq(1,ix,3,2), &
     &      dxidx_lq(1,ix,1,3), dxidx_lq(1,ix,2,3), dxidx_lq(1,ix,3,3), &
     &      dnxi_20(1,ix), dnxi_20(1,ix), dnxi_20(1,ix),                &
     &      dnxi_infty20(1,1,ix), dnei_infty20(1,1,ix),                 &
     &      dnzi_infty20(1,1,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_infty_linear_quad
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_infinity
