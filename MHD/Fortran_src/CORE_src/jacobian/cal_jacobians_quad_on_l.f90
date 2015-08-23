!cal_jacobians_quad_on_l.f90
!      module cal_jacobians_quad_on_l
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_quad_on_linear
!      subroutine cal_jacobian_surface_quad_on_l(jac_2d_ql)
!      subroutine cal_jacobian_edge_quad_on_l(jac_1d_ql)
!      subroutine cal_jacobian_dyquad_on_linear(sf_grp, jac_sf_grp_ql)
!
      module cal_jacobians_quad_on_l
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
      subroutine cal_jacobian_quad_on_linear
!
      use m_jacobians
      use cal_jacobian_3d_linear_quad
      use cal_shape_function_3d
!
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_quad                                    &
     &   (jac1_3d_lq%ntot_int, jac1_3d_lq%an,                           &
     &    dnxi_20, dnei_20, dnzi_20, xi3, ei3, zi3)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call cal_jacobian_3d_8_20(node1%numnod, ele1%numele,          &
     &        np_smp, ele1%istack_ele_smp, ele1%ie, node1%xx,           &
     &        jac1_3d_lq%xjac(1:ele1%numele,ix),                        &
     &        jac1_3d_lq%axjac(1:ele1%numele,ix),                       &
     &        dmx(1,1,ix,1), dmx(1,1,ix,2),                             &
     &        dmx(1,1,ix,3),  dxidx_lq(1,ix,1,1), dxidx_lq(1,ix,2,1),   &
     &        dxidx_lq(1,ix,3,1), dxidx_lq(1,ix,1,2),                   &
     &        dxidx_lq(1,ix,2,2), dxidx_lq(1,ix,3,2),                   &
     &        dxidx_lq(1,ix,1,3), dxidx_lq(1,ix,2,3),                   &
     &        dxidx_lq(1,ix,3,3), dnxi_20(1,ix), dnei_20(1,ix),         &
     &        dnzi_20(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_quad_on_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_quad_on_l(jac_2d_ql)
!
      use t_jacobian_2d
      use cal_jacobian_2d_linear_quad
      use cal_shape_function_2d
!
      type(jacobians_2d), intent(inout) :: jac_2d_ql
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_quad(jac_2d_ql%ntot_int,             &
     &    jac_2d_ql%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_2d_4_8(node1%numnod, surf1%numsurf,       &
     &        surf1%ie_surf, node1%xx, np_smp, surf1%istack_surf_smp,   &
     &        jac_2d_ql%xj_sf(1:surf1%numsurf,ix),                      &
     &        jac_2d_ql%axj_sf(1:surf1%numsurf,ix),                     &
     &        jac_2d_ql%xsf_sf(1:surf1%numsurf,ix,1),                   &
     &        jac_2d_ql%xsf_sf(1:surf1%numsurf,ix,2),                   &
     &        jac_2d_ql%xsf_sf(1:surf1%numsurf,ix,3),                   &
     &        dnxi_sf20(1,ix), dnei_sf20(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_surface_quad_on_l
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_quad_on_l(jac_1d_ql)
!
      use t_jacobian_1d
      use cal_jacobian_1d
      use cal_shape_function_1d
!
      type(jacobians_1d), intent(inout) :: jac_1d_ql
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_1d_quad(jac_1d_ql%ntot_int,             &
     &    jac_1d_ql%an_edge, dnxi_ed20, xi1)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
!
          call s_cal_jacobian_1d_2_3(node1%numnod, edge1%numedge,       &
     &        edge1%ie_edge, node1%xx, np_smp, edge1%istack_edge_smp,   &
     &        jac_1d_ql%xj_edge(1:edge1%numedge,ix),                    &
     &        jac_1d_ql%axj_edge(1:edge1%numedge,ix),                   &
     &        jac_1d_ql%xeg_edge(1:edge1%numedge,ix,1),                 &
     &        jac_1d_ql%xeg_edge(1:edge1%numedge,ix,2),                 &
     &        jac_1d_ql%xeg_edge(1:edge1%numedge,ix,3),                 &
     &        dnxi_ed20(1,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_edge_quad_on_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_dyquad_on_linear(sf_grp, jac_sf_grp_ql)
!
      use m_geometry_constants
      use t_group_data
      use t_jacobian_2d
      use cal_jacobian_sf_grp_l_quad
      use cal_shape_function_2d
!
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp_ql
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_quad(jac_sf_grp_ql%ntot_int,         &
     &    jac_sf_grp_ql%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_4_8                                &
     &       (node1%numnod, ele1%numele, ele1%ie, node1%xx,             &
     &        sf_grp%num_grp, sf_grp%num_item, sf_grp%item_sf_grp,      &
     &        np_smp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp,        &
     &        jac_sf_grp_ql%xj_sf(1:sf_grp%num_item,ix),                &
     &        jac_sf_grp_ql%axj_sf(1:sf_grp%num_item,ix),               &
     &        jac_sf_grp_ql%xsf_sf(1:sf_grp%num_item,ix,1),             &
     &        jac_sf_grp_ql%xsf_sf(1:sf_grp%num_item,ix,2),             &
     &        jac_sf_grp_ql%xsf_sf(1:sf_grp%num_item,ix,3),             &
     &        dnxi_sf20(1,ix), dnei_sf20(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_dyquad_on_linear
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_quad_on_l
