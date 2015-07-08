!cal_jacobians_quad_on_l.f90
!      module cal_jacobians_quad_on_l
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_quad_on_linear
!      subroutine cal_jacobian_dyquad_on_linear
!      subroutine cal_jacobian_surface_quad_on_l
!      subroutine cal_jacobian_edge_quad_on_l(jac_1d_ql)
!
      module cal_jacobians_quad_on_l
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
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
      use cal_jac_3d
      use cal_shape_function_3d
!
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_quad(ntot_int_3d, am,                   &
     &    dnxi_20, dnei_20, dnzi_20, xi3, ei3, zi3)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
!
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_lin_quad(xjac_lq(1,ix),                &
     &        axjac_lq(1,ix), dmx(1,1,ix,1), dmx(1,1,ix,2),             &
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
      subroutine cal_jacobian_dyquad_on_linear
!
      use m_surface_group
      use m_jacobian_sf_grp
      use cal_jac_2d
      use cal_shape_function_2d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_quad(jac1_sf_grp_2d_ql%ntot_int,     &
     &    jac1_sf_grp_2d_ql%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_quad                               &
     &       (jac1_sf_grp_2d_ql%xj_sf(1:num_surf_bc,ix),                &
     &        jac1_sf_grp_2d_ql%axj_sf(1:num_surf_bc,ix),               &
     &        jac1_sf_grp_2d_ql%xsf_sf(1:num_surf_bc,ix,1),             &
     &        jac1_sf_grp_2d_ql%xsf_sf(1:num_surf_bc,ix,2),             &
     &        jac1_sf_grp_2d_ql%xsf_sf(1:num_surf_bc,ix,3),             &
     &        dnxi_sf20(1,ix), dnei_sf20(1,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_dyquad_on_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_quad_on_l
!
      use m_jacobians_4_surface
      use cal_jac_2d
      use cal_shape_function_2d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_quad(jac1_2d_ql%ntot_int,            &
     &    jac1_2d_ql%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_2d_l_quad                                 &
     &       (jac1_2d_ql%xj_sf(1:numsurf,ix),                           &
     &        jac1_2d_ql%axj_sf(1:numsurf,ix),                          &
     &        jac1_2d_ql%xsf_sf(1:numsurf,ix,1),                        &
     &        jac1_2d_ql%xsf_sf(1:numsurf,ix,2),                        &
     &        jac1_2d_ql%xsf_sf(1:numsurf,ix,3),                        &
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
      use cal_jac_1d
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
          call s_cal_jacobian_1d_l_quad                                 &
     &       (jac_1d_ql%xj_edge(1:numedge,ix),                          &
     &        jac_1d_ql%axj_edge(1:numedge,ix),                         &
     &        jac_1d_ql%xeg_edge(1:numedge,ix,1),                       &
     &        jac_1d_ql%xeg_edge(1:numedge,ix,2),                       &
     &        jac_1d_ql%xeg_edge(1:numedge,ix,3), dnxi_ed20(1,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_edge_quad_on_l
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_quad_on_l
