!
!      module cal_jacobians_quad
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_quad
!      subroutine cal_jacobian_surface_quad(jac_2d_q)
!      subroutine cal_jacobian_edge_quad(jac_1d_q)
!      subroutine cal_jacobian_dyquad(jac_sf_grp_q)
!
      module cal_jacobians_quad
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
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
      subroutine cal_jacobian_quad
!
      use m_jacobians
      use cal_jacobian_3d_quad
      use cal_shape_function_3d
!
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_quad(ntot_int_3d, aw,                   &
     &    dnxi_20, dnei_20, dnzi_20, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_20(numnod, numele, np_smp,             &
     &      iele_smp_stack, ie, xx, xjac_q(1,ix), axjac_q(1,ix),        &
     &      dwx(1,1,ix,1), dwx(1,1,ix,2), dwx(1,1,ix,3),                &
     &      dxidx_20(1,ix,1,1), dxidx_20(1,ix,2,1), dxidx_20(1,ix,3,1), &
     &      dxidx_20(1,ix,1,2), dxidx_20(1,ix,2,2), dxidx_20(1,ix,3,2), &
     &      dxidx_20(1,ix,1,3), dxidx_20(1,ix,2,3), dxidx_20(1,ix,3,3), &
     &      dnxi_20(1,ix), dnei_20(1,ix), dnzi_20(1,ix) )
        end do
      end do
!
      xjac = xjac_q
      axjac = axjac_q
!
      end subroutine cal_jacobian_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_quad(jac_2d_q)
!
      use t_jacobian_2d
      use cal_jacobian_2d_quad
      use cal_shape_function_2d
!
      type(jacobians_2d), intent(inout) :: jac_2d_q
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_quad(jac_2d_q%ntot_int,              &
     &    jac_2d_q%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_2d_8                                      &
     &       (numnod, numsurf, ie_surf, xx, np_smp, isurf_smp_stack,    &
     &        jac_2d_q%xj_sf(1:numsurf,ix),                             &
     &        jac_2d_q%axj_sf(1:numsurf,ix),                            &
     &        jac_2d_q%xsf_sf(1:numsurf,ix,1),                          &
     &        jac_2d_q%xsf_sf(1:numsurf,ix,2),                          &
     &        jac_2d_q%xsf_sf(1:numsurf,ix,3),                          &
     &        dnxi_sf20(1,ix), dnei_sf20(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_surface_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_quad(jac_1d_q)
!
      use t_jacobian_1d
      use cal_jacobian_1d
      use cal_shape_function_1d
!
      type(jacobians_1d), intent(inout) :: jac_1d_q
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_1d_quad(jac_1d_q%ntot_int,              &
     &    jac_1d_q%an_edge, dnxi_ed20, xi1)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
!
          call s_cal_jacobian_1d_3                                      &
     &       (numnod, numedge, ie_edge, xx, np_smp, iedge_smp_stack,    &
     &        jac_1d_q%xj_edge(1:numedge,ix),                           &
     &        jac_1d_q%axj_edge(1:numedge,ix),                          &
     &        jac_1d_q%xeg_edge(1:numedge,ix,1),                        &
     &        jac_1d_q%xeg_edge(1:numedge,ix,2),                        &
     &        jac_1d_q%xeg_edge(1:numedge,ix,3),                        &
     &        dnxi_ed20(1,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_edge_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_dyquad(jac_sf_grp_q)
!
      use m_surface_group
      use t_jacobian_2d
      use cal_jacobian_sf_grp_quad
      use cal_shape_function_2d
!
      type(jacobians_2d), intent(inout) :: jac_sf_grp_q
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_quad(jac_sf_grp_q%ntot_int,          &
     &    jac_sf_grp_q%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_8(numnod, numele,                  &
     &        ie, xx, sf_grp1%num_grp, num_surf_bc, surf_item,          &
     &        np_smp, num_surf_smp, isurf_grp_smp_stack,                &
     &        jac_sf_grp_q%xj_sf(1:num_surf_bc,ix),                     &
     &        jac_sf_grp_q%axj_sf(1:num_surf_bc,ix),                    &
     &        jac_sf_grp_q%xsf_sf(1:num_surf_bc,ix,1),                  &
     &        jac_sf_grp_q%xsf_sf(1:num_surf_bc,ix,2),                  &
     &        jac_sf_grp_q%xsf_sf(1:num_surf_bc,ix,3),                  &
     &        dnxi_sf20(1,ix), dnei_sf20(1,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_dyquad
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_quad
