!
!      module cal_jacobians_quad
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_quad
!      subroutine cal_jacobian_dyquad
!      subroutine cal_jacobian_surface_quad
!      subroutine cal_jacobian_edge_quad
!
      module cal_jacobians_quad
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
      use cal_jac_3d
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
!
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_quad(xjac_q(1,ix), axjac_q(1,ix),      &
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
      subroutine cal_jacobian_dyquad
!
      use m_jacobians_2d
      use cal_jac_2d
      use cal_shape_function_2d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_quad(ntot_int_sf_grp, aw_sf,         &
     &    dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
!
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_quad(xjq_sf(1,ix), axjq_sf(1,ix),  &
     &        xsq_sf(1,ix,1), xsq_sf(1,ix,2), xsq_sf(1,ix,3),           &
     &        dnxi_sf20(1,ix), dnei_sf20(1,ix) )
!
        end do
      end do
!
!
      end subroutine cal_jacobian_dyquad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_quad
!
      use m_jacobians_4_surface
      use cal_jac_2d
      use cal_shape_function_2d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_quad(ntot_int_2d, aw_surf,           &
     &    dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
!
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_2d_quad(xjq_surf(1,ix), axjq_surf(1,ix),  &
     &        xsq_surf(1,ix,1), xsq_surf(1,ix,2), xsq_surf(1,ix,3),     &
     &        dnxi_sf20(1,ix), dnei_sf20(1,ix) )
!
        end do
      end do
!
      end subroutine cal_jacobian_surface_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_quad
!
      use m_jacobians_4_edge
      use cal_jac_1d
      use cal_shape_function_1d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_1d_quad(ntot_int_1d, aw_edge,           &
     &    dnxi_ed20, xi1)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
!
          ix = int_start1(i0) + ii
!
          call s_cal_jacobian_1d_quad(xjq_edge(1,ix), axjq_edge(1,ix),  &
     &        xeq_edge(1,ix,1), xeq_edge(1,ix,2), xeq_edge(1,ix,3),     &
     &        dnxi_ed20(1,ix))
!
        end do
      end do
!
!
      end subroutine cal_jacobian_edge_quad
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_quad
