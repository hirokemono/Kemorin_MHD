!
!      module cal_jacobians_lag
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_lag
!      subroutine cal_jacobian_dylag(jac_sf_grp_q)
!      subroutine cal_jacobian_surface_lag
!
      module cal_jacobians_lag
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
      subroutine cal_jacobian_lag
!
      use m_jacobians
      use cal_jac_3d
      use cal_shape_function_3d
!
      integer (kind=kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_lag(ntot_int_3d, aw,                    &
     &    dnxi_27, dnei_27, dnzi_27, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
!
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_lag(xjac_q(1,ix), axjac_q(1,ix),       &
     &      dwx(1,1,ix,1), dwx(1,1,ix,2), dwx(1,1,ix,3),                &
     &      dxidx_20(1,ix,1,1), dxidx_20(1,ix,2,1), dxidx_20(1,ix,3,1), &
     &      dxidx_20(1,ix,1,2), dxidx_20(1,ix,2,2), dxidx_20(1,ix,3,2), &
     &      dxidx_20(1,ix,1,3), dxidx_20(1,ix,2,3), dxidx_20(1,ix,3,3), &
     &      dnxi_27(1,ix), dnei_27(1,ix), dnzi_27(1,ix) )
        end do
      end do
!
      xjac = xjac_q
      axjac = axjac_q
!
      end subroutine cal_jacobian_lag
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_dylag(jac_sf_grp_q)
!
      use m_surface_group
      use t_jacobian_2d
      use cal_jac_2d
      use cal_shape_function_2d
!
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
          call s_cal_jacobian_sf_grp_lag                                &
     &       (jac_sf_grp_q%xj_sf(1:num_surf_bc,ix),                     &
     &        jac_sf_grp_q%axj_sf(1:num_surf_bc,ix),                    &
     &        jac_sf_grp_q%xsf_sf(1:num_surf_bc,ix,1),                  &
     &        jac_sf_grp_q%xsf_sf(1:num_surf_bc,ix,2),                  &
     &        jac_sf_grp_q%xsf_sf(1:num_surf_bc,ix,3),                  &
     &        dnxi_sf27(1,ix), dnei_sf27(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_dylag
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_lag
!
      use m_jacobians_4_surface
      use cal_jac_2d
      use cal_shape_function_2d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_lag(ntot_int_2d, aw_surf,            &
     &    dnxi_sf27, dnei_sf27, xi2, ei2)
!
!   jacobian for quadrature  elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
!
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_2d_lag(xjq_surf(1,ix), axjq_surf(1,ix),   &
     &        xsq_surf(1,ix,1), xsq_surf(1,ix,2), xsq_surf(1,ix,3),     &
     &        dnxi_sf27(1,ix), dnei_sf27(1,ix) )
!
        end do
      end do
!
      end subroutine cal_jacobian_surface_lag
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_lag
