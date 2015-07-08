!
!      module cal_jacobians_linear
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_trilinear
!      subroutine cal_jacobian_dylinear(jac_sf_grp_l)
!      subroutine cal_jacobian_edge_linear(jac_1d_l)
!
      module cal_jacobians_linear
!
      use m_precision
!
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
      subroutine cal_jacobian_trilinear
!
      use m_jacobians
      use cal_jac_3d
      use cal_shape_function_3d
!
      integer (kind = kint) :: ii, ix, i0
!
      call s_cal_shape_function_linear(ntot_int_3d, an,                 &
     &    dnxi_1, dnei_1, dnzi_1, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
!
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_linear(xjac(1,ix), axjac(1,ix),        &
     &        dnx(1,1,ix,1), dnx(1,1,ix,2), dnx(1,1,ix,3),              &
     &        dxidx_1(1,ix,1,1), dxidx_1(1,ix,2,1), dxidx_1(1,ix,3,1),  &
     &        dxidx_1(1,ix,1,2), dxidx_1(1,ix,2,2), dxidx_1(1,ix,3,2),  &
     &        dxidx_1(1,ix,1,3), dxidx_1(1,ix,2,3), dxidx_1(1,ix,3,3),  &
     &        dnxi_1(1,ix), dnei_1(1,ix), dnzi_1(1,ix) )
!
        end do
      end do
!
      end subroutine cal_jacobian_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_dylinear(jac_sf_grp_l)
!
      use m_surface_group
      use t_jacobian_2d
      use cal_jac_2d
      use cal_shape_function_2d
!
      type(jacobians_2d), intent(inout) :: jac_sf_grp_l
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_linear(jac_sf_grp_l%ntot_int,        &
     &    jac_sf_grp_l%an_sf, dnxi_sf1, dnei_sf1, xi2, ei2)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_linear                             &
     &       (jac_sf_grp_l%xj_sf(1:num_surf_bc,ix),                     &
     &        jac_sf_grp_l%axj_sf(1:num_surf_bc,ix),                    &
     &        jac_sf_grp_l%xsf_sf(1:num_surf_bc,ix,1),                  &
     &        jac_sf_grp_l%xsf_sf(1:num_surf_bc,ix,2),                  &
     &        jac_sf_grp_l%xsf_sf(1:num_surf_bc,ix,3),                  &
     &        dnxi_sf1(1,ix), dnei_sf1(1,ix) )
        end do
      end do
!
!
      end subroutine cal_jacobian_dylinear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_linear
!
      use m_jacobians_4_surface
      use cal_jac_2d
      use cal_shape_function_2d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_2d_linear(ntot_int_2d, an_surf,         &
     &    dnxi_sf1, dnei_sf1, xi2, ei2)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
!
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_2d_linear(xj_surf(1,ix), axj_surf(1,ix),  &
     &        xsf_surf(1,ix,1), xsf_surf(1,ix,2), xsf_surf(1,ix,3),     &
     &        dnxi_sf1(1,ix), dnei_sf1(1,ix) )
!
        end do
      end do
!
!
      end subroutine cal_jacobian_surface_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_linear(jac_1d_l)
!
      use t_jacobian_1d
      use cal_jac_1d
      use cal_shape_function_1d
!
      type(jacobians_1d), intent(inout) :: jac_1d_l
      integer (kind = kint) :: ii, ix, i0
!
!
      call s_cal_shape_function_1d_linear(jac_1d_l%ntot_int,            &
     &    jac_1d_l%an_edge, dnxi_ed1, xi1)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
!
          call s_cal_jacobian_1d_linear                                 &
     &       (jac_1d_l%xj_edge(1:numedge,ix),                           &
     &        jac_1d_l%axj_edge(1:numedge,ix),                          &
     &        jac_1d_l%xeg_edge(1:numedge,ix,1),                        &
     &        jac_1d_l%xeg_edge(1:numedge,ix,2),                        &
     &        jac_1d_l%xeg_edge(1:numedge,ix,3),                        &
     &        dnxi_ed1(1,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_edge_linear
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_linear
