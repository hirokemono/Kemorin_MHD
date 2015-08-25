!
!      module cal_jacobians_linear
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_trilinear
!      subroutine cal_jacobian_surface_linear(jac_2d_l)
!      subroutine cal_jacobian_edge_linear(jac_1d_l)
!      subroutine cal_jacobian_dylinear(sf_grp, jac_sf_grp_l)
!
      module cal_jacobians_linear
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
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
      subroutine cal_jacobian_trilinear
!
      use m_jacobians
      use cal_1ele_jacobians
      use cal_shape_function_3d
!
!
      call s_cal_shape_function_linear                                  &
     &   (jac1_3d_l%ntot_int, jac1_3d_l%an,                             &
     &    dnxi_1, dnei_1, dnzi_1, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_8                                            &
     &       (node1%numnod, ele1%numele, ele1%nnod_4_ele, np_smp,       &
     &        ele1%istack_ele_smp, ele1%ie, node1%xx,                   &
     &        jac1_3d_l%ntot_int, jac1_3d_l%xjac, jac1_3d_l%axjac,      &
     &        dnx, dxidx_1, dnxi_1, dnei_1, dnzi_1)
!
      end subroutine cal_jacobian_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_linear(jac_2d_l)
!
      use t_jacobian_2d
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(jacobians_2d), intent(inout) :: jac_2d_l
!
!
      call s_cal_shape_function_2d_linear(jac_2d_l%ntot_int,            &
     &    jac_2d_l%an_sf, dnxi_sf1, dnei_sf1, xi2, ei2)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_2d_4                                            &
     &       (node1%numnod, surf1%numsurf, surf1%nnod_4_surf,           &
     &        surf1%ie_surf, node1%xx, np_smp, surf1%istack_surf_smp,   &
     &        jac_2d_l%ntot_int, jac_2d_l%xj_sf, jac_2d_l%axj_sf,       &
     &        jac_2d_l%xsf_sf, dnxi_sf1, dnei_sf1)
!
      end subroutine cal_jacobian_surface_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_linear(jac_1d_l)
!
      use t_jacobian_1d
      use cal_1edge_jacobians
      use cal_shape_function_1d
!
      type(jacobians_1d), intent(inout) :: jac_1d_l
!
!
      call s_cal_shape_function_1d_linear(jac_1d_l%ntot_int,            &
     &    jac_1d_l%an_edge, dnxi_ed1, xi1)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_1d_2                                            &
     &   (node1%numnod, edge1%numedge, edge1%nnod_4_edge,               &
     &    edge1%ie_edge, node1%xx, np_smp, edge1%istack_edge_smp,       &
     &    jac_1d_l%ntot_int, jac_1d_l%xj_edge, jac_1d_l%axj_edge,       &
     &    jac_1d_l%xeg_edge, dnxi_ed1)
!
      end subroutine cal_jacobian_edge_linear
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_dylinear(sf_grp, jac_sf_grp_l)
!
      use t_group_data
      use t_jacobian_2d
      use cal_jacobian_sf_grp_linear
      use cal_shape_function_2d
!
      type(surface_group_data), intent(in) :: sf_grp
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
          call s_cal_jacobian_sf_grp_4                                  &
     &       (node1%numnod, ele1%numele, ele1%ie, node1%xx,             &
     &        sf_grp%num_grp, sf_grp%num_item, sf_grp%item_sf_grp,      &
     &        np_smp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp,        &
     &        jac_sf_grp_l%xj_sf(1:sf_grp%num_item,ix),                 &
     &        jac_sf_grp_l%axj_sf(1:sf_grp%num_item,ix),                &
     &        jac_sf_grp_l%xsf_sf(1:sf_grp%num_item,ix,1),              &
     &        jac_sf_grp_l%xsf_sf(1:sf_grp%num_item,ix,2),              &
     &        jac_sf_grp_l%xsf_sf(1:sf_grp%num_item,ix,3),              &
     &        dnxi_sf1(1,ix), dnei_sf1(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_dylinear
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_linear
