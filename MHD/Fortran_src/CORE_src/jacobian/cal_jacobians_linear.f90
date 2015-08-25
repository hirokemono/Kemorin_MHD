!
!      module cal_jacobians_linear
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_trilinear
!      subroutine cal_jacobian_quad
!      subroutine cal_jacobian_lag
!      subroutine cal_jacobian_quad_on_linear
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
     &        jac1_3d_l%dnx, dxidx_1, dnxi_1, dnei_1, dnzi_1)
!
      end subroutine cal_jacobian_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_quad
!
      use m_jacobians
      use cal_1ele_jacobians
      use cal_shape_function_3d
!
!
      call s_cal_shape_function_quad(jac1_3d_q%ntot_int, jac1_3d_q%an,  &
     &    dnxi_20, dnei_20, dnzi_20, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_20                                           &
     &       (node1%numnod, ele1%numele, ele1%nnod_4_ele, np_smp,       &
     &        ele1%istack_ele_smp, ele1%ie, node1%xx,                   &
     &        jac1_3d_q%ntot_int, jac1_3d_q%xjac, jac1_3d_q%axjac,      &
     &        dwx, dxidx_20, dnxi_20, dnei_20, dnzi_20)
!
      end subroutine cal_jacobian_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_lag
!
      use m_jacobians
      use cal_1ele_jacobians
      use cal_shape_function_3d
!
!
      call s_cal_shape_function_lag(jac1_3d_q%ntot_int, jac1_3d_q%an,   &
     &    dnxi_27, dnei_27, dnzi_27, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_27                                           &
     &       (node1%numnod, ele1%numele, ele1%nnod_4_ele,               &
     &        np_smp, ele1%istack_ele_smp, ele1%ie, node1%xx,           &
     &        jac1_3d_q%ntot_int, jac1_3d_q%xjac, jac1_3d_q%axjac, dwx, &
     &        dxidx_20, dnxi_27, dnei_27, dnzi_27)
!
      end subroutine cal_jacobian_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_quad_on_linear
!
      use m_jacobians
      use cal_1ele_jacobians
      use cal_shape_function_3d
!
!
      call s_cal_shape_function_quad                                    &
     &   (jac1_3d_lq%ntot_int, jac1_3d_lq%an,                           &
     &    dnxi_20, dnei_20, dnzi_20, xi3, ei3, zi3)
!
!   jacobian for quadrature elaments
!
      call cal_jacobian_3d_8_20                                         &
     &       (node1%numnod, ele1%numele, ele1%nnod_4_ele,               &
     &        np_smp, ele1%istack_ele_smp, ele1%ie, node1%xx,           &
     &        jac1_3d_lq%ntot_int, jac1_3d_lq%xjac, jac1_3d_lq%axjac,   &
     &        jac1_3d_lq%dnx, dxidx_lq, dnxi_20, dnei_20, dnzi_20)
!
      end subroutine cal_jacobian_quad_on_linear
!
!-----------------------------------------------------------------------
!
      end module cal_jacobians_linear
