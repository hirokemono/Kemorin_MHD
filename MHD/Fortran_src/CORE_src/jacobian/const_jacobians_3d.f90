!
!      module const_jacobians_3d
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine cal_jacobian_trilinear(node, ele, jac_3d)
!      subroutine cal_jacobian_quad(node, ele, jac_3d)
!      subroutine cal_jacobian_lag(node, ele, jac_3d)
!      subroutine cal_jacobian_quad_on_linear(node, ele, jac_3d)
!
      module const_jacobians_3d
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      use t_geometry_data
      use t_jacobians
      use cal_1ele_jacobians
      use cal_shape_function_3d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_trilinear(node, ele, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_function_linear(jac_3d%ntot_int, jac_3d%an,      &
     &    dnxi_1, dnei_1, dnzi_1, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_8                                            &
     &   (node%numnod, ele%numele, ele%nnod_4_ele,                      &
     &    np_smp, ele%istack_ele_smp, ele%ie, node%xx,                  &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_1, dnei_1, dnzi_1)
!
      end subroutine cal_jacobian_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_quad(node, ele, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_function_quad(jac_3d%ntot_int, jac_3d%an,        &
     &    dnxi_20, dnei_20, dnzi_20, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_20                                           &
     &   (node%numnod, ele%numele, ele%nnod_4_ele,                      &
     &    np_smp, ele%istack_ele_smp, ele%ie, node%xx,                  &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d, dnxi_20, dnei_20, dnzi_20)
!
      end subroutine cal_jacobian_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_lag(node, ele, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_function_lag(jac_3d%ntot_int, jac_3d%an,         &
     &    dnxi_27, dnei_27, dnzi_27, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_27                                           &
     &   (node%numnod, ele%numele, ele%nnod_4_ele,                      &
     &    np_smp, ele%istack_ele_smp, ele%ie, node%xx,                  &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d,  dnxi_27, dnei_27, dnzi_27)
!
      end subroutine cal_jacobian_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_quad_on_linear(node, ele, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_function_quad(jac_3d%ntot_int, jac_3d%an,        &
     &    dnxi_20, dnei_20, dnzi_20, xi3, ei3, zi3)
!
!   jacobian for quadrature elaments
!
      call cal_jacobian_3d_8_20                                         &
     &       (node%numnod, ele%numele, ele%nnod_4_ele,                  &
     &        np_smp, ele%istack_ele_smp, ele%ie, node%xx,              &
     &        jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,               &
     &        jac_3d%dnx, jac_3d%dxidx_3d, dnxi_20, dnei_20, dnzi_20)
!
      end subroutine cal_jacobian_quad_on_linear
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_shape_func_from_array(ntot_int_3d, nnod_4_ele,    &
     &          an_org, an_tgt)
!
      integer(kind = kint), intent(in) :: ntot_int_3d, nnod_4_ele
      real(kind=kreal), intent(in) ::    an_org(nnod_4_ele,ntot_int_3d)
      real(kind=kreal), intent(inout) :: an_tgt(nnod_4_ele,ntot_int_3d)
      integer(kind = kint) :: ix, k1
!
!
      do ix = 1, ntot_int_3d
        do k1 = 1, nnod_4_ele
          an_tgt(k1,ix) = an_org(k1,ix)
        end do
      end do
!
      end subroutine copy_shape_func_from_array
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_3d
