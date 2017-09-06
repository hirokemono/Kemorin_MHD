!
!      module const_jacobians_3d
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!!      subroutine initialize_FEM_integration
!!
!!      subroutine sel_jacobian_type(node, ele, spf_3d, jac_3d)
!!      subroutine cal_jacobian_trilinear(node, ele, jac_3d)
!!      subroutine cal_jacobian_quad_on_linear                          &
!!     &         (node, ele, spf_3d_20, jac_3d)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(scalar_surf_BC_list), intent(in) :: infinity_list
!!        type(jacobians_3d), intent(inout) :: jac_3d
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
      use t_shape_functions
      use t_geometry_data
      use t_jacobian_3d
      use t_group_data
      use t_surface_boundary
!
      use cal_1ele_jacobians
      use cal_shape_function_3d
!
      implicit none
!
      private :: cal_jacobian_quad, cal_jacobian_lag
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for hexadedral element
!
      subroutine initialize_FEM_integration
!
      use set_gauss_int_parameters
      use set_integration_indices
!
!  data allocation
!
      call allocate_integrate_parameters
      call allocate_gauss_coef_4_fem
!
!  set constant for gauss integration with roots
!
      call init_gauss_int_parameters
!
!  set indices for gauss integration
!
      call set_integrate_indices_1d
      call set_integrate_indices_2d
      call set_integrate_indices_3d
!
!  set weighting for integration
!
      call set_gauss_coefs_4_1d
      call set_gauss_coefs_4_2d
      call set_gauss_coefs_4_3d
!
!
      end subroutine initialize_FEM_integration
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_jacobian_type(node, ele, spf_3d, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(volume_shape_function), intent(inout) :: spf_3d
      type(jacobians_3d), intent(inout) :: jac_3d
!
!  set jacobians
!
      if (ele%nnod_4_ele .eq. num_t_linear) then
        call cal_jacobian_trilinear(node, ele, spf_3d, jac_3d)
      else if (ele%nnod_4_ele .eq. num_t_quad) then
        call cal_jacobian_quad(node, ele, spf_3d, jac_3d)
      else if (ele%nnod_4_ele .eq. num_t_lag) then
        call cal_jacobian_lag(node, ele, spf_3d, jac_3d)
      end if
!
      end subroutine sel_jacobian_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_trilinear(node, ele, spf_3d_8, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(volume_shape_function), intent(inout) :: spf_3d_8
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_function_linear(jac_3d%ntot_int, jac_3d%an,      &
     &    spf_3d_8%dnxi, spf_3d_8%dnei, spf_3d_8%dnzi, xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_8                                            &
     &   (node%numnod, ele%numele, ele%nnod_4_ele,                      &
     &    np_smp, ele%istack_ele_smp, ele%ie, node%xx,                  &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d,                                  &
     &    spf_3d_8%dnxi, spf_3d_8%dnei, spf_3d_8%dnzi)
!
      end subroutine cal_jacobian_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_quad(node, ele, spf_3d_20, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(volume_shape_function), intent(inout) :: spf_3d_20
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_function_quad(jac_3d%ntot_int, jac_3d%an,        &
     &    spf_3d_20%dnxi, spf_3d_20%dnei, spf_3d_20%dnzi,               &
     &    xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_20                                           &
     &   (node%numnod, ele%numele, ele%nnod_4_ele,                      &
     &    np_smp, ele%istack_ele_smp, ele%ie, node%xx,                  &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d,                                  &
     &    spf_3d_20%dnxi, spf_3d_20%dnei, spf_3d_20%dnzi)
!
      end subroutine cal_jacobian_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_lag(node, ele, spf_3d_27, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(volume_shape_function), intent(inout) :: spf_3d_27
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_function_lag(jac_3d%ntot_int, jac_3d%an,         &
     &    spf_3d_27%dnxi, spf_3d_27%dnei, spf_3d_27%dnzi,               &
     &     xi3, ei3, zi3)
!
!   jacobian for tri-linear elaments
!
      call cal_jacobian_3d_27                                           &
     &   (node%numnod, ele%numele, ele%nnod_4_ele,                      &
     &    np_smp, ele%istack_ele_smp, ele%ie, node%xx,                  &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d,                                  &
     &    spf_3d_27%dnxi, spf_3d_27%dnei, spf_3d_27%dnzi)
!
      end subroutine cal_jacobian_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_quad_on_linear                            &
     &         (node, ele, spf_3d_20, jac_3d)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(volume_shape_function), intent(inout) :: spf_3d_20
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      call s_cal_shape_function_quad(jac_3d%ntot_int, jac_3d%an,        &
     &    spf_3d_20%dnxi, spf_3d_20%dnei, spf_3d_20%dnzi,               &
     &    xi3, ei3, zi3)
!
!   jacobian for quadrature elaments
!
      call cal_jacobian_3d_8_20                                         &
     &   (node%numnod, ele%numele, ele%nnod_4_ele,                      &
     &    np_smp, ele%istack_ele_smp, ele%ie, node%xx,                  &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%axjac,                   &
     &    jac_3d%dnx, jac_3d%dxidx_3d,                                  &
     &    spf_3d_20%dnxi, spf_3d_20%dnei, spf_3d_20%dnzi)
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
