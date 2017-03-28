!>@file  const_jacobians_1d.f90
!!       module const_jacobians_1d
!!
!!@author H. Matsui
!!@date   Programmed in Dec., 2008
!
!> @brief  Construct Jacobians on edge
!!
!!@verbatim
!!      subroutine cal_jacobian_edge(node, edge, jac_1d_l, jac_1d_q)
!!        type(node_data), intent(in) :: node
!!        type(edge_data), intent(in)  :: edge
!!        type(jacobians_1d), intent(inout) :: jac_1d_l
!!        type(jacobians_1d), intent(inout) :: jac_1d_q
!!
!!      subroutine sel_jacobian_edge_type(node, edge, jac_1d)
!!      subroutine cal_jacobian_edge_linear(node, edge, jac_1d)
!!      subroutine cal_jacobian_edge_quad(node, edge, jac_1d)
!!      subroutine cal_jacobian_edge_quad_on_l(node, edge, jac_1d)
!!        type(jacobians_1d), intent(inout) :: jac_1d
!!@endverbatim
!
      module const_jacobians_1d
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      use t_geometry_data
      use t_edge_data
      use t_jacobian_1d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for edge element
!
      subroutine cal_jacobian_edge(node, edge, jac_1d_l, jac_1d_q)
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
!
      type(jacobians_1d), intent(inout) :: jac_1d_l
      type(jacobians_1d), intent(inout) :: jac_1d_q
!
!
      call alloc_1d_jac_type                                            &
     &   (edge%numedge, num_linear_edge, maxtot_int_1d, jac_1d_l)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_edge_linear'
      call cal_jacobian_edge_linear(node, edge, jac_1d_l)
!
      if(edge%nnod_4_edge .eq. num_quad_edge) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_edge_quad'
        call alloc_1d_jac_type                                          &
     &     (edge%numedge, edge%nnod_4_edge, maxtot_int_1d, jac_1d_q)
        call cal_jacobian_edge_quad(node, edge, jac_1d_q)
      else
        if (iflag_debug.eq.1) write(*,*) 'copy_1d_jacobians'
        call copy_1d_jacobians                                          &
     &     (edge%numedge, num_linear_edge, jac_1d_l, jac_1d_q)
      end if
!
      end subroutine cal_jacobian_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_jacobian_edge_type(node, edge, jac_1d)
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      if      (edge%nnod_4_edge .eq. num_linear_edge) then
        call cal_jacobian_edge_linear(node, edge, jac_1d)
      else if (edge%nnod_4_edge .eq. num_quad_edge) then
        call cal_jacobian_edge_quad(node, edge, jac_1d)
      end if
!
      end subroutine sel_jacobian_edge_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_linear(node, edge, jac_1d)
!
      use cal_1edge_jacobians
      use cal_shape_function_1d
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call s_cal_shape_function_1d_linear                               &
     &   (jac_1d%ntot_int, jac_1d%an_edge, dnxi_ed1, xi1)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_1d_2                                            &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge, edge%ie_edge,    &
     &    node%xx, np_smp, edge%istack_edge_smp,                        &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, dnxi_ed1)
!
      end subroutine cal_jacobian_edge_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_quad(node, edge, jac_1d)
!
      use cal_1edge_jacobians
      use cal_shape_function_1d
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call s_cal_shape_function_1d_quad                                 &
     &   (jac_1d%ntot_int, jac_1d%an_edge, dnxi_ed20, xi1)
!
!   jacobian for quadrature elaments
!
      call cal_jacobian_1d_3                                            &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge, edge%ie_edge,    &
     &    node%xx, np_smp, edge%istack_edge_smp,                        &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, dnxi_ed20)
!
      end subroutine cal_jacobian_edge_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_edge_quad_on_l(node, edge, jac_1d)
!
      use cal_1edge_jacobians
      use cal_shape_function_1d
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in)  :: edge
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call s_cal_shape_function_1d_quad(jac_1d%ntot_int,                &
     &    jac_1d%an_edge, dnxi_ed20, xi1)
!
!   jacobian for quadrature elaments
      call cal_jacobian_1d_2_3                                          &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge,                  &
     &    edge%ie_edge, node%xx, np_smp, edge%istack_edge_smp,          &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, dnxi_ed20)
!
      end subroutine cal_jacobian_edge_quad_on_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_shape_func_from_array(ntot_int_1d, nnod_4_edge,   &
     &          an_org, an_tgt)
!
      integer(kind = kint), intent(in) :: ntot_int_1d, nnod_4_edge
      real(kind=kreal), intent(in) :: an_org(nnod_4_edge,ntot_int_1d)
      real(kind=kreal), intent(inout)                                   &
     &                 :: an_tgt(nnod_4_edge,ntot_int_1d)
      integer(kind = kint) :: ix, k1
!
!
      do ix = 1, ntot_int_1d
        do k1 = 1, nnod_4_edge
          an_tgt(k1,ix) = an_org(k1,ix)
        end do
      end do
!
      end subroutine copy_shape_func_from_array
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_1d
