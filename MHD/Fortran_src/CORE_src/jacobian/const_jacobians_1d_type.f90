!>@file  const_jacobians_1d_type.f90
!!       module const_jacobians_1d_type
!!
!!@author H. Matsui
!!@date   Programmed in Dec., 2008
!
!> @brief  Construct Jacobians on edge
!!
!!@verbatim
!!!      subroutine cal_jacobian_type_1d_linear(mesh, edge_mesh, jac_1d)
!!      subroutine cal_jacobian_type_1d_quad(mesh, edge_mesh, jac_1d)
!!      subroutine cal_jacobian_type_1d_l_quad(mesh, edge_mesh, jac_1d)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(edge_geometry), intent(in)  :: edge_mesh
!!        type(jacobians_1d), intent(inout) :: jac_1d
!!@endverbatim
!
      module const_jacobians_1d_type
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
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
      subroutine cal_jacobian_type_1d_linear(mesh, edge_mesh, jac_1d)
!
      use m_jacobians_4_edge
      use t_mesh_data
      use t_jacobians
      use cal_1edge_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call copy_shape_func_from_array(jac_1d%ntot_int,                  &
     &    edge_mesh%edge%nnod_4_edge, jac1_1d_l%an_edge,                &
     &    jac_1d%an_edge)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_1d_2(mesh%node%numnod, edge_mesh%edge%numedge,  &
     &    edge_mesh%edge%nnod_4_edge, edge_mesh%edge%ie_edge,           &
     &    mesh%node%xx, np_smp, edge_mesh%edge%istack_edge_smp,         &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, dnxi_ed1)
!
      end subroutine cal_jacobian_type_1d_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_1d_quad(mesh, edge_mesh, jac_1d)
!
      use m_jacobians_4_edge
      use t_mesh_data
      use t_jacobians
      use cal_1edge_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      call copy_shape_func_from_array(jac_1d%ntot_int,                  &
     &    edge_mesh%edge%nnod_4_edge, jac1_1d_q%an_edge,                &
     &    jac_1d%an_edge)
!
!   jacobian for quadrature elaments
!
      call cal_jacobian_1d_3(mesh%node%numnod, edge_mesh%edge%numedge,  &
     &    edge_mesh%edge%nnod_4_edge, edge_mesh%edge%ie_edge,           &
     &    mesh%node%xx, np_smp, edge_mesh%edge%istack_edge_smp,         &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, dnxi_ed1)
!
      end subroutine cal_jacobian_type_1d_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_type_1d_l_quad(mesh, edge_mesh, jac_1d)
!
      use m_jacobians_4_edge
      use t_mesh_data
      use t_jacobians
      use cal_1edge_jacobians
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_1d%ntot_int,                  &
     &    edge_mesh%edge%nnod_4_edge, jac1_1d_ql%an_edge,               &
     &    jac_1d%an_edge)
!
!   jacobian for quadrature elaments
      call cal_jacobian_1d_2_3(mesh%node%numnod, edge_mesh%edge%numedge,  &
     &    edge_mesh%edge%nnod_4_edge, edge_mesh%edge%ie_edge,           &
     &    mesh%node%xx, np_smp, edge_mesh%edge%istack_edge_smp,         &
     &    jac_1d%ntot_int, jac_1d%xj_edge, jac_1d%axj_edge,             &
     &    jac_1d%xeg_edge, dnxi_ed1)
!
      end subroutine cal_jacobian_type_1d_l_quad
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
      end module const_jacobians_1d_type
