!
!     module const_jacobians_1d_type
!
!      Written by H. Matsui on Dec., 2008
!
!      subroutine cal_jacobian_type_1d_linear(mesh, edge_mesh, jac_1d)
!      subroutine cal_jacobian_type_1d_quad(mesh, edge_mesh, jac_1d)
!      subroutine cal_jacobian_type_1d_l_quad(mesh, edge_mesh, jac_1d)
!        type(mesh_geometry), intent(in) :: mesh
!        type(edge_geometry), intent(in)  :: edge_mesh
!        type(jacobians_1d), intent(inout) :: jac_1d
!
      module const_jacobians_1d_type
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
      subroutine cal_jacobian_type_1d_linear(mesh, edge_mesh, jac_1d)
!
      use m_jacobians_4_edge
      use t_mesh_data
      use t_jacobians
      use cal_jac_1d_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_1d%ntot_int,                  &
     &    edge_mesh%edge%nnod_4_edge, an_edge, jac_1d%an_edge)
!
!   jacobian for tri-linear elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
!
          ix = int_start1(i0) + ii
!
          call cal_jac_1d_linear_type(mesh, edge_mesh,                 &
     &        jac_1d%xj_edge(1:edge_mesh%edge%numedge,ix),             &
     &        jac_1d%axj_edge(1:edge_mesh%edge%numedge,ix),            &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,1),          &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,2),          &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,3),          &
     &        dnxi_ed1(1:edge_mesh%edge%nnod_4_edge,ix) )
!
        end do
      end do
!
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
      use cal_jac_1d_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_1d%ntot_int,                  &
     &    edge_mesh%edge%nnod_4_edge, aw_edge, jac_1d%an_edge)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
!
          ix = int_start1(i0) + ii
!
          call cal_jac_1d_quad_type(mesh, edge_mesh,                   &
     &        jac_1d%xj_edge(1:edge_mesh%edge%numedge,ix),             &
     &        jac_1d%axj_edge(1:edge_mesh%edge%numedge,ix),            &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,1),          &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,2),          &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,3),          &
     &        dnxi_ed20(1:edge_mesh%edge%nnod_4_edge,ix) )
!
        end do
      end do
!
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
      use cal_jac_1d_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
      type(jacobians_1d), intent(inout) :: jac_1d
!
      integer (kind = kint) :: ii, ix, i0
!
!
      call copy_shape_func_from_array(jac_1d%ntot_int,                  &
     &    edge_mesh%edge%nnod_4_edge, aw_edge, jac_1d%an_edge)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
!
          ix = int_start1(i0) + ii
!
          call cal_jac_1d_l_quad_type(mesh, edge_mesh,                  &
     &        jac_1d%xj_edge(1:edge_mesh%edge%numedge,ix),             &
     &        jac_1d%axj_edge(1:edge_mesh%edge%numedge,ix),            &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,1),          &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,2),          &
     &        jac_1d%xeg_edge(1:edge_mesh%edge%numedge,ix,3),          &
     &        dnxi_ed20(1:edge_mesh%edge%nnod_4_edge,ix) )
!
        end do
      end do
!
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
