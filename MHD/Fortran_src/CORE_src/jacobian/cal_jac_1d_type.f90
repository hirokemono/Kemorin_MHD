!
!      module cal_jac_1d_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine cal_jac_1d_linear_type(mesh, edge_mesh, xjac, axjac,  &
!     &          xeg, yeg, zeg, dnxi)
!      subroutine cal_jac_1d_quad_type(mesh, edge_mesh, xjac, axjac,    &
!     &          xeg, yeg, zeg, dnxi)
!      subroutine cal_jac_1d_l_quad_type(mesh, edge_mesh, xjac, axjac,  &
!     &          xeg, yeg, zeg, dnxi)
!        type(mesh_geometry), intent(in) :: mesh
!        type(edge_geometry), intent(in)  :: edge_mesh
!
!        real(kind=kreal), intent(in) :: dnxi(edge_mesh%edge%nnod_4_edge)
!
!        real(kind=kreal), intent(inout) :: xjac(edge_mesh%edge%numedge)
!        real(kind=kreal), intent(inout) :: axjac(edge_mesh%edge%numedge)
!        real(kind=kreal), intent(inout) :: xeg(edge_mesh%edge%numedge)
!        real(kind=kreal), intent(inout) :: yeg(edge_mesh%edge%numedge)
!        real(kind=kreal), intent(inout) :: zeg(edge_mesh%edge%numedge)
!
      module cal_jac_1d_type
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_mesh_data
      use cal_jacobian_1d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_1d_linear_type(mesh, edge_mesh, xjac, axjac,   &
     &          xeg, yeg, zeg, dnxi)
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
!
      real(kind=kreal), intent(in) :: dnxi(num_linear_edge)
!
      real(kind=kreal), intent(inout) :: xjac(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: axjac(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: xeg(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: yeg(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: zeg(edge_mesh%edge%numedge)
!
!
      call s_cal_jacobian_1d_2(mesh%node%numnod,                        &
    &     edge_mesh%edge%numedge, edge_mesh%edge%ie_edge, mesh%node%xx, &
    &     np_smp, edge_mesh%edge%istack_edge_smp,                       &
    &     xjac, axjac, xeg, yeg, zeg, dnxi)
!
      end subroutine cal_jac_1d_linear_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_1d_quad_type(mesh, edge_mesh, xjac, axjac,     &
     &          xeg, yeg, zeg, dnxi)
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
!
      real(kind=kreal), intent(in) :: dnxi(num_quad_edge)
!
      real(kind=kreal), intent(inout) :: xjac(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: axjac(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: xeg(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: yeg(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: zeg(edge_mesh%edge%numedge)
!
!
      call s_cal_jacobian_1d_3(mesh%node%numnod,                        &
    &     edge_mesh%edge%numedge, edge_mesh%edge%ie_edge, mesh%node%xx, &
    &     np_smp, edge_mesh%edge%istack_edge_smp,                       &
    &     xjac, axjac, xeg, yeg, zeg, dnxi)
!
      end subroutine cal_jac_1d_quad_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_1d_l_quad_type(mesh, edge_mesh, xjac, axjac,   &
     &          xeg, yeg, zeg, dnxi)
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(in)  :: edge_mesh
!
      real(kind=kreal), intent(in) :: dnxi(num_quad_edge)
!
      real(kind=kreal), intent(inout) :: xjac(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: axjac(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: xeg(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: yeg(edge_mesh%edge%numedge)
      real(kind=kreal), intent(inout) :: zeg(edge_mesh%edge%numedge)
!
!
      call s_cal_jacobian_1d_2_3(mesh%node%numnod,                      &
    &     edge_mesh%edge%numedge, edge_mesh%edge%ie_edge, mesh%node%xx, &
    &     np_smp, edge_mesh%edge%istack_edge_smp,                       &
    &     xjac, axjac, xeg, yeg, zeg, dnxi)
!
      end subroutine cal_jac_1d_l_quad_type
!
!-----------------------------------------------------------------------
!
      end module cal_jac_1d_type
