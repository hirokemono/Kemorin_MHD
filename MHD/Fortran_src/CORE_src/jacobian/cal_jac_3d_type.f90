!
!      module cal_jac_3d_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June, 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine cal_jac_3d_linear_type(mesh, xjac, axjac,             &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy, deidy,      &
!     &          dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!      subroutine cal_jac_3d_quad_type(mesh, xjac, axjac, dnx, dny, dnz,&
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!      subroutine cal_jac_3d_lag_type(mesh, xjac, axjac, dnx, dny, dnz, &
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!      subroutine cal_jac_3d_lin_quad_type                              &
!     &         (mesh, xjac, axjac, dnx, dny, dnz,                      &
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!        type(mesh_geometry), intent(in) :: mesh
!        real(kind = kreal), intent(in) :: dnxi(mesh%ele%nnod_4_ele)
!        real(kind = kreal), intent(in) :: dnei(mesh%ele%nnod_4_ele)
!        real(kind = kreal), intent(in) :: dnzi(mesh%ele%nnod_4_ele)
!
!        real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
!        real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
!        real(kind = kreal), intent(inout)                              &
!     &      :: dnx(mesh%ele%numele,mesh%ele%nnod_4_ele)
!        real(kind = kreal), intent(inout)                              &
!     &      :: dny(mesh%ele%numele,mesh%ele%nnod_4_ele)
!        real(kind = kreal), intent(inout)                              &
!     &      :: dnz(mesh%ele%numele,mesh%ele%nnod_4_ele)
!
      module cal_jac_3d_type
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_mesh_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_3d_linear_type(mesh, xjac, axjac,              &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy, deidy,       &
     &          dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use cal_jacobian_3d_linear
!
      type(mesh_geometry), intent(in) :: mesh
      real(kind = kreal), intent(in) :: dnxi(num_t_linear)
      real(kind = kreal), intent(in) :: dnei(num_t_linear)
      real(kind = kreal), intent(in) :: dnzi(num_t_linear)
!
      real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
      real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnx(mesh%ele%numele,num_t_linear)
      real(kind = kreal), intent(inout)                                 &
     &      :: dny(mesh%ele%numele,num_t_linear)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnz(mesh%ele%numele,num_t_linear)
!
!
      call s_cal_jacobian_3d_8(mesh%node%numnod, mesh%ele%numele,       &
     &    np_smp, mesh%ele%istack_ele_smp,                              &
     &    mesh%ele%ie(1:mesh%ele%numele,1:num_t_linear),                &
     &    mesh%node%xx, xjac, axjac, dnx, dny, dnz,                     &
     &    dxidx, deidx, dzidx, dxidy, deidy, dzidy,                     &
     &    dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      end subroutine cal_jac_3d_linear_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_3d_quad_type(mesh, xjac, axjac, dnx, dny, dnz, &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use cal_jacobian_3d_quad
!
      type(mesh_geometry), intent(in) :: mesh
      real(kind = kreal), intent(in) :: dnxi(num_t_quad)
      real(kind = kreal), intent(in) :: dnei(num_t_quad)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad)
!
      real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
      real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnx(mesh%ele%numele,num_t_quad)
      real(kind = kreal), intent(inout)                                 &
     &      :: dny(mesh%ele%numele,num_t_quad)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnz(mesh%ele%numele,num_t_quad)
!
!
      call s_cal_jacobian_3d_20(mesh%node%numnod, mesh%ele%numele,      &
     &    np_smp, mesh%ele%istack_ele_smp,                              &
     &    mesh%ele%ie(1:mesh%ele%numele,1:num_t_quad),                  &
     &    mesh%node%xx, xjac, axjac, dnx, dny, dnz,                     &
     &    dxidx, deidx, dzidx, dxidy, deidy, dzidy,                     &
     &    dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      end subroutine cal_jac_3d_quad_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_jac_3d_lag_type(mesh, xjac, axjac, dnx, dny, dnz,  &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use cal_jacobian_3d_lag
!
      type(mesh_geometry), intent(in) :: mesh
      real(kind = kreal), intent(in) :: dnxi(num_t_lag)
      real(kind = kreal), intent(in) :: dnei(num_t_lag)
      real(kind = kreal), intent(in) :: dnzi(num_t_lag)
!
      real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
      real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnx(mesh%ele%numele,num_t_lag)
      real(kind = kreal), intent(inout)                                 &
     &      :: dny(mesh%ele%numele,num_t_lag)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnz(mesh%ele%numele,num_t_lag)
!
!
      call s_cal_jacobian_3d_27(mesh%node%numnod, mesh%ele%numele,      &
     &    np_smp, mesh%ele%istack_ele_smp,                              &
     &    mesh%ele%ie(1:mesh%ele%numele,1:num_t_lag),                   &
     &    mesh%node%xx, xjac, axjac, dnx, dny, dnz,                     &
     &    dxidx, deidx, dzidx, dxidy, deidy, dzidy,                     &
     &    dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      end subroutine cal_jac_3d_lag_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jac_3d_lin_quad_type                               &
     &         (mesh, xjac, axjac, dnx, dny, dnz,                       &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use cal_jacobian_3d_linear_quad
!
      type(mesh_geometry), intent(in) :: mesh
      real(kind = kreal), intent(in) :: dnxi(num_t_quad)
      real(kind = kreal), intent(in) :: dnei(num_t_quad)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad)
!
      real(kind = kreal), intent(inout) :: dxidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidx(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidy(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dxidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: deidz(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: dzidz(mesh%ele%numele)
!
      real(kind = kreal), intent(inout) :: xjac(mesh%ele%numele)
      real(kind = kreal), intent(inout) :: axjac(mesh%ele%numele)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnx(mesh%ele%numele,num_t_lag)
      real(kind = kreal), intent(inout)                                 &
     &      :: dny(mesh%ele%numele,num_t_lag)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnz(mesh%ele%numele,num_t_lag)
!
!
      call cal_jacobian_3d_8_20(mesh%node%numnod, mesh%ele%numele,      &
     &    np_smp, mesh%ele%istack_ele_smp,                              &
     &    mesh%ele%ie(1:mesh%ele%numele,1:num_t_lag),                   &
     &    mesh%node%xx, xjac, axjac, dnx, dny, dnz,                     &
     &    dxidx, deidx, dzidx, dxidy, deidy, dzidy,                     &
     &    dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      end subroutine cal_jac_3d_lin_quad_type
!
!-----------------------------------------------------------------------
!
      end module cal_jac_3d_type
