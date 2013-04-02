!cal_jac_3d.f90
!      module cal_jac_3d
!
!        programmed by H.Matsui on Nov., 2008
!        Mmodified by H.Matsui on Dec., 2008
!
!      subroutine s_cal_jacobian_3d_linear(xjac, axjac, dnx, dny, dnz,  &
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!      subroutine s_cal_jacobian_3d_quad(xjac, axjac, dnx, dny, dnz,    &
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!      subroutine s_cal_jacobian_3d_lag(xjac, axjac, dnx, dny, dnz,     &
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
!      subroutine s_cal_jacobian_3d_lin_quad(xjac, axjac, dnx, dny, dnz,&
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      module cal_jac_3d
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_linear(xjac, axjac, dnx, dny, dnz,   &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use cal_jacobian_3d_linear
!
      real(kind = kreal), intent(in) :: dnxi(num_t_linear)
      real(kind = kreal), intent(in) :: dnei(num_t_linear)
      real(kind = kreal), intent(in) :: dnzi(num_t_linear)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_linear)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_linear)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_linear)
!
!
      call s_cal_jacobian_3d_8(numnod, numele,                          &
     &          np_smp, iele_smp_stack, ie, xx, xjac, axjac,            &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy, deidy,       &
     &          dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      end subroutine s_cal_jacobian_3d_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_quad(xjac, axjac, dnx, dny, dnz,     &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use cal_jacobian_3d_quad
!
      real(kind = kreal), intent(in) :: dnxi(num_t_quad)
      real(kind = kreal), intent(in) :: dnei(num_t_quad)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_quad)
!
!
      call s_cal_jacobian_3d_20(numnod, numele,                         &
     &          np_smp, iele_smp_stack, ie, xx, xjac, axjac,            &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy, deidy,       &
     &          dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      end subroutine s_cal_jacobian_3d_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_lag(xjac, axjac, dnx, dny, dnz,      &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use cal_jacobian_3d_lag
!
      real(kind = kreal), intent(in) :: dnxi(num_t_lag)
      real(kind = kreal), intent(in) :: dnei(num_t_lag)
      real(kind = kreal), intent(in) :: dnzi(num_t_lag)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_lag)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_lag)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_lag)
!
!
      call s_cal_jacobian_3d_27(numnod, numele,                         &
     &          np_smp, iele_smp_stack, ie, xx, xjac, axjac,            &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy, deidy,       &
     &          dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      end subroutine s_cal_jacobian_3d_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_lin_quad(xjac, axjac, dnx, dny, dnz, &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use cal_jacobian_3d_linear_quad
!
      real(kind = kreal), intent(in) :: dnxi(num_t_quad)
      real(kind = kreal), intent(in) :: dnei(num_t_quad)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_quad)
!
!
      call cal_jacobian_3d_8_20(numnod, numele,                         &
     &          np_smp, iele_smp_stack, ie, xx, xjac, axjac,            &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy, deidy,       &
     &          dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      end subroutine s_cal_jacobian_3d_lin_quad
!
!-----------------------------------------------------------------------
!
      end module cal_jac_3d
