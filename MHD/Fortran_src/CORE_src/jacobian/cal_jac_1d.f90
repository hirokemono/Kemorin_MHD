!
!      module cal_jac_1d
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine s_cal_jacobian_1d_linear(xjac, axjac, xeg, yeg, zeg,  &
!     &          dnxi)
!      subroutine s_cal_jacobian_1d_quad(xjac, axjac, xeg, yeg, zeg,    &
!     &          dnxi)
!      subroutine s_cal_jacobian_1d_l_quad(xjac, axjac, xeg, yeg, zeg,  &
!     &          dnxi)
!
      module cal_jac_1d
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
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
      subroutine s_cal_jacobian_1d_linear(xjac, axjac, xeg, yeg, zeg,   &
     &          dnxi)
!
      real(kind = kreal), intent(in) :: dnxi(num_linear_edge)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
      real(kind = kreal), intent(inout) :: yeg(numedge)
      real(kind = kreal), intent(inout) :: zeg(numedge)
!
!
      call s_cal_jacobian_1d_2(numnod, numedge,                         &
    &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,      &
    &           xeg, yeg, zeg,  dnxi)
!
      end subroutine s_cal_jacobian_1d_linear
!
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_1d_quad(xjac, axjac, xeg, yeg, zeg,     &
     &          dnxi)
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_edge)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
      real(kind = kreal), intent(inout) :: yeg(numedge)
      real(kind = kreal), intent(inout) :: zeg(numedge)
!
!
      call s_cal_jacobian_1d_3(numnod, numedge,                         &
    &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,      &
    &           xeg, yeg, zeg,  dnxi)
!
      end subroutine s_cal_jacobian_1d_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_1d_l_quad(xjac, axjac, xeg, yeg, zeg,   &
     &          dnxi)
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_edge)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
      real(kind = kreal), intent(inout) :: yeg(numedge)
      real(kind = kreal), intent(inout) :: zeg(numedge)
!
!
      call s_cal_jacobian_1d_2_3(numnod, numedge,                       &
    &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,      &
    &           xeg, yeg, zeg,  dnxi)
!
      end subroutine s_cal_jacobian_1d_l_quad
!
!-----------------------------------------------------------------------
!
      end module cal_jac_1d
