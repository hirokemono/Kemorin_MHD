!>@file  cal_1point_jacobian_linear.f90
!!       module cal_1point_jacobian_linear
!!
!!@author H. Matsui
!!@date   Programmed on Dec., 2008
!!@n      Modified by H. Matsui on Aug., 2015
!
!> @brief  obtain jacobian at one gauss point for linear element
!!
!!@verbatim
!!      subroutine cal_jacobian_3d_8(numnod, numele, nnod_4_ele,        &
!!     &        np_smp, iele_smp_stack, ie, xx,                         &
!!     &        ntot_int_3d, xjac, axjac, dnx,                          &
!!     &        dxidx_1, dnxi_1, dnei_1, dnzi_1)
!!@end verbatim
!
      module cal_1point_jacobian_linear
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_3d_8(numnod, numele, nnod_4_ele,          &
     &        np_smp, iele_smp_stack, ie, xx,                           &
     &        ntot_int_3d, xjac, axjac, dnx,                            &
     &        dxidx_1, dnxi_1, dnei_1, dnzi_1)
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use cal_jacobian_3d_linear
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_int_3d
      real(kind = kreal), intent(in)                                    &
     &      :: dnxi_1(num_t_linear,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &      :: dnei_1(num_t_linear,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &      :: dnzi_1(num_t_linear,ntot_int_3d)
!
      real(kind = kreal), intent(inout)                                 &
     &      :: dxidx_1(numele,ntot_int_3d,3,3)
!
      real(kind = kreal), intent(inout) :: xjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout) :: axjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout)                                 &
     &      :: dnx(numele,num_t_linear,ntot_int_3d,3)
!
      integer (kind = kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_8                                      &
     &       (numnod, numele, np_smp, iele_smp_stack,                   &
     &        ie(1,1), xx, xjac(1,ix), axjac(1,ix),                     &
     &        dnx(1,1,ix,1), dnx(1,1,ix,2), dnx(1,1,ix,3),              &
     &        dxidx_1(1,ix,1,1), dxidx_1(1,ix,2,1), dxidx_1(1,ix,3,1),  &
     &        dxidx_1(1,ix,1,2), dxidx_1(1,ix,2,2), dxidx_1(1,ix,3,2),  &
     &        dxidx_1(1,ix,1,3), dxidx_1(1,ix,2,3), dxidx_1(1,ix,3,3),  &
     &        dnxi_1(1,ix), dnei_1(1,ix), dnzi_1(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_3d_8
!
!-----------------------------------------------------------------------
!
      end module cal_1point_jacobian_linear
