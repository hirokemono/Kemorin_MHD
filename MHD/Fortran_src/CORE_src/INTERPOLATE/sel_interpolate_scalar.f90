!>@file   sel_interpolate_scalar.f90
!!@brief  module sel_interpolate_scalar.f90
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  interpolation on each subdomains
!!
!!@verbatim
!!      subroutine s_sel_interpolate_scalar_ele                         &
!!     &         (np_smp, numnod, numele, nnod_4_ele, ie,               &
!!     &          v_org, istack_smp, num_points, iele_gauss,            &
!!     &          xi_gauss, vect)
!!        integer (kind = kint), intent(in) :: np_smp
!!        integer (kind = kint), intent(in) :: numnod, numele
!!        integer (kind = kint), intent(in) :: ie(numele,20)
!!        integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
!!        integer (kind = kint), intent(in) :: num_points
!!        integer (kind = kint), intent(in) :: iele_gauss(num_points)
!!        real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
!!        real (kind=kreal), intent(in) :: v_org(numnod)
!!        real (kind=kreal), intent(inout) :: vect(num_points)
!!@endverbatim
!
      module sel_interpolate_scalar
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_sel_interpolate_scalar_ele                           &
     &         (np_smp, numnod, numele, nnod_4_ele, ie,                 &
     &          v_org, istack_smp, num_points, iele_gauss,              &
     &          xi_gauss, vect)
!
      use interpolate_scalar_ele8
      use interpolate_scalar_ele20
      use interpolate_scalar_ele27
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      real (kind=kreal), intent(inout) :: vect(num_points)
!
      if (ele%nnod_4_ele .eq. num_t_linear) then
        call s_interpolate_scalar_ele8                                  &
     &     (np_smp, n_point, numele, ie, v_org, istack_smp,             &
     &      num_points, iele_gauss, xi_gauss, vect)
      else if(ele%nnod_4_ele .eq. num_t_quad)
        call s_interpolate_scalar_ele27                                 &
     &     (np_smp, n_point, numele, ie, v_org, istack_smp,             &
     &      num_points, iele_gauss, xi_gauss, vect)
      else if(ele%nnod_4_ele .eq. num_t_lag) then
!
        call s_interpolate_scalar_ele20                                 &
     &     (np_smp, n_point, numele, ie, v_org, istack_smp,             &
     &      num_points, iele_gauss, xi_gauss, vect)
      end if
!
      end subroutine s_sel_interpolate_scalar_ele
!
!------------------------------------------------------------------
!
      end module sel_interpolate_scalar
