!>@file  cal_shape_func_infty_1d.f90
!!       module cal_shape_func_infty_1d
!!
!!@author H. Matsui and H.Okuda
!!@date   Programmed in July, 2000
!!@n      modified by H. Matsui on Aug., 2006
!
!> @brief  caliculate shape function and differences at Gauss points
!!
!!@verbatim
!!      subroutine s_cal_shape_func_inf_1d_linear(ntot_int_1d, xk,      &
!!     &          an_infty, dnxi_infty, xi)
!!      subroutine s_cal_shape_func_inf_1d_quad(ntot_int_1d, xk,        &
!!     &          an_infty, dnxi_infty, xi)
!!@endverbatim
!
      module cal_shape_func_infty_1d
!
      use m_precision
!
      use m_geometry_constants
      use set_shape_elements_infty_sf
!
      implicit none
!
      real (kind=kreal) :: xi_nega, xi_posi, xi_sqre
      real (kind=kreal) :: xi_inf, dxi_inf
!
      private :: xi_nega, xi_posi, xi_sqre, xi_inf, dxi_inf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_func_inf_1d_linear(ntot_int_1d, xk,        &
     &          an_infty, dnxi_infty, xi)
!
      use shape_func_1d_linear
!
      integer (kind=kint), intent(in) :: ntot_int_1d
      real (kind=kreal), intent(in) :: xk
      real(kind=kreal), intent(in) :: xi(ntot_int_1d)
!
      real (kind=kreal), intent(inout)                                  &
     &      :: an_infty(num_linear_edge,num_linear_edge,ntot_int_1d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnxi_infty(num_linear_edge,num_linear_edge,ntot_int_1d)
!
      integer (kind=kint) :: isf, ix
!
!
      do isf = 1, num_linear_edge
        do ix = 1, ntot_int_1d
          call s_shape_elenents_inf_aw_1d(isf, xk,                      &
     &        xi_nega, xi_posi, xi_sqre, xi_inf,  dxi_inf, xi(ix))
!
          call shape_function_an_1d_1(an_infty(1,isf,ix),               &
     &        xi_nega, xi_posi)
          call shape_function_dnxi_1d_1(dnxi_infty(1,isf,ix), dxi_inf)
        end do
      end do
!
      end subroutine s_cal_shape_func_inf_1d_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_func_inf_1d_quad(ntot_int_1d, xk,          &
     &          an_infty, dnxi_infty, xi)
!
      use shape_func_1d_quad
!
      integer (kind=kint), intent(in) :: ntot_int_1d
      real (kind=kreal), intent(in) :: xk
      real(kind=kreal), intent(in) :: xi(ntot_int_1d)
!
      real (kind=kreal), intent(inout)                                  &
     &      :: an_infty(num_quad_edge,nend_4_edge,ntot_int_1d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnxi_infty(num_quad_edge,nend_4_edge,ntot_int_1d)
!
      integer (kind=kint) :: isf, ix
!
!
      do isf = 1, nend_4_edge
        do ix = 1, ntot_int_1d
          call s_shape_elenents_inf_aw_1d(isf, xk,                      &
     &        xi_nega, xi_posi, xi_sqre, xi_inf,  dxi_inf, xi(ix))
!
          call shape_function_an_1d_20( an_infty(1,isf,ix),             &
     &        xi_inf, xi_nega, xi_posi, xi_sqre)
          call shape_function_dnxi_1d_20(dnxi_infty(1,isf,ix),          &
     &        xi_inf, dxi_inf )
        end do
      end do
!
      end subroutine s_cal_shape_func_inf_1d_quad
!
!-----------------------------------------------------------------------
!
      end module cal_shape_func_infty_1d
