!
!      module cal_shape_func_infty_2d
!
!        modified by H. Matsui on Aug., 2006
!
!      subroutine s_cal_shape_func_inf_2d_linear(ntot_int_2d, xk,       &
!     &          an_infty, dnxi_infty, dnei_infty, xi, ei)
!      subroutine s_cal_shape_func_inf_2d_quad(ntot_int_2d, xk,         &
!     &          an_infty, dnxi_infty, dnei_infty, xi, ei)
!      subroutine s_cal_shape_func_inf_2d_lag(ntot_int_2d, xk,          &
!     &          an_infty, dnxi_infty, dnei_infty, xi, ei)
!
      module cal_shape_func_infty_2d
!
      use m_precision
!
      use m_geometry_constants
      use m_geometry_parameter
      use set_shape_elements_infty_sf
!
      implicit none
!
      real (kind=kreal) :: xi_nega, ei_nega
      real (kind=kreal) :: xi_posi, ei_posi
      real (kind=kreal) :: xi_sqre, ei_sqre
      real (kind=kreal) :: xi_inf,  ei_inf
      real (kind=kreal) :: dxi_inf, dei_inf
!
      private :: xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre
      private :: xi_inf,  ei_inf, dxi_inf, dei_inf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_func_inf_2d_linear(ntot_int_2d, xk,        &
     &          an_infty, dnxi_infty, dnei_infty, xi, ei)
!
      use shape_func_2d_linear
!
      integer (kind=kint), intent(in) :: ntot_int_2d
      real (kind=kreal), intent(in) :: xk
      real(kind=kreal), intent(in) :: xi(ntot_int_2d)
      real(kind=kreal), intent(in) :: ei(ntot_int_2d)
!
      real (kind=kreal), intent(inout)                                  &
     &      :: an_infty(num_linear_sf,nedge_4_surf,ntot_int_2d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnxi_infty(num_linear_sf,nedge_4_surf,ntot_int_2d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnei_infty(num_linear_sf,nedge_4_surf,ntot_int_2d)
!
      integer(kind = kint) :: isf, ix
! 
!
      do isf = 1, nedge_4_surf
        do ix = 1, ntot_int_2d
          call s_shape_elenents_inf_aw_2d(isf, xk,                      &
     &        xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,     &
     &        xi_inf,  ei_inf,  dxi_inf, dei_inf, xi(ix), ei(ix) )
!
          call shape_function_an_sf_1(an_infty(1,isf,ix),               &
     &        xi_nega, ei_nega, xi_posi, ei_posi)
          call shape_function_dnxi_sf_1(dnxi_infty(1,isf,ix),           &
     &        ei_nega, ei_posi, dxi_inf)
          call shape_function_dnei_sf_1(dnei_infty(1,isf,ix),           &
     &        xi_nega, xi_posi, dei_inf)
        end do
      end do
!
      end subroutine s_cal_shape_func_inf_2d_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_func_inf_2d_quad(ntot_int_2d, xk,          &
     &          an_infty, dnxi_infty, dnei_infty, xi, ei)
!
      use shape_func_2d_quad
!
      integer (kind=kint), intent(in) :: ntot_int_2d
      real (kind=kreal), intent(in) :: xk
      real(kind=kreal), intent(in) :: xi(ntot_int_2d)
      real(kind=kreal), intent(in) :: ei(ntot_int_2d)
!
      real (kind=kreal), intent(inout)                                  &
     &      :: an_infty(num_quad_sf,nedge_4_surf,ntot_int_2d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnxi_infty(num_quad_sf,nedge_4_surf,ntot_int_2d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnei_infty(num_quad_sf,nedge_4_surf,ntot_int_2d)
!
      integer (kind=kint) :: isf, ix
!
!
      do isf = 1, nedge_4_surf
        do ix = 1, ntot_int_2d
          call s_shape_elenents_inf_aw_2d(isf, xk,                      &
     &        xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,     &
     &        xi_inf,  ei_inf,  dxi_inf, dei_inf, xi(ix), ei(ix) )
!
          call shape_function_an_sf20( an_infty(1,isf,ix),              &
     &        xi_inf, ei_inf, xi_nega, ei_nega, xi_posi, ei_posi,       &
     &        xi_sqre, ei_sqre)
          call shape_function_dnxi_sf20( dnxi_infty(1,isf,ix),          &
     &        xi_inf, ei_inf, xi_nega, ei_nega, xi_posi, ei_posi,       &
     &        xi_sqre, ei_sqre, dxi_inf)
          call shape_function_dnei_sf20( dnei_infty(1,isf,ix),          &
     &        xi_inf, ei_inf, xi_nega, ei_nega, xi_posi, ei_posi,       &
     &        xi_sqre, ei_sqre, dei_inf)
        end do
      end do
!
      end subroutine s_cal_shape_func_inf_2d_quad
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_func_inf_2d_lag(ntot_int_2d, xk,           &
     &          an_infty, dnxi_infty, dnei_infty, xi, ei)
!
      use shape_func_2d_lag
!
      integer (kind=kint), intent(in) :: ntot_int_2d
      real (kind=kreal), intent(in) :: xk
      real (kind=kreal), intent(in) :: xi(ntot_int_2d)
      real (kind=kreal), intent(in) :: ei(ntot_int_2d)
!
      real (kind=kreal), intent(inout)                                  &
     &      :: an_infty(num_lag_sf,nedge_4_surf,ntot_int_2d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnxi_infty(num_lag_sf,nedge_4_surf,ntot_int_2d)
      real (kind=kreal), intent(inout)                                  &
     &      :: dnei_infty(num_lag_sf,nedge_4_surf,ntot_int_2d)
!
        integer (kind=kint) :: isf, ix
!
!
      do isf = 1, nedge_4_surf
        do ix = 1, ntot_int_2d
          call s_shape_elenents_inf_aw_2d(isf, xk,                      &
     &        xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,     &
     &        xi_inf,  ei_inf,  dxi_inf, dei_inf, xi(ix), ei(ix) )
!
          call shape_function_an_sf27( an_infty(1,isf,ix),              &
     &        xi_inf, ei_inf, xi_nega, ei_nega, xi_posi, ei_posi,       &
     &        xi_sqre, ei_sqre)
          call shape_function_dnxi_sf27( dnxi_infty(1,isf,ix),          &
     &        xi_inf, ei_inf, xi_nega, ei_nega, xi_posi, ei_posi,       &
     &        xi_sqre, ei_sqre, dxi_inf)
          call shape_function_dnei_sf27( dnei_infty(1,isf,ix),          &
     &        xi_inf, ei_inf, xi_nega, ei_nega, xi_posi, ei_posi,       &
     &        xi_sqre, ei_sqre, dei_inf)
        end do
      end do
!
      end subroutine s_cal_shape_func_inf_2d_lag
!
!-----------------------------------------------------------------------
!
      end module cal_shape_func_infty_2d
