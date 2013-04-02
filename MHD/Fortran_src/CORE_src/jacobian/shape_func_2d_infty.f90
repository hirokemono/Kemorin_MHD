!
!      module shape_func_2d_infty
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine shape_function_an_2d_infty(an_infty, xi, ei)
!      subroutine shape_function_dnxi_2d_infty(dnxi_infty, xi, ei)
!      subroutine shape_function_dnei_2d_infty(dnei_infty, xi, ei)
!
      module shape_func_2d_infty
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
      subroutine shape_function_an_2d_infty(an_infty, xi, ei)
!
      use m_constants
!
      real (kind=kreal), dimension(4,4) :: an_infty
      real (kind=kreal) :: xi, ei
!
      real (kind=kreal) :: xi_nega, ei_nega
      real (kind=kreal) :: xi_posi, ei_posi
!
!
      xi_nega = one - xi
      xi_posi = one + xi
!
      ei_nega = one - ei
      ei_posi = one + ei
!
!
      an_infty(1,1) = ( one - two*xi/xi_posi ) * ei_nega * half
      an_infty(2,1) = two*xi/xi_posi *           ei_nega * half
      an_infty(3,1) = two*xi/xi_posi *           ei_posi * half
      an_infty(4,1) = ( one - two*xi/xi_posi ) * ei_posi * half
!
      an_infty(1,2) = - two*xi/xi_nega *         ei_nega * half
      an_infty(2,2) = ( one + two*xi/xi_nega ) * ei_nega * half
      an_infty(3,2) = ( one + two*xi/xi_nega ) * ei_posi * half
      an_infty(4,2) = - two*xi/xi_nega *         ei_posi * half
!
      an_infty(1,3) = xi_nega * ( one - two*ei/ei_posi ) * half
      an_infty(2,3) = xi_posi * ( one - two*ei/ei_posi ) * half
      an_infty(3,3) = xi_posi * two*ei/ei_posi           * half
      an_infty(4,3) = xi_nega * two*ei/ei_posi           * half
!
      an_infty(1,4) = - xi_nega * two*ei/ei_nega         * half
      an_infty(2,4) = - xi_posi * two*ei/ei_nega         * half
      an_infty(3,4) = xi_posi * ( one + two*ei/ei_nega ) * half
      an_infty(4,4) = xi_nega * ( one + two*ei/ei_nega ) * half
!
      end subroutine shape_function_an_2d_infty
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_2d_infty(dnxi_infty, xi, ei)
!
      use m_constants
!
      real (kind=kreal), dimension(4,4) :: dnxi_infty
      real (kind=kreal) :: xi, ei
!
      real (kind=kreal) :: xi_nega, ei_nega
      real (kind=kreal) :: xi_posi, ei_posi
!
!
      xi_nega = one - xi
      xi_posi = one + xi
!
      ei_nega = one - ei
      ei_posi = one + ei
!
      dnxi_infty(1,1) = -ei_nega / (xi_posi**2)
      dnxi_infty(2,1) =  ei_nega / (xi_posi**2)
      dnxi_infty(3,1) =  ei_posi / (xi_posi**2)
      dnxi_infty(4,1) = -ei_posi / (xi_posi**2)
!
      dnxi_infty(1,2) = -ei_nega / (xi_nega**2)
      dnxi_infty(2,2) =  ei_nega / (xi_nega**2)
      dnxi_infty(3,2) =  ei_posi / (xi_nega**2)
      dnxi_infty(4,2) = -ei_posi / (xi_nega**2)
!
      dnxi_infty(1,3) = -half * ( one - two*ei/ei_posi )
      dnxi_infty(2,3) =  half * ( one - two*ei/ei_posi )
      dnxi_infty(3,3) =  half * two*ei/ei_posi
      dnxi_infty(4,3) = -half * two*ei/ei_posi
!
      dnxi_infty(1,4) = -half * (-two*ei/ei_nega )
      dnxi_infty(2,4) =  half * (-two*ei/ei_nega )
      dnxi_infty(3,4) =  half * ( one + two*ei/ei_nega )
      dnxi_infty(4,4) = -half * ( one + two*ei/ei_nega )
!
      end subroutine shape_function_dnxi_2d_infty
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnei_2d_infty(dnei_infty, xi, ei)
!
      use m_constants
!
      real (kind=kreal), dimension(4,4) :: dnei_infty
      real (kind=kreal) :: xi, ei
!
      real (kind=kreal) :: xi_nega, ei_nega
      real (kind=kreal) :: xi_posi, ei_posi
!
      xi_nega = one - xi
      xi_posi = one + xi
!
      ei_nega = one - ei
      ei_posi = one + ei
!
      dnei_infty(1,1) = -half * ( one - two*xi/xi_posi )
      dnei_infty(2,1) = -half * two*xi/xi_posi
      dnei_infty(3,1) =  half * two*xi/xi_posi
      dnei_infty(4,1) =  half * ( one - two*xi/xi_posi )
!
      dnei_infty(1,2) = -half * (- two*xi/xi_nega)
      dnei_infty(2,2) = -half * ( one + two*xi/xi_nega )
      dnei_infty(3,2) =  half * ( one + two*xi/xi_nega )
      dnei_infty(4,2) =  half * (- two*xi/xi_nega)
!
      dnei_infty(1,3) = -xi_nega / (ei_posi**2)
      dnei_infty(2,3) = -xi_posi / (ei_posi**2)
      dnei_infty(3,3) =  xi_posi / (ei_posi**2)
      dnei_infty(4,3) =  xi_nega / (ei_posi**2)
!
      dnei_infty(1,4) = -xi_nega / (ei_nega**2)
      dnei_infty(2,4) = -xi_posi / (ei_nega**2)
      dnei_infty(3,4) =  xi_posi / (ei_nega**2)
      dnei_infty(4,4) =  xi_nega / (ei_nega**2)
!
      end subroutine shape_function_dnei_2d_infty
!
!-----------------------------------------------------------------------
!
      end module shape_func_2d_infty
