!
!      module shape_func_1d_infty
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine shape_function_an_1d_infty(an_infty, xi)
!      subroutine shape_function_dnxi_1d_infty(dnxi_infty, xi)
!
      module shape_func_1d_infty
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
      subroutine shape_function_an_1d_infty(an_infty, xi)
!
      use m_constants
!
      real (kind=kreal), dimension(2,2) :: an_infty
      real (kind=kreal) :: xi
!
      real (kind=kreal) :: xi_nega
      real (kind=kreal) :: xi_posi
!
!
      xi_nega = one - xi
      xi_posi = one + xi
!
!
      an_infty(1,1) = two*xi/xi_posi
      an_infty(2,1) = ( one - two*xi/xi_posi )
!
      an_infty(1,2) = ( one + two*xi/xi_nega )
      an_infty(2,2) = - two*xi/xi_nega
!
      end subroutine shape_function_an_1d_infty
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_1d_infty(dnxi_infty, xi)
!
      use m_constants
!
      real (kind=kreal), dimension(2,2) :: dnxi_infty
      real (kind=kreal) :: xi
!
      real (kind=kreal) :: xi_nega
      real (kind=kreal) :: xi_posi
!
!
      xi_nega = one - xi
      xi_posi = one + xi
!
      dnxi_infty(1,1) =  two / (xi_posi**2)
      dnxi_infty(2,1) = -two / (xi_posi**2)
!
      dnxi_infty(1,2) =  two / (xi_nega**2)
      dnxi_infty(2,2) = -two / (xi_nega**2)
!
      end subroutine shape_function_dnxi_1d_infty
!
!-----------------------------------------------------------------------
!
      end module shape_func_1d_infty
