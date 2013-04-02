!legendre.f90
!      module legendre
!
!        Written by H. Matsui in 1995
!        Modified by H. Matsui on June, 2006
!>
!!    @brief module for Legendre polynomials
!!
!!@n    subroutine dladendre(nth, x, dplm, df)
!!@n*************************************************************
!!@n     lead legendre and adjoint Legendle Polynomial
!!@n*
!!@n*      dplm(m,l) : adjoint Legendre Polynomial P_l^m (x)
!!@n*         x        :  input value x ( -1 =< x =<1 )
!!@n*        df(m,l)  :   work area
!!@n*
!!@n*************************************************************
!!@n*
!!@n      subroutine schmidt_normalization(nth, dplm, dc, p, dp)
!!@n*************************************************************
!!@n*     lead Schmidt quasi-normalization
!!@n*
!!@n*      p(m,l)  : Schmidt Polynomial
!!@n*      dp(m,l) : diffrence of Schmidt Polynomial dp/dtheta
!!@n*      dplm(m,l)  : adjoint Legendre Polynomial P_l^m (x)
!!@n*      dc(m,l)  :   work area
!!@n*
!!@n*************************************************************
!!
!!@n @param nth       Truncation level for the polynomial
!!@n @param x         Input value  ( -1 =< x =<1 )
!!@n @param dplm(m,l) adjoint Legendre Polynomial P_l^m (x)
!!@n @param p(m,l)    Schmidt Polynomial
!!@n @param dp(m,l)   diffrence of Schmidt Polynomial  dp/dtheta
!!@n @param df(m,l)   work area
!!@n @param dc(m,l)   work area
!
      module legendre
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dladendre(nth, x, dplm, df)
!*
      integer(kind = kint), intent(in) :: nth
      real(kind = kreal), intent(in) :: x
!
      real(kind = kreal), intent(inout) :: dplm(0:nth+2,0:nth+2)
      real(kind = kreal), intent(inout) :: df(0:nth+2,0:nth+2)
!
      integer(kind = kint) :: l, m, mm, n
!
!* +++++++  Legendre Polynomial  ++++++++++++
!*
      dplm(0,0) = 1.0d0
      dplm(0,1) = x
!*
      do 10 l = 2 ,nth+1
!*
        dplm(0,l) = x * dplm(0,l-1) * dble(2*l-1)/dble(l)               &
     &             - dplm(0,l-2) * dble(l-1)/dble(l)
!*
  10  continue
!*
!* +++++++  adjoint Legendre Polynomial  ++++++++++++
!*
      do 20 m = 1 ,nth+1
!*
        df(m,m) = 1.0
        df(m,m+1) = x
        do 30 n = 1 ,2*m-1 ,2
          df(m,m) = df(m,m)*dble(n)
          df(m,m+1) = df(m,m+1)*dble(n)
  30    continue
        df(m,m+1) = df(m,m+1)*dble(2*m+1)
!*
!*
        if ( m .lt. nth-1 ) then
          do 40 mm = m+2 ,nth
            df(m,mm) = x * df(m,mm-1) * dble(2*mm-1)/dble(mm-m)         &
     &                - df(m,mm-2) * dble(mm+m-1)/dble(mm-m)
  40      continue
        endif
!*
        do 50 l = m ,nth
!            write(*,*) 'l,m,df', l,m,df(m,l)
          dplm(m,l) = ( abs(1-x**2) )**(dble(m)/2) * df(m,l)
  50    continue
!*
  20  continue
!*
!*
      return
      end subroutine dladendre
!
!  ---------------------------------------------------------------------
!
      subroutine schmidt_normalization(nth, dplm, dc, p, dp)
!*
      use factorials
!
      integer(kind = kint), intent(in) :: nth
      real(kind = kreal), intent(in) :: dplm(0:nth+2,0:nth+2)
!
      real(kind = kreal), intent(inout) :: dc(0:nth,0:nth+2)
      real(kind = kreal), intent(inout) :: p(0:nth,0:nth)
      real(kind = kreal), intent(inout) :: dp(0:nth,0:nth)
!
      integer(kind = kint) :: l, m
!
!*  +++++++   set normalized cpnstant ++++++++++++
!*
      do 10 l = 0 ,nth
        dc(0,l) = 1.0
        do 11 m = 1 ,l
          dc(m,l) = ( 2.0d0 / factorial(l-m,l+m,1) )**0.5d0
  11    continue
  10  continue
!*
!*   ++++++++++  lead difference of the Polynomial  ++++++
!*
        dp(0,0) = 0.0d0
        do 20 l = 1 ,nth
          do 21 m = 1 ,l-1
!*
            dp(m,l) = ( dble(l+m) * dble(l-m+1) * dplm(m-1,l)           &
     &                 - dplm(m+1,l) ) / 2.0d0
!*
  21      continue
          dp(0,l) = - dplm(1,l)
          dp(l,l) = dble(l) * dplm(l-1,l)
!*
  20    continue
!*
!*   ++++++++++ normalize  +++++++++++
!*
        do 31 l = 0 ,nth
          do 32 m = 0 ,l
!*
            p(m,l) =  dplm(m,l) * dc(m,l)
            dp(m,l) = dp(m,l) * dc(m,l)
!            write(*,*) 'l,m',l,m, p(m,l),dp(m,l)
!*
  32      continue
  31    continue
!*
      return
      end subroutine schmidt_normalization
!
!  ---------------------------------------------------------------------
!
      end module legendre
