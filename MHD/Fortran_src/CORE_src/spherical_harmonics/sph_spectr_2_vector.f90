!> @file  sph_spectr_2_vector.f90
!!      module sph_spectr_2_vector
!!
!!@author  H. Matsui
!!@date Programmed in 1997
!!@n     modified by H. Matsui in June, 2006
!
!> @brief Copy between field data and IO data
!!
!!@verbatim
!!      subroutine cvt_spectr_2_vector                                  &
!!     &         (nth, jmax, r, theta, g, Y, bp, bt, dbp, b_pole)
!!
!!      subroutine cvt(jmax, r, theta, g, Y, bp, bt, dbp, b_pole)
!!******************************************************
!!*
!!*    subroutin of  spector => vector for vector field
!!!*      of a point     expect for pole 
!!*                                          '96, 7,30
!!*
!!******************************************************
!!*
!!*
!!******************************************************
!!*
!!*      jmax_tri_sph  : the number of dimension of spector
!!*      vp(j) : spector of poloidal compornent
!!*     dvp(j) : differential of spector of poloidal compornent
!!*      vt(j) : spector of toroidal compornent
!!*      v_pole(m) : vector field ( 1:r , 2: theta , 3:phi)
!!*      b_pole(m) : vector field ( 1:r , 2: theta , 3:phi)
!!*       r1   : 1 / (radious)
!!*       sit  : sin(theta)
!!*     Y(j,m) : spherical harmonics
!!*   ( 0:original , 1:differentialof phi , 2:differential of thetra)
!!*     g3(j)  : l*(l+1)
!!*
!!******************************************************
!!*
!!*
!!******************************************************
!!*
!!*               l(l+1)
!!*     Vr     = -------- Vs(r)Y(l,m)
!!*                r^2
!!*
!!*               1    dVs   dY(l,m)          Vt        dY(l,m)
!!*     Vtheta = --- [----- --------- + ------------- ----------]
!!*               r    dr   d\theta      sin(\theta)     d\phi
!!*
!!*               1         1        dVs   dY(l,m)        dY(l,m)
!!*     Vphi   = --- [------------- ----- --------- - Vt ---------]
!!*               r    sin(\theta)   dr     d\phi         d\theta
!!*
!!*               
!!*     Th     = Th(r)Y(l,m)
!!*               
!!*
!!***********************************************************************
!!*
!!***********************************************************************
!!*
!!*       dY(l,m)     dP(l,m)
!!*      --------- = ---------  {cos(mphi) or sin(mphi)}
!!*       d\theta     d\theta
!!*
!!*       dY(l,m)
!!*      --------- =  m P(l,m)  {-sin(mphi or cos(mphi)}
!!*        d\phi
!!*
!!***********************************************************************
!!*
!!*
!!
!!      subroutine cvtp(nth, jmax, r, theta, g, Y, bp, bt, dbp, b_pole)
!!******************************************************
!!*    subroutin of  spector => vector for vector field
!!*      of a point    for pole 
!!*                                          '97,12,10
!!******************************************************
!!*
!!*
!!******************************************************
!!*      jmax_tri_sph  : the number of dimension of spector
!!*      yp(j) : spector of poloidal compornent
!!*     dyp(j) : differential of spector of poloidal compornent
!!*      yt(j) : spector of toroidal compornent
!!*      vp(m) : vector field ( 1:r , 2: theta , 3:phi)
!!*       r1   : 1 / (radious)
!!*       cst  : cos(theta)
!!*     Y(j,m) : spherical harmonics
!!*   ( 0:original , 1:differentialof phi , 2:differential of thetra)
!!*     g3(j)  : l*(l+1)
!!******************************************************
!!*
!!@endverbatim
!
      module sph_spectr_2_vector
!
      use m_precision
!
      implicit none
!
      private ::  cvt, cvtp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_spectr_2_vector                                    &
     &         (nth, jmax, r, theta, g, Y, bp, bt, dbp, b_pole)
!
      integer(kind = kint), intent(in) :: nth, jmax
      real(kind = kreal), intent(in) :: r, theta
      real(kind = kreal), intent(in):: Y(0:jmax,0:3)
      real(kind = kreal), intent(in):: g(0:jmax,17)
!
      real(kind = kreal), intent(inout) :: bp(0:jmax),  bt(0:jmax)
      real(kind = kreal), intent(inout) :: dbp(0:jmax)
!
      real(kind = kreal), intent(inout) :: b_pole(3)
!
!
      if ( r .le. 0.0d0 ) return
      if ( sin(theta) .eq. 0.0d0 ) then
        call cvtp(nth, jmax, r, theta, g, Y, bp, bt, dbp, b_pole)
      else 
        call cvt(jmax, r, theta, g, Y, bp, bt, dbp, b_pole)
      end if
!
      end subroutine cvt_spectr_2_vector
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cvt(jmax, r, theta, g, Y, bp, bt, dbp, b_pole)
!*
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) ::  r, theta
      real(kind = kreal), intent(in):: Y(0:jmax,0:3)
      real(kind = kreal), intent(in):: g(0:jmax,17)
!
      real(kind = kreal), intent(inout) :: bp(0:jmax),  bt(0:jmax)
      real(kind = kreal), intent(inout) :: dbp(0:jmax)
!
      real(kind = kreal), intent(inout) :: b_pole(3)
!
      integer(kind = kint) :: nm, j
!*
!*
      do 10 nm = 1 ,3
        b_pole(nm) = 0.0
  10  continue
!*
      do 20 j = 1 ,jmax
        b_pole(1) = b_pole(1) + g(j,3) * bp(j) * Y(j,0) / r**2
!*
        b_pole(2) = b_pole(2) + ( dbp(j) * Y(j,2)                       &
     &         + bt(j) * Y(j,1) / sin(theta) ) / r
!*
        b_pole(3) = b_pole(3) + ( dbp(j) * Y(j,1) / sin(theta)          &
     &         - bt(j) * Y(j,2) ) / r
!*
  20  continue
!*
      return
      end subroutine cvt
!
!  ---------------------------------------------------------------------
!
      subroutine cvtp(nth, jmax, r, theta, g, Y, bp, bt, dbp, b_pole)
!
      integer(kind = kint), intent(in) :: nth, jmax
      real(kind = kreal), intent(in) ::  r, theta
      real(kind = kreal), intent(in):: Y(0:jmax,0:3)
      real(kind = kreal), intent(in):: g(0:jmax,17)
!
      real(kind = kreal), intent(inout) :: bp(0:jmax),  bt(0:jmax)
      real(kind = kreal), intent(inout) :: dbp(0:jmax)
!
      real(kind = kreal), intent(inout) :: b_pole(3)
!
      integer(kind = kint) :: nm, l, j0, j9, j1
!*
!*
      do 10 nm = 1 ,3
        b_pole(nm) = 0.0
  10  continue
!*
      do 20 l = 1 ,nth
        j0 = l*(l+1)
        j9 = l*(l+1) - 1
        j1 = l*(l+1) + 1
!*
        b_pole(1) = b_pole(1) +  g(j0,3) * bp(j0) * Y(j0,0) / r**2
!*
        b_pole(2) = b_pole(2) +  ( dbp(j9) * Y(j9,2)                    &
     &   + dbp(j1) * Y(j1,2) + cos(theta) * ( bt(j9) * Y(j9,3)          &
     &         + bt(j1) * Y(j1,3) ) ) / r
!*
        b_pole(3) = b_pole(3) + ( cos(theta) * ( dbp(j9) * Y(j9,3)      &
     &         + dbp(j1) * Y(j1,3) )  - bt(j9) * Y(j9,2)                &
     &         - bt(j1) * Y(j1,2) ) / r
!*
  20  continue
!*
      return
      end subroutine cvtp
!
!  ---------------------------------------------------------------------
!
      end module sph_spectr_2_vector
