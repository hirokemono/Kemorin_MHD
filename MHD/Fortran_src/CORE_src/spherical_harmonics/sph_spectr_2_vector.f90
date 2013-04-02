!sph_spectr_2_vector.f90
!     module sph_spectr_2_vector
!
!     Written by H. Matsui in 1997
!     modified by H. Matsui on June, 2006
!
!      subroutine cvt(r)
!******************************************************
!*
!*    subroutin of  spector => vector for vector field
!*      of a point     expect for pole 
!*                                          '96, 7,30
!*
!******************************************************
!*
!*
!******************************************************
!*
!*      jmax_tri_sph  : the number of dimension of spector
!*      vp(j) : spector of poloidal compornent
!*     dvp(j) : differential of spector of poloidal compornent
!*      vt(j) : spector of toroidal compornent
!*      v_pole(m) : vector field ( 1:r , 2: theta , 3:phi)
!*      b_pole(m) : vector field ( 1:r , 2: theta , 3:phi)
!*       r1   : 1 / (radious)
!*       sit  : sin(theta)
!*     s(j,m) : spherical hermonics
!*   ( 0:original , 1:differentialof phi , 2:differential of thetra)
!*     g3(j)  : l*(l+1)
!*
!******************************************************
!*
!*
!******************************************************
!*
!*               l(l+1)
!*     Vr     = -------- Vs(r)Y(l,m)
!*                r^2
!*
!*               1    dVs   dY(l,m)          Vt        dY(l,m)
!*     Vtheta = --- [----- --------- + ------------- ----------]
!*               r    dr   d\theta      sin(\theta)     d\phi
!*
!*               1         1        dVs   dY(l,m)        dY(l,m)
!*     Vphi   = --- [------------- ----- --------- - Vt ---------]
!*               r    sin(\theta)   dr     d\phi         d\theta
!*
!*               
!*     Th     = Th(r)Y(l,m)
!*               
!*
!***********************************************************************
!*
!***********************************************************************
!*
!*       dY(l,m)     dP(l,m)
!*      --------- = ---------  {cos(mphi) or sin(mphi)}
!*       d\theta     d\theta
!*
!*       dY(l,m)
!*      --------- =  m P(l,m)  {-sin(mphi or cos(mphi)}
!*        d\phi
!*
!***********************************************************************
!*
!*
!
!      subroutine cvtp(r)
!******************************************************
!*    subroutin of  spector => vector for vector field
!*      of a point    for pole 
!*                                          '97,12,10
!******************************************************
!*
!*
!******************************************************
!*      jmax_tri_sph  : the number of dimension of spector
!*      yp(j) : spector of poloidal compornent
!*     dyp(j) : differential of spector of poloidal compornent
!*      yt(j) : spector of toroidal compornent
!*      vp(m) : vector field ( 1:r , 2: theta , 3:phi)
!*       r1   : 1 / (radious)
!*       cst  : cos(theta)
!*     s(j,m) : spherical hermonics
!*   ( 0:original , 1:differentialof phi , 2:differential of thetra)
!*     g3(j)  : l*(l+1)
!******************************************************
!*
      module sph_spectr_2_vector
!
      use m_precision
!
      use m_schmidt_polynomial
      use m_spherical_harmonics
!*
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cvt(r)
!*
      real(kind = kreal), intent(in) ::  r
      integer(kind = kint) :: nm, j
!*
!*
      do 10 nm = 1 ,3
        v_pole(nm) = 0.0
        b_pole(nm) = 0.0
  10  continue
!*
      do 20 j = 1 ,jmax_tri_sph
!*
        v_pole(1) = v_pole(1) + g(j,3) * vp(j) * s(j,0) / r**2
!*
        v_pole(2) = v_pole(2) + ( dvp(j) * s(j,2)                       &
     &         + vt(j) * s(j,1) / sin(dth) ) / r
!*
        v_pole(3) = v_pole(3) + ( dvp(j) * s(j,1) / sin(dth)            &
     &         - vt(j) * s(j,2) ) / r
!*
        b_pole(1) = b_pole(1) + g(j,3) * bp(j) * s(j,0) / r**2
!*
        b_pole(2) = b_pole(2) + ( dbp(j) * s(j,2)                       &
     &         + bt(j) * s(j,1) / sin(dth) ) / r
!*
        b_pole(3) = b_pole(3) + ( dbp(j) * s(j,1) / sin(dth)            &
     &         - bt(j) * s(j,2) ) / r
!*
  20  continue
!*
      return
      end subroutine cvt
!
!  ---------------------------------------------------------------------
!
      subroutine cvtp(r)
!
      real(kind = kreal), intent(in) ::  r
      integer(kind = kint) :: nm, l, j0, j9, j1
!*
!*
      do 10 nm = 1 ,3
        v_pole(nm) = 0.0
        b_pole(nm) = 0.0
  10  continue
!*
      do 20 l = 1 ,nth
!*
        j0 = l*(l+1)
        j9 = l*(l+1) - 1
        j1 = l*(l+1) + 1
!*
        v_pole(1) = v_pole(1) +  g(j0,3) * vp(j0) * s(j0,0) / r**2
!*
        v_pole(2) = v_pole(2) +  ( dvp(j9) * s(j9,2)                   &
     &   + dvp(j1) * s(j1,2) + cos(dth) * ( vt(j9) * s(j9,3)           &
     &         + vt(j1) * s(j1,3) ) ) / r
!*
        v_pole(3) = v_pole(3) + ( cos(dth) * ( dvp(j9) * s(j9,3)       &
     &         + dvp(j1) * s(j1,3) )  - vt(j9) * s(j9,2)               &
     &         - vt(j1) * s(j1,2) ) / r
!*
        b_pole(1) = b_pole(1) +  g(j0,3) * bp(j0) * s(j0,0) / r**2
!*
        b_pole(2) = b_pole(2) +  ( dbp(j9) * s(j9,2)                   &
     &   + dbp(j1) * s(j1,2) + cos(dth) * ( bt(j9) * s(j9,3)           &
     &         + bt(j1) * s(j1,3) ) ) / r
!*
        b_pole(3) = b_pole(3) + ( cos(dth) * ( dbp(j9) * s(j9,3)       &
     &         + dbp(j1) * s(j1,3) )  - bt(j9) * s(j9,2)               &
     &         - bt(j1) * s(j1,2) ) / r
!*
  20  continue
!*
      return
      end subroutine cvtp
!
!  ---------------------------------------------------------------------
!
      end module sph_spectr_2_vector
