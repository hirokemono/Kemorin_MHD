!m_ranbic_dodecahedron.f90
!      module m_ranbic_dodecahedron
!
!     Written by H. Matsui on Dec., 2011
!
      module m_ranbic_dodecahedron
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind = kreal), parameter :: xyz_romb_dodeca(3,15)            &
     &     = reshape( (/  zero,  zero,  zero,                           &
     &                    zero,  zero,   one,     zero,  zero,  -one,   &
     &                     one,  zero,  zero,     zero,   one,  zero,   &
     &                    -one,  zero,  zero,     zero,  -one,  zero,   &
     &                   -half, -half,  half,     half, -half,  half,   &
     &                    half,  half,  half,    -half,  half,  half,   &
     &                   -half, -half, -half,     half, -half, -half,   &
     &                    half,  half, -half,    -half,  half, -half/), &
     &       shape=(/3,15/))
!
      real(kind = kreal), parameter :: refx = 1.73205080756888 / three
      real(kind = kreal), parameter :: xyz_sph_romb_dodeca(3,15)        &
     &     = reshape( (/  zero,  zero,  zero,                           &
     &                    zero,  zero,   one,     zero,  zero,  -one,   &
     &                     one,  zero,  zero,     zero,   one,  zero,   &
     &                    -one,  zero,  zero,     zero,  -one,  zero,   &
     &                   -refx, -refx,  refx,     refx, -refx,  refx,   &
     &                    refx,  refx,  refx,    -refx,  refx,  refx,   &
     &                   -refx, -refx, -refx,     refx, -refx, -refx,   &
     &                    refx,  refx, -refx,    -refx,  refx, -refx/), &
     &       shape=(/3,15/))
!
      integer (kind=kint), parameter :: ie_romb_dodeca(8,4)             &
     &     = reshape( (/  1,   8,   2,   10,   15,   6,   11,   5,      &
     &                    1,  10,   2,    8,   13,   4,    9,   7,      &
     &                    1,  13,   3,   15,   10,   4,   14,   5,      &
     &                    1,  15,   3,   13,    8,   6,   12,   7/),    &
     &       shape=(/8,4/))
!
      end module m_ranbic_dodecahedron
