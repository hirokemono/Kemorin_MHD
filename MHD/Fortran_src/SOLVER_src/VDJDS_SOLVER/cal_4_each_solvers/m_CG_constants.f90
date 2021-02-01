!
!      module m_CG_constants
!
!     Written by Kemorin
!
      module m_CG_constants
!
      use m_precision
!
      implicit none
!
      integer(kind=kint), parameter :: P = 3
      integer(kind=kint), parameter :: Q = 2
      integer(kind=kint), parameter :: R = 1
      integer(kind=kint), parameter :: Z = 2
      integer(kind=kint), parameter :: ZQ= 4
!
      integer(kind=kint), parameter :: nWK_CG =  4
      integer(kind=kint), parameter :: iWK =     5
      integer(kind=kint) :: ntotWK_CG = nWK_CG + 3
!
      end module m_CG_constants
