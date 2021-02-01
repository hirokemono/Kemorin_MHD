!
!      module m_BiCGSTAB_constants
!
      module m_BiCGSTAB_constants
!
      use m_precision
!
      implicit none
!
!
      integer(kind=kint), parameter :: R = 1
      integer(kind=kint), parameter :: RT= 2
      integer(kind=kint), parameter :: P = 3
      integer(kind=kint), parameter :: PT= 4
      integer(kind=kint), parameter :: S = 5
      integer(kind=kint), parameter :: ST= 6
      integer(kind=kint), parameter :: T = 7
      integer(kind=kint), parameter :: V = 8
      integer(kind=kint), parameter :: ZQ= 9
!
      integer(kind=kint), parameter :: nWK_BiCGSTAB = 10
      integer(kind=kint), parameter :: iWK =          11
      integer(kind=kint) :: ntotWK_BiCGSTAB = nWK_BiCGSTAB + 3
!
      end module m_BiCGSTAB_constants
