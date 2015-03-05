!
!      module m_work_4_BiCGSTAB
!
      module m_work_4_BiCGSTAB
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: ALPHA, OMEGA
      real(kind=kreal) :: BNRM2,  DNRM2,  C1,  C2, RHO, RHO1
      real(kind=kreal) :: BNRM20, DNRM20, C20, RHO0
      real(kind=kreal) :: C0(2), CG(2)
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
      end module m_work_4_BiCGSTAB
