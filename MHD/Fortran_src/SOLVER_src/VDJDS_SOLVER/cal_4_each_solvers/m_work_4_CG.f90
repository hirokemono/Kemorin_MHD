!
!      module m_work_4_CG
!
!     Written by Kemorin
!
      module m_work_4_CG
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: BNRM2,  DNRM2,  C1,  RHO, RHO1, ALPHA
      real(kind=kreal) :: BNRM20, DNRM20, C10, RHO0
!
      integer(kind=kint), parameter :: P = 3
      integer(kind=kint), parameter :: Q = 2
      integer(kind=kint), parameter :: R = 1
      integer(kind=kint), parameter :: Z = 2
      integer(kind=kint), parameter :: ZQ= 4
!
      integer(kind=kint), parameter :: nWK_CG =    4
      integer(kind=kint), parameter :: iWK =    5
      integer(kind=kint) :: ntotWK_CG = nWK_CG + 3
!
      end module m_work_4_CG
