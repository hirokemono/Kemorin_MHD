!
!      module m_work_4_GPBiCG
!
!     Written by Kemorin on Dec., 2005
!
!      subroutine allocate_work_GPBiCG_33(NP, PEsmpTOT)
!      subroutine init_work_GPBiCG_33(NP, PEsmpTOT)
!      subroutine deallocate_work_GPBiCG_33
!
      module m_work_4_GPBiCG
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: ALPHA, BETA, QSI, ETA
      real(kind=kreal) :: BNRM2(1),  DNRM2(1),  RHO(1),  RHO1(1)
      real(kind=kreal) :: BNRM20(1), DNRM20(1), RHO0(1), RHO10(1)
      real(kind=kreal) :: COEF1(1)
      real(kind=kreal) :: COEF10(1)
      real(kind=kreal) :: C0(5), CG(5)
      real(kind=kreal) :: EQ(2)
!
      integer(kind=kint), parameter ::  R =  1
      integer(kind=kint), parameter :: RT =  2
      integer(kind=kint), parameter ::  T =  3
      integer(kind=kint), parameter :: TT =  4
      integer(kind=kint), parameter :: T0 =  5
      integer(kind=kint), parameter ::  P =  6
      integer(kind=kint), parameter :: PT =  7
      integer(kind=kint), parameter ::  U =  8
      integer(kind=kint), parameter :: W1 =  9
      integer(kind=kint), parameter ::  Y = 10
      integer(kind=kint), parameter ::  Z = 11
      integer(kind=kint), parameter :: WK = 12
      integer(kind=kint), parameter :: WZ = 13
      integer(kind=kint), parameter :: WT = 14
      integer(kind=kint), parameter :: RX = 15
      integer(kind=kint), parameter :: RY = 16
      integer(kind=kint), parameter :: RZ = 17
!
      integer(kind=kint), parameter :: nWK_GPBiCG = 17
      integer(kind=kint), parameter :: iWK = 18
      integer(kind=kint) :: ntotWK_GPBiCG = nWK_GPBiCG + 6
!
!  ---------------------------------------------------------------------
!
!      contains
!
!  ---------------------------------------------------------------------
!
      end module m_work_4_GPBiCG
