!
!      module m_work_4_GPBiCGnn
!
!     Written by Kemorin on Dec., 2005
!
!      subroutine verify_work_GPBiCG_nn(NP, NB, PEsmpTOT)
!      subroutine allocate_work_GPBiCG_nn(NP, NB, PEsmpTOT)
!      subroutine init_work_GPBiCG_nn(NP, NB, PEsmpTOT)
!      subroutine deallocate_work_GPBiCG_nn
!
      module m_work_4_GPBiCGnn
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: iflag_work_GPBiCGnn = 0
      real(kind=kreal), allocatable :: W(:,:)
!
      private :: iflag_work_GPBiCGnn
!
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: ALPHA, BETA, QSI, ETA
      real(kind=kreal) :: BNRM2,  DNRM2,  COEF1,  RHO,  RHO1
      real(kind=kreal) :: BNRM20, DNRM20, COEF10, RHO0, RHO10
      real(kind=kreal), dimension(5) :: C0, CG
      real(kind=kreal), dimension(2) :: EQ
      real(kind=kreal), allocatable :: DNRMsmp(:)
      real(kind=kreal), allocatable :: COEFsmp(:)
      real(kind=kreal), allocatable :: SP1smp(:)
      real(kind=kreal), allocatable :: C0_smp(:,:)
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_GPBiCG_nn(NP, NB, PEsmpTOT)
!
      integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
!
      if (iflag_work_GPBiCGnn.eq.0) then
        call allocate_work_GPBiCG_nn(NP, NB, PEsmpTOT)
      else if (iflag_work_GPBiCGnn .lt. (17*NB*NP)) then
        call deallocate_work_GPBiCG_nn
        call allocate_work_GPBiCG_nn(NP, NB, PEsmpTOT)
      end if
!
      end subroutine verify_work_GPBiCG_nn
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_GPBiCG_nn(NP, NB, PEsmpTOT)
!
      integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
!
      allocate (W(NB*NP,17))
      allocate (DNRMsmp(PEsmpTOT))
      allocate (COEFsmp(PEsmpTOT))
      allocate (SP1smp(PEsmpTOT))
      allocate (C0_smp(PEsmpTOT,5))
      iflag_work_GPBiCGnn = 17*NB*NP
!
      end subroutine allocate_work_GPBiCG_nn
!
!  ---------------------------------------------------------------------
!
      subroutine init_work_GPBiCG_nn(NP, NB, PEsmpTOT)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint) :: i
!
!cdir parallel do
!$omp parallel do
!poption indep (W)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W)
      do i= 1, NB*NP
        W(i, 1)= 0.d0
        W(i, 2)= 0.d0
        W(i, 3)= 0.d0
        W(i, 4)= 0.d0
        W(i, 5)= 0.d0
        W(i, 6)= 0.d0
        W(i, 7)= 0.d0
        W(i, 8)= 0.d0
        W(i, 9)= 0.d0
        W(i,10)= 0.d0
        W(i,11)= 0.d0
        W(i,12)= 0.d0
        W(i,13)= 0.d0
        W(i,14)= 0.d0
        W(i,15)= 0.d0
        W(i,16)= 0.d0
        W(i,17)= 0.d0
      enddo
!$omp end parallel do
!
      DNRMsmp = 0.0d0
      COEFsmp = 0.0d0
      SP1smp = 0.0d0
      C0(1:5)= 0.0d0
      CG(1:5)= 0.0d0
      C0_smp(1:PEsmpTOT,1:5) = 0.0d0
!
!      ALPHA =  0.0d0
!      BETA =   0.0d0
!      OMEGA =  0.0d0
!      BNRM2 =  0.0d0
!      DNRM2 =  0.0d0
!      CG =     0.0d0
!      C1  =    0.0d0
!      C2  =    0.0d0
!      RHO  =   0.0d0
!      RHO1 =   0.0d0
!     C0  =    0.0d0
!      C10 =    0.0d0
!      C20 =    0.0d0
!      RHO0 =   0.0d0
!
      end subroutine init_work_GPBiCG_nn
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_GPBiCG_nn
!
      deallocate (W)
      deallocate (DNRMsmp, SP1smp)
      deallocate (COEFsmp, C0_smp)
      iflag_work_GPBiCGnn = 0
!
      end subroutine deallocate_work_GPBiCG_nn
!
!  ---------------------------------------------------------------------
!
      end module m_work_4_GPBiCGnn
