!
!      module m_work_4_BiCGSTAB11
!
!     Written by Kemorin
!
!      subroutine verify_work_BiCGSTAB_11(NP, PEsmpTOT)
!      subroutine allocate_work_BiCGSTAB_11(NP, PEsmpTOT)
!      subroutine init_work_BiCGSTAB_11(NP)
!      subroutine deallocate_work_BiCGSTAB_11
!
      module m_work_4_BiCGSTAB11
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: iflag_work_BiCGSTAB11 = 0
      real(kind=kreal), allocatable :: W(:,:)
!
      private :: iflag_work_BiCGSTAB11
!
      integer(kind = kint) :: iterPRE
      integer(kind = kint) :: ierr
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: ALPHA, BETA, OMEGA
      real(kind=kreal) :: BNRM2,  DNRM2,  C1,  C2, RHO, RHO1
      real(kind=kreal) :: BNRM20, DNRM20, C10, C20, RHO0
      real(kind=kreal) :: C0(2), CG(2)
      real(kind=kreal), allocatable :: DNRMsmp(:)
      real(kind=kreal), allocatable :: SP1smp(:)
      real(kind=kreal), allocatable :: SP2smp(:)
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_BiCGSTAB_11(NP, PEsmpTOT)
!
      integer(kind = kint), intent(in) :: NP, PEsmpTOT
!
      if (iflag_work_BiCGSTAB11.eq.0) then
        call allocate_work_BiCGSTAB_11(NP, PEsmpTOT)
      else if (iflag_work_BiCGSTAB11 .lt. (9*NP)) then
        call deallocate_work_BiCGSTAB_11
        call allocate_work_BiCGSTAB_11(NP, PEsmpTOT)
      end if
!
      end subroutine verify_work_BiCGSTAB_11
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_BiCGSTAB_11(NP, PEsmpTOT)
!
      integer(kind = kint), intent(in) :: NP, PEsmpTOT
!
      allocate (W(NP,9))
      allocate (DNRMsmp(PEsmpTOT))
      allocate (SP1smp(PEsmpTOT))
      allocate (SP2smp(PEsmpTOT))
      iflag_work_BiCGSTAB11 = 9*NP
!
      end subroutine allocate_work_BiCGSTAB_11
!
!  ---------------------------------------------------------------------
!
      subroutine init_work_BiCGSTAB_11(NP, PEsmpTOT)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint) :: i
!
!cdir parallel do
!$omp parallel do
!poption indep (W)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W)
      do i= 1, NP
        W(i, 1)= 0.d0
        W(i, 2)= 0.d0
        W(i, 3)= 0.d0
        W(i, 4)= 0.d0
        W(i, 5)= 0.d0
        W(i, 6)= 0.d0
        W(i, 7)= 0.d0
        W(i, 8)= 0.d0
        W(i, 9)= 0.d0
      enddo
!$omp end parallel do
!
      DNRMsmp = 0.0d0
      SP1smp = 0.0d0
      SP2smp = 0.0d0
!
      ALPHA =  0.0d0
      BETA =   0.0d0
      OMEGA =  0.0d0
      BNRM2 =  0.0d0
      DNRM2 =  0.0d0
      CG =     0.0d0
      C1  =    0.0d0
      C2  =    0.0d0
      RHO  =   0.0d0
      RHO1 =   0.0d0
      BNRM20 = 0.0d0
      DNRM20 = 0.0d0
      C0  =    0.0d0
      C10 =    0.0d0
      C20 =    0.0d0
      RHO0 =   0.0d0
!
      end subroutine init_work_BiCGSTAB_11
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_BiCGSTAB_11
!
      deallocate (W)
      deallocate (DNRMsmp, SP1smp, SP2smp)
      iflag_work_BiCGSTAB11 = 0
!
      end subroutine deallocate_work_BiCGSTAB_11
!
!  ---------------------------------------------------------------------
!
      end module m_work_4_BiCGSTAB11
