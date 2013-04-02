!
!      module m_work_4_MGCGnn
!
!     Written by Kemorin
!
!      subroutine verify_work_MGCG_nn(NP, NB, PEsmpTOT)
!      subroutine allocate_work_MGCG_nn(NP, NB, PEsmpTOT)
!      subroutine init_work_MGCG_nn(NP, NB)
!      subroutine deallocate_work_MGCG_nn
!
      module m_work_4_MGCGnn
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: iflag_work_MGCGnn = 0
      real(kind=kreal), allocatable :: W(:,:)
!
      private :: iflag_work_MGCGnn
!
      integer(kind = kint) :: iterPRE
      integer(kind = kint) :: ierr
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: BNRM2,  DNRM2,  C1,  RHO, RHO1, ALPHA, BETA
      real(kind=kreal) :: BNRM20, DNRM20, C10, RHO0
      real(kind=kreal), allocatable :: DNRMsmp(:)
      real(kind=kreal), allocatable :: SPsmp(:)
!
      integer(kind=kint), parameter :: P = 3
      integer(kind=kint), parameter :: Q = 2
      integer(kind=kint), parameter :: R = 1
      integer(kind=kint), parameter :: Z = 2
      integer(kind=kint), parameter :: ZQ= 4
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_CG_nn(NP, NB, PEsmpTOT)
!
      integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
!
      if (iflag_work_MGCGnn.eq.0) then
        call allocate_work_MGCG_nn(NP, NB, PEsmpTOT)
      else if (iflag_work_MGCGnn .lt. (4*NB*NP)) then
        call deallocate_work_MGCG_nn
        call allocate_work_MGCG_nn(NP, NB, PEsmpTOT)
      end if
!
      end subroutine verify_work_CG_nn
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_MGCG_nn(NP, NB, PEsmpTOT)
!
      integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
!
      allocate (W(NB*NP,4))
      allocate (DNRMsmp(PEsmpTOT))
      allocate (SPsmp(PEsmpTOT))
      iflag_work_MGCGnn = 4*NB*NP
!
      end subroutine allocate_work_MGCG_nn
!
!  ---------------------------------------------------------------------
!
      subroutine init_work_MGCG_nn(NP, NB)
!
       integer(kind = kint), intent(in) :: NP, NB
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
      enddo
!$omp end parallel do
!
      DNRMsmp = 0.0d0
      SPsmp = 0.0d0
!
      BNRM2 =  0.0d0
      DNRM2 =  0.0d0
      C1  =    0.0d0
      RHO  =   0.0d0
      RHO1 =   0.0d0
      ALPHA =  0.0d0
      BETA =   0.0d0
      BNRM20 = 0.0d0
      DNRM20 = 0.0d0
      C10 =    0.0d0
      RHO0 =   0.0d0
!
      end subroutine init_work_MGCG_nn
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_MGCG_nn
!
      deallocate (W)
      deallocate (DNRMsmp, SPsmp)
      iflag_work_MGCGnn = 0
!
      end subroutine deallocate_work_MGCG_nn
!
!  ---------------------------------------------------------------------
!
      end module m_work_4_MGCGnn
