!
!      module m_work_4_MGCG11
!
!     Written by Kemorin
!
!      subroutine verify_work_MGCG_11(NP, PEsmpTOT)
!      subroutine allocate_work_MGCG_11(NP)
!      subroutine init_work_MGCG_11(NP)
!      subroutine deallocate_work_MGCG_11
!
      module m_work_4_MGCG11
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: iflag_work_MGCG11 = 0
      real(kind=kreal), allocatable :: W(:,:)
!
      private :: iflag_work_MGCG11
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_MGCG_11(NP, PEsmpTOT)
!
      integer(kind = kint), intent(in) :: NP, PEsmpTOT
!
      if (iflag_work_MGCG11 .eq. 0 ) then
        call allocate_work_MGCG_11(NP, PEsmpTOT)
      else if (iflag_work_MGCG11 .lt. (4*NP)) then
        call deallocate_work_MGCG_11
        call allocate_work_MGCG_11(NP, PEsmpTOT)
      end if
!
      end subroutine verify_work_MGCG_11
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_MGCG_11(NP, PEsmpTOT)
!
      integer(kind = kint), intent(in) :: NP, PEsmpTOT
!
      allocate (W(NP,4))
      allocate (DNRMsmp(PEsmpTOT))
      allocate (SPsmp(PEsmpTOT))
      iflag_work_MGCG11 = 4*NP
!
      end subroutine allocate_work_MGCG_11
!
!  ---------------------------------------------------------------------
!
      subroutine init_work_MGCG_11(NP)
!
       integer(kind = kint), intent(in) :: NP
       integer(kind = kint) :: i
!
!cdir parallel do
!$omp parallel do
!poption indep (W)
!cdir nodep
!voption indep (W)
      do i= 1, NP
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
      end subroutine init_work_MGCG_11
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_MGCG_11
!
      deallocate (W)
      iflag_work_MGCG11 = 0
!
      end subroutine deallocate_work_MGCG_11
!
!  ---------------------------------------------------------------------
!
      end module m_work_4_MGCG11
