!C
!C*** 
!C*** module solver_BLCG_3
!C***
!C
      module solver_BLCG_3
!
      use m_precision
!
      implicit none
!
      real(kind=kreal), allocatable :: Dlu0(:,:,:)
      real(kind=kreal), allocatable :: AUlu0(:,:,:), ALlu0(:,:,:)
!
      integer(kind=kint), allocatable :: inumFI1L(:), FI1L(:)
      integer(kind=kint), allocatable :: inumFI1U(:), FI1U(:)
!
      private :: a_SCHWARTZ_33_BSSOR, GAUSSJ33
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!C
!C*** BLCG_3
!C
!C    BLCG_3 solves the linear system Ax = b with 3*3 block matrix 
!C    using the Conjugate Gradient iterative method with the following
!C    FULL-BLOCK TYPE preconditioners :
!C
!C      (1) Block IC(0) with Additive Shcwartz Domain Decomposition       
!C      (2) Block IC(1) with Additive Shcwartz Domain Decomposition       
!C      (3) Block IC(2) with Additive Shcwartz Domain Decomposition       
!C
      subroutine BLCG_3                                                 &
     &                 (N, NP, NPL, NPU, D, AL, INL, IAL, AU, INU, IAU, &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  RESID,  ITER, ERROR, iterPREmax,                &
     &                  my_rank, NEIBPETOT, NEIBPE,                     &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT,                       &
     &                  SOLVER_COMM , NSET)     

      use calypso_mpi
!
      use  solver_SR_3
      use vector_calc_solver_33
      use crs_matrix_calcs_33
!
      integer(kind=kint ), intent(in):: N, NP, NPU, NPL, my_rank
      integer(kind=kint ), intent(in):: NEIBPETOT, iterPREmax
      integer(kind=kint ), intent(in):: SOLVER_COMM
      integer(kind=kint ), intent(in):: NSET
      real   (kind=kreal), intent(in):: SIGMA, SIGMA_DIAG

      integer(kind=kint ), intent(inout):: ITER, ERROR
      real   (kind=kreal), intent(inout):: RESID

      real(kind=kreal), dimension(3*NP)   , intent(inout):: B, X
      real(kind=kreal), dimension(3,3,NPL), intent(inout):: AL
      real(kind=kreal), dimension(3,3,NPU), intent(inout):: AU
      real(kind=kreal), dimension(3,3,NP ), intent(inout):: D

      integer(kind=kint ), dimension(0:NP) ,intent(in) :: INU, INL
      integer(kind=kint ), dimension(  NPL),intent(in) :: IAL
      integer(kind=kint ), dimension(  NPU),intent(in) :: IAU
      character(len=kchara),                intent(in) :: PRECOND

      integer(kind=kint ), dimension(NEIBPETOT)   :: NEIBPE
! \beginARG       neighboring pe id                        (i-th pe)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
! \beginARG       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &       :: NOD_IMPORT
! \beginARG       imported degree of freedom               (i-th node)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
! \beginARG       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &       :: NOD_EXPORT
! \beginARG       exported node                            (i-th node)

      real(kind=kreal), dimension(:),    allocatable, save :: SCALE
      real(kind=kreal), dimension(:,:),  allocatable       :: WW
      real(kind=kreal), dimension(:,:,:),allocatable, save :: ALU

      integer(kind=kint ), parameter ::NB= 3 
!
      integer(kind=kint ) :: P, Q, R, Z, ZP, MAXIT, IFLAG, ierr
      integer(kind=kint ) :: i, j, k, ip, inod, ip1, ip2, ip3
      integer(kind=kint ) :: iterPRE
      integer(kind=kint ) :: isL, iEL, isU, iEU
      integer(kind=kint ) :: iq1, iq2, iq3
      real   (kind=kreal) :: RHO, RHO0, RHO1, BETA
      real   (kind=kreal) :: BNRM20, BNRM2
      real   (kind=kreal) :: DNRM20, DNRM2
      real   (kind=kreal) :: X1, X2, X3
      real   (kind=kreal) :: SW1, SW2, SW3
      real   (kind=kreal) :: D11, D22, D33
!!
      real   (kind=kreal) :: TOL, ALPHA, C1, C10
      data IFLAG/0/

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 301
        return
      endif

      allocate (WW(3*NP,4))

      if (IFLAG.eq.0) then
        allocate (SCALE(3*NP))
        allocate (ALU(3,3,NP))
        IFLAG= 1
        do i= 1, NP
          SCALE(3*i-2)= 1.d0
          SCALE(3*i-1)= 1.d0
          SCALE(3*i  )= 1.d0
        enddo
      endif

      R = 1
      Z = 2
      Q = 2
      P = 3
      ZP= 4
      
      MAXIT  = ITER
       TOL   = RESID           

      if (NSET.ge.1) then
!C
!C-- exchanging DIAGONAL components
        WW= 0.d0
        do i= 1, N
          WW(3*i-2,1)= D(1,1,i)
          WW(3*i-1,1)= D(2,1,i)
          WW(3*i  ,1)= D(3,1,i)
          WW(3*i-2,2)= D(1,2,i)
          WW(3*i-1,2)= D(2,2,i)
          WW(3*i  ,2)= D(3,2,i)
          WW(3*i-2,3)= D(1,3,i)
          WW(3*i-1,3)= D(2,3,i)
          WW(3*i  ,3)= D(3,3,i)
        enddo
        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,1))
        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,2))
        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,3))
        do i= N+1, NP
          D(1,1,i)= WW(3*i-2,1)
          D(2,1,i)= WW(3*i-1,1)
          D(3,1,i)= WW(3*i  ,1)
          D(1,2,i)= WW(3*i-2,2)
          D(2,2,i)= WW(3*i-1,2)
          D(3,2,i)= WW(3*i  ,2)
          D(1,3,i)= WW(3*i-2,3)
          D(2,3,i)= WW(3*i-1,3)
          D(3,3,i)= WW(3*i  ,3)
        enddo

!C
!C-- SCALING
      if (NSET.eq.2) then
        do i= 1, NP
          SCALE (3*i-2)= 1.d0/dsqrt(dabs(D(1,1,i)))
          SCALE (3*i-1)= 1.d0/dsqrt(dabs(D(2,2,i)))
          SCALE (3*i  )= 1.d0/dsqrt(dabs(D(3,3,i)))
        enddo

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SCALE)

      do i= 1, NP
        ip1= 3*i-2
        ip2= 3*i-1
        ip3= 3*i 
        D(1,1,i)= D(1,1,i)*SCALE(ip1)*SCALE(ip1)
        D(1,2,i)= D(1,2,i)*SCALE(ip1)*SCALE(ip2)
        D(1,3,i)= D(1,3,i)*SCALE(ip1)*SCALE(ip3)
        D(2,1,i)= D(2,1,i)*SCALE(ip2)*SCALE(ip1)
        D(2,2,i)= D(2,2,i)*SCALE(ip2)*SCALE(ip2)
        D(2,3,i)= D(2,3,i)*SCALE(ip2)*SCALE(ip3)
        D(3,1,i)= D(3,1,i)*SCALE(ip3)*SCALE(ip1)
        D(3,2,i)= D(3,2,i)*SCALE(ip3)*SCALE(ip2)
        D(3,3,i)= D(3,3,i)*SCALE(ip3)*SCALE(ip3)

          isL= INL(i-1) + 1
          ieL= INL(i  ) 
          do k= isL, ieL
             inod= IAL(k)
             iq1= 3*inod - 2
             iq2= 3*inod - 1
             iq3= 3*inod 
            AL(1,1,k)= AL(1,1,k)*SCALE(ip1)*SCALE(iq1)
            AL(1,2,k)= AL(1,2,k)*SCALE(ip1)*SCALE(iq2)
            AL(1,3,k)= AL(1,3,k)*SCALE(ip1)*SCALE(iq3)
            AL(2,1,k)= AL(2,1,k)*SCALE(ip2)*SCALE(iq1)
            AL(2,2,k)= AL(2,2,k)*SCALE(ip2)*SCALE(iq2)
            AL(2,3,k)= AL(2,3,k)*SCALE(ip2)*SCALE(iq3)
            AL(3,1,k)= AL(3,1,k)*SCALE(ip3)*SCALE(iq1)
            AL(3,2,k)= AL(3,2,k)*SCALE(ip3)*SCALE(iq2)
            AL(3,3,k)= AL(3,3,k)*SCALE(ip3)*SCALE(iq3)
          enddo

          isU= INU(i-1) + 1
          ieU= INU(i  ) 
          do k= isU, ieU
             inod= IAU(k)
             iq1= 3*inod - 2
             iq2= 3*inod - 1
             iq3= 3*inod 
            AU(1,1,k)= AU(1,1,k)*SCALE(ip1)*SCALE(iq1)
            AU(1,2,k)= AU(1,2,k)*SCALE(ip1)*SCALE(iq2)
            AU(1,3,k)= AU(1,3,k)*SCALE(ip1)*SCALE(iq3)
            AU(2,1,k)= AU(2,1,k)*SCALE(ip2)*SCALE(iq1)
            AU(2,2,k)= AU(2,2,k)*SCALE(ip2)*SCALE(iq2)
            AU(2,3,k)= AU(2,3,k)*SCALE(ip2)*SCALE(iq3)
            AU(3,1,k)= AU(3,1,k)*SCALE(ip3)*SCALE(iq1)
            AU(3,2,k)= AU(3,2,k)*SCALE(ip3)*SCALE(iq2)
            AU(3,3,k)= AU(3,3,k)*SCALE(ip3)*SCALE(iq3)
          enddo
        enddo
      endif
!C===

!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
      if (PRECOND.eq.'BILU1') call FORM_ILU1_33(NB, NP, NPL, NPU, D,    &
     &                            AL, INL, IAL, AU, INU, IAU)
      if (PRECOND.eq.'BILU2') call FORM_ILU2_33(NB, NP, NPL, NPU, D,    &
     &                            AL, INL, IAL, AU, INU, IAU)

      if (PRECOND.eq.'BILU0') then
        do ip= 1, NP
          D11= D(1,1,ip) * SIGMA_DIAG
          D22= D(2,2,ip) * SIGMA_DIAG
          D33= D(3,3,ip) * SIGMA_DIAG
          call ILU1a33 (ALU(1,1,ip),                                    &
     &                  D11      , D(1,2,ip), D(1,3,ip),                &
     &                  D(2,1,ip), D22      , D(2,3,ip),                &
     &                  D(3,1,ip), D(3,2,ip), D33)
        enddo
      endif

      if (PRECOND.eq.'BILU1'.or.PRECOND.eq.'BILU2') then
        do ip= 1, NP
          call ILU1a33 (ALU(1,1,ip),                                    &
     &                  Dlu0(1,1,ip), Dlu0(1,2,ip), Dlu0(1,3,ip),       &
     &                  Dlu0(2,1,ip), Dlu0(2,2,ip), Dlu0(2,3,ip),       &
     &                  Dlu0(3,1,ip), Dlu0(3,2,ip), Dlu0(3,3,ip))
        enddo
      endif
!C===
      endif

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
      do i= 1, N
        B(3*i-2)= B(3*i-2) * SCALE(3*i-2)
        B(3*i-1)= B(3*i-1) * SCALE(3*i-1)
        B(3*i  )= B(3*i  ) * SCALE(3*i  )
      enddo
!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)

      call subtruct_crs_matvec_33(NP, N, NPL, NPU, INL, INU, IAL, IAU,  &
     &    D, AL, AU,  WW(1,R), B, X)

      BNRM20= 0.d0
      do i= 1, N
        BNRM20= BNRM20+B(3*i-2)**2+B(3*i-1)**2+B(3*i)**2
      enddo

      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
      ITER = 0
!C===

      do iter= 1, MAXIT
!C
!C************************************************* Conjugate Gradient Iteration

!C
!C +----------------+
!C | {z}= [Minv]{r} |
!C +----------------+
!C===

!C
!C== Block SSOR
      if (PRECOND.eq.'BILU0') then

      do i= 1, N
        WW(3*i-2,ZP)= WW(3*i-2,R)
        WW(3*i-1,ZP)= WW(3*i-1,R)
        WW(3*i  ,ZP)= WW(3*i  ,R)
      enddo

      do i= 1, NP
        WW(3*i-2,Z )= 0.d0
        WW(3*i-1,Z )= 0.d0
        WW(3*i  ,Z )= 0.d0
      enddo

      do iterPRE= 1, iterPREmax
        do i= 1+N, NP
          WW(3*i-2,ZP)= 0.d0
          WW(3*i-1,ZP)= 0.d0
          WW(3*i  ,ZP)= 0.d0
        enddo
!C
!C-- FORWARD
        do i= 1, N
          SW1= WW(3*i-2,ZP)
          SW2= WW(3*i-1,ZP)
          SW3= WW(3*i  ,ZP)
          isL= INL(i-1)+1
          ieL= INL(i)
          do j= isL, ieL
              k= IAL(j)
             X1= WW(3*k-2,ZP)
             X2= WW(3*k-1,ZP)
             X3= WW(3*k  ,ZP)
            SW1= SW1 - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
            SW2= SW2 - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
            SW3= SW3 - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
          enddo

          X1= SW1
          X2= SW2
          X3= SW3
          X2= X2 - ALU(2,1,i)*X1
          X3= X3 - ALU(3,1,i)*X1 - ALU(3,2,i)*X2
          X3= ALU(3,3,i)*  X3
          X2= ALU(2,2,i)*( X2 - ALU(2,3,i)*X3 )
          X1= ALU(1,1,i)*( X1 - ALU(1,3,i)*X3 - ALU(1,2,i)*X2)
          WW(3*i-2,ZP)= X1
          WW(3*i-1,ZP)= X2
          WW(3*i  ,ZP)= X3
        enddo
!C
!C-- BACKWARD
        do i= N, 1, -1
          isU= INU(i-1) + 1
          ieU= INU(i) 
          SW1= 0.d0
          SW2= 0.d0
          SW3= 0.d0
          do j= ieU, isU, -1
              k= IAU(j)
             X1= WW(3*k-2,ZP)
             X2= WW(3*k-1,ZP)
             X3= WW(3*k  ,ZP)
            SW1= SW1 + AU(1,1,j)*X1 + AU(1,2,j)*X2 + AU(1,3,j)*X3
            SW2= SW2 + AU(2,1,j)*X1 + AU(2,2,j)*X2 + AU(2,3,j)*X3
            SW3= SW3 + AU(3,1,j)*X1 + AU(3,2,j)*X2 + AU(3,3,j)*X3
          enddo
          X1= SW1
          X2= SW2
          X3= SW3
          X2= X2 - ALU(2,1,i)*X1
          X3= X3 - ALU(3,1,i)*X1 - ALU(3,2,i)*X2
          X3= ALU(3,3,i)*  X3
          X2= ALU(2,2,i)*( X2 - ALU(2,3,i)*X3 )
          X1= ALU(1,1,i)*( X1 - ALU(1,3,i)*X3 - ALU(1,2,i)*X2)
          WW(3*i-2,ZP)=  WW(3*i-2,ZP) - X1
          WW(3*i-1,ZP)=  WW(3*i-1,ZP) - X2
          WW(3*i  ,ZP)=  WW(3*i  ,ZP) - X3
        enddo

!C
!C-- additive Schwartz

        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,ZP))

        call a_SCHWARTZ_33_BSSOR(iterPRE, iterPREmax, R, ZP, Z,         &
     &      SIGMA_DIAG, N, NP, NPL, NPU, D, AL, INL, IAL,               &
     &      AU, INU, IAU, WW)

      enddo
      endif

!C
!C== Block ILU
      if (PRECOND.eq.'BILU1'.or.PRECOND.eq.'BILU2') then
      do i= 1, N
        WW(3*i-2,ZP)= WW(3*i-2,R)
        WW(3*i-1,ZP)= WW(3*i-1,R)
        WW(3*i  ,ZP)= WW(3*i  ,R)
      enddo

      do i= 1, NP
        WW(3*i-2,Z )= 0.d0
        WW(3*i-1,Z )= 0.d0
        WW(3*i  ,Z )= 0.d0
      enddo

      do iterPRE= 1, iterPREmax
        do i= 1+N, NP
          WW(3*i-2,ZP)= 0.d0
          WW(3*i-1,ZP)= 0.d0
          WW(3*i  ,ZP)= 0.d0
        enddo

!C
!C-- FORWARD
        do i= 1, N
          SW1= WW(3*i-2,ZP)
          SW2= WW(3*i-1,ZP)
          SW3= WW(3*i  ,ZP)
          isL= inumFI1L(i-1)+1
          ieL= inumFI1L(i)
          do j= isL, ieL
              k= FI1L(j)
             X1= WW(3*k-2,ZP)
             X2= WW(3*k-1,ZP)
             X3= WW(3*k  ,ZP)
            SW1= SW1-ALlu0(1,1,j)*X1-ALlu0(1,2,j)*X2-ALlu0(1,3,j)*X3
            SW2= SW2-ALlu0(2,1,j)*X1-ALlu0(2,2,j)*X2-ALlu0(2,3,j)*X3
            SW3= SW3-ALlu0(3,1,j)*X1-ALlu0(3,2,j)*X2-ALlu0(3,3,j)*X3
          enddo

          X1= SW1
          X2= SW2
          X3= SW3
          X2= X2 - ALU(2,1,i)*X1
          X3= X3 - ALU(3,1,i)*X1 - ALU(3,2,i)*X2
          X3= ALU(3,3,i)*  X3
          X2= ALU(2,2,i)*( X2 - ALU(2,3,i)*X3 )
          X1= ALU(1,1,i)*( X1 - ALU(1,3,i)*X3 - ALU(1,2,i)*X2)
          WW(3*i-2,ZP)=  X1
          WW(3*i-1,ZP)=  X2
          WW(3*i  ,ZP)=  X3
        enddo
      
!C
!C-- BACKWARD
        do i= N, 1, -1
          isU= inumFI1U(i-1) + 1
          ieU= inumFI1U(i) 
          SW1= 0.d0
          SW2= 0.d0
          SW3= 0.d0
          do j= ieU, isU, -1
              k= FI1U(j)
             X1= WW(3*k-2,ZP)
             X2= WW(3*k-1,ZP)
             X3= WW(3*k  ,ZP)
            SW1= SW1+AUlu0(1,1,j)*X1+AUlu0(1,2,j)*X2+AUlu0(1,3,j)*X3
            SW2= SW2+AUlu0(2,1,j)*X1+AUlu0(2,2,j)*X2+AUlu0(2,3,j)*X3
            SW3= SW3+AUlu0(3,1,j)*X1+AUlu0(3,2,j)*X2+AUlu0(3,3,j)*X3
          enddo

          X1= SW1
          X2= SW2
          X3= SW3
          X2= X2 - ALU(2,1,i)*X1
          X3= X3 - ALU(3,1,i)*X1 - ALU(3,2,i)*X2
          X3= ALU(3,3,i)*  X3
          X2= ALU(2,2,i)*( X2 - ALU(2,3,i)*X3 )
          X1= ALU(1,1,i)*( X1 - ALU(1,3,i)*X3 - ALU(1,2,i)*X2)
          WW(3*i-2,ZP)=  WW(3*i-2,ZP) - X1
          WW(3*i-1,ZP)=  WW(3*i-1,ZP) - X2
          WW(3*i  ,ZP)=  WW(3*i  ,ZP) - X3
        enddo

!C
!C-- additive Schwartz

        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,ZP))

        call a_SCHWARTZ_33_BSSOR(iterPRE, iterPREmax, R, ZP, Z,         &
     &      SIGMA_DIAG, N, NP, NPL, NPU, D, AL, INL, IAL,               &
     &      AU, INU, IAU, WW)

      enddo
      endif
!C==
!C===
      
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
      RHO0= 0.d0
      do i= 1, N
        RHO0= RHO0 + WW(3*i-2,R)*WW(3*i-2,Z) + WW(3*i-1,R)*WW(3*i-1,Z)  &
     &             + WW(3*i  ,R)*WW(3*i  ,Z)
      enddo

      call MPI_allREDUCE (RHO0, RHO, 1, MPI_DOUBLE_PRECISION,           &
     &                    MPI_SUM, SOLVER_COMM, ierr)
!C===

!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C +-----------------------------+
!C===
      if ( ITER.eq.1 ) then
        do i= 1, N
          WW(3*i-2,P)= WW(3*i-2,Z)
          WW(3*i-1,P)= WW(3*i-1,Z)
          WW(3*i  ,P)= WW(3*i  ,Z)
        enddo
       else
         BETA= RHO / RHO1
         do i= 1, N
           WW(3*i-2,P)= WW(3*i-2,Z) + BETA*WW(3*i-2,P)
           WW(3*i-1,P)= WW(3*i-1,Z) + BETA*WW(3*i-1,P)
           WW(3*i  ,P)= WW(3*i  ,Z) + BETA*WW(3*i  ,P)
         enddo
      endif
!C===

!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===        

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,P))
!
        call cal_crs_matvec_33(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      D, AL, AU,  WW(1,Q), WW(1,P) )
!
!C===

!C
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
      C10= 0.d0
      do i= 1, N
        C10= C10 + WW(3*i-2,P)*WW(3*i-2,Q) + WW(3*i-1,P)*WW(3*i-1,Q)    &
     &           + WW(3*i  ,P)*WW(3*i  ,Q)
      enddo
      call MPI_allREDUCE (C10, C1, 1, MPI_DOUBLE_PRECISION,             &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      ALPHA= RHO / C1
!C===

!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C +----------------------+
!C===
      do i= 1, N
         X(3*i-2)  = X (3*i-2)   + ALPHA * WW(3*i-2,P)
         X(3*i-1)  = X (3*i-1)   + ALPHA * WW(3*i-1,P)
         X(3*i  )  = X (3*i  )   + ALPHA * WW(3*i  ,P)
        WW(3*i-2,R)= WW(3*i-2,R) - ALPHA * WW(3*i-2,Q)
        WW(3*i-1,R)= WW(3*i-1,R) - ALPHA * WW(3*i-1,Q)
        WW(3*i  ,R)= WW(3*i  ,R) - ALPHA * WW(3*i  ,Q)
      enddo

      DNRM20= 0.d0
      do i= 1, N
        DNRM20= DNRM20 + WW(3*i-2,R)**2 + WW(3*i-1,R)**2                &
     &                                  + WW(3*i  ,R)**2
      enddo
      call MPI_allREDUCE (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)

        RESID= dsqrt(DNRM2/BNRM2)

!C##### ITERATION HISTORY
!        if (my_rank.eq.0) write (*, 1000) ITER, RESID
!        if (my_rank.eq.0) write (12,1010) RESID
! 1000   format (i5, 1pe16.6)
! 1010   format (1pe16.6)
!C#####

        if ( RESID.le.TOL   ) exit
        if ( ITER .eq.MAXIT ) ERROR= -300

        RHO1 = RHO                                                             
      enddo
!C===

!C
!C-- INTERFACE data EXCHANGE
      do i= 1, N
        X(3*i-2)= X(3*i-2) * SCALE(3*i-2)
        X(3*i-1)= X(3*i-1) * SCALE(3*i-1)
        X(3*i  )= X(3*i  ) * SCALE(3*i  )
        B(3*i-2)= B(3*i-2) / SCALE(3*i-2)
        B(3*i-1)= B(3*i-1) / SCALE(3*i-1)
        B(3*i  )= B(3*i  ) / SCALE(3*i  )
      enddo

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)

      deallocate (WW)
!
      end subroutine BLCG_3
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
!C
!C***
!C*** FORM_ILU1_33
!C***
!C
!C    form ILU(1) matrix
!C
!
!  ---------------------------------------------------------------------
!
      subroutine FORM_ILU1_33(NB, NP, NPL, NPU, D,                      &
     &          AL, INL, IAL, AU, INU, IAU)
!
      integer(kind=kint ), intent(in):: NP, NPU, NPL, NB
      integer(kind=kint ), dimension(0:NP) ,intent(in) :: INU, INL
      integer(kind=kint ), dimension(  NPL),intent(in) :: IAL
      integer(kind=kint ), dimension(  NPU),intent(in) :: IAU
      real(kind=kreal), dimension(3,3,NP), intent(in):: D
      real(kind=kreal), dimension(3,3,NPL), intent(in):: AL
      real(kind=kreal), dimension(3,3,NPU), intent(in):: AU
!
      integer(kind=kint), dimension(:), allocatable :: IW1 , IW2
      integer(kind=kint), dimension(:), allocatable :: IWsL, IWsU
      real (kind=kreal),  dimension(3,3) :: RHS_Aij, DkINV, Aik, Akj
!
      integer(kind=kint) :: L, NPLf1, NPLf2, icou0, ij0
      real   (kind=kreal) :: D11, D12, D13
      real   (kind=kreal) :: D21, D22, D23
      real   (kind=kreal) :: D31, D32, D33
!
      integer(kind=kint) :: NPUf1
      integer(kind=kint) :: i, k, kk, j, jj, ik, kk1, jj1
      integer(kind=kint) :: iSj, iEj, iSk, iEk
      integer(kind=kint) :: iSU, iSL
      integer(kind=kint) :: icouU, icouL, icou
      integer(kind=kint) :: icouL1, icouL2, icouL3
      integer(kind=kint) :: icouU1, icouU2, icouU3
!C
!C +--------------+
!C | find fill-in |
!C +--------------+
!C===

!C
!C-- count fill-in
      allocate (IW1(NP) , IW2(NP))
      allocate (inumFI1L(0:NP), inumFI1U(0:NP))

      inumFI1L= 0
      inumFI1U= 0

      NPLf1= 0
      NPLf2= 0
      do i= 2, NP
        icou= 0
        IW1= 0
        IW1(i)= 1
        do L= INL(i-1)+1, INL(i)
          IW1(IAL(L))= 1
        enddo
        do L= INU(i-1)+1, INU(i)
          IW1(IAU(L))= 1
        enddo

        iSk= INL(i-1) + 1
        iEk= INL(i)
        do k= iSk, iEk
          kk= IAL(k)
          iSj= INU(kk-1) + 1
          iEj= INU(kk  )
          do j= iSj, iEj
            jj= IAU(j)
            if (IW1(jj).eq.0 .and. jj.lt.i) then
              inumFI1L(i)= inumFI1L(i)+1
                  IW1(jj)= 1
            endif  
            if (IW1(jj).eq.0 .and. jj.gt.i) then
              inumFI1U(i)= inumFI1U(i)+1
                  IW1(jj)= 1
            endif  
          enddo
        enddo
        NPLf1= NPLf1 + inumFI1L(i)
        NPUf1= NPUf1 + inumFI1U(i)
      enddo

!C
!C-- specify fill-in

      allocate (IWsL(0:NP), IWsU(0:NP))
      allocate (FI1L (NPL+NPLf1), FI1U (NPU+NPUf1))
      allocate (ALlu0(NPLf2,NB,NPL+NPLf1), AUlu0(NB,NB,NPU+NPUf1))

      FI1L= 0
      FI1U= 0

      IWsL= 0
      IWsU= 0
      do i= 1, NP
        IWsL(i)= INL(i)-INL(i-1) + inumFI1L(i) + IWsL(i-1)
        IWsU(i)= INU(i)-INU(i-1) + inumFI1U(i) + IWsU(i-1)
      enddo

      do i= 2, NP
        icouL= 0
        icouU= 0
        inumFI1L(i)= inumFI1L(i-1) + inumFI1L(i)
        inumFI1U(i)= inumFI1U(i-1) + inumFI1U(i)
        icou= 0
        IW1= 0
        IW1(i)= 1
        do L= INL(i-1)+1, INL(i)
          IW1(IAL(L))= 1
        enddo
        do L= INU(i-1)+1, INU(i)
          IW1(IAU(L))= 1
        enddo

        iSk= INL(i-1) + 1
        iEk= INL(i)
        do k= iSk, iEk
          kk= IAL(k)
          iSj= INU(kk-1) + 1
          iEj= INU(kk  )
          do j= iSj, iEj
            jj= IAU(j)
            if (IW1(jj).eq.0 .and. jj.lt.i) then
                   icouL           = icouL + 1
              FI1L(icouL+IWsL(i-1)+INL(i)-INL(i-1))= jj
                  IW1(jj)          = 1
            endif
            if (IW1(jj).eq.0 .and. jj.gt.i) then
                   icouU           = icouU + 1
              FI1U(icouU+IWsU(i-1)+INU(i)-INU(i-1))= jj
                  IW1(jj)          = 1
            endif
          enddo
        enddo
      enddo
!C=== 

!C
!C +-------------------------------------------------+
!C | SORT and RECONSTRUCT matrix considering fill-in |
!C +-------------------------------------------------+
!C===
      ALlu0= 0.d0
      AUlu0= 0.d0
      iSL  = 0
      iSU  = 0
      do i= 1, NP
        icouL1=      INL(i) -      INL(i-1)
        icouL2= inumFI1L(i) - inumFI1L(i-1)
        icouL3= icouL1 + icouL2
        icouU1=      INU(i) -      INU(i-1)
        icouU2= inumFI1U(i) - inumFI1U(i-1)
        icouU3= icouU1 + icouU2
!C
!C-- LOWER part
        icou0= 0
        do k= INL(i-1)+1, INL(i)
              icou0 = icou0 + 1
          IW1(icou0)= IAL(k)
        enddo

        do k= inumFI1L(i-1)+1, inumFI1L(i)
              icou0 = icou0 + 1
          IW1(icou0)= FI1L(icou0+IWsL(i-1))
        enddo

        do k= 1, icouL3
          IW2(k)= k
        enddo
        call fill_in_S33_SORT (IW1, IW2, icouL3, NP)

        do k= 1, icouL3
          FI1L (k+isL)= IW1(k)
          ik= IW2(k)
          if (ik.le.INL(i)-INL(i-1)) then
            ALlu0(1,1,k+isL)= AL(1,1,ik+INL(i-1))
            ALlu0(1,2,k+isL)= AL(1,2,ik+INL(i-1))
            ALlu0(1,3,k+isL)= AL(1,3,ik+INL(i-1))
            ALlu0(2,1,k+isL)= AL(2,1,ik+INL(i-1))
            ALlu0(2,2,k+isL)= AL(2,2,ik+INL(i-1))
            ALlu0(2,3,k+isL)= AL(2,3,ik+INL(i-1))
            ALlu0(3,1,k+isL)= AL(3,1,ik+INL(i-1))
            ALlu0(3,2,k+isL)= AL(3,2,ik+INL(i-1))
            ALlu0(3,3,k+isL)= AL(3,3,ik+INL(i-1))
          endif
        enddo
!C
!C-- UPPER part
        icou0= 0
        do k= INU(i-1)+1, INU(i)
              icou0 = icou0 + 1
          IW1(icou0)= IAU(k)
        enddo

        do k= inumFI1U(i-1)+1, inumFI1U(i)
              icou0 = icou0 + 1
          IW1(icou0)= FI1U(icou0+IWsU(i-1))
        enddo

        do k= 1, icouU3
          IW2(k)= k
        enddo
        call fill_in_S33_SORT (IW1, IW2, icouU3, NP)

        do k= 1, icouU3
          FI1U (k+isU)= IW1(k)
          ik= IW2(k)
          if (ik.le.INU(i)-INU(i-1)) then
            AUlu0(1,1,k+isU)= AU(1,1,ik+INU(i-1))
            AUlu0(1,2,k+isU)= AU(1,2,ik+INU(i-1))
            AUlu0(1,3,k+isU)= AU(1,3,ik+INU(i-1))
            AUlu0(2,1,k+isU)= AU(2,1,ik+INU(i-1))
            AUlu0(2,2,k+isU)= AU(2,2,ik+INU(i-1))
            AUlu0(2,3,k+isU)= AU(2,3,ik+INU(i-1))
            AUlu0(3,1,k+isU)= AU(3,1,ik+INU(i-1))
            AUlu0(3,2,k+isU)= AU(3,2,ik+INU(i-1))
            AUlu0(3,3,k+isU)= AU(3,3,ik+INU(i-1))
          endif
        enddo

        iSL= iSL + icouL3
        iSU= iSU + icouU3
      enddo
!C===
      do i= 1, NP
        inumFI1L(i)= IWsL(i)
        inumFI1U(i)= IWsU(i)
      enddo
      deallocate (IWsL, IWsU)

!C
!C +----------------------+
!C | ILU(1) factorization |
!C +----------------------+
!C===
      allocate (Dlu0(NB,NB,NP))
      Dlu0= D

      do i= 2, NP
        IW1= 0
        IW2= 0

        do k= inumFI1L(i-1)+1, inumFI1L(i)
          IW1(FI1L(k))= k
        enddo

        do k= inumFI1U(i-1)+1, inumFI1U(i)
          IW2(FI1U(k))= k
        enddo

        do kk= INL(i-1)+1, INL(i)
          k= IAL(kk)
          D11= Dlu0(1,1,k)
          D12= Dlu0(1,2,k)
          D13= Dlu0(1,3,k)
          D21= Dlu0(2,1,k)
          D22= Dlu0(2,2,k)
          D23= Dlu0(2,3,k)
          D31= Dlu0(3,1,k)
          D32= Dlu0(3,2,k)
          D33= Dlu0(3,3,k)

          call ILU1a33 (DkINV, D11,D12,D13,D21,D22,D23,D31,D32,D33)

          do kk1= inumFI1L(i-1)+1, inumFI1L(i)
            if (k.eq.FI1L(kk1)) then
              Aik(1,1)= ALlu0(1,1,kk1)
              Aik(1,2)= ALlu0(1,2,kk1)
              Aik(1,3)= ALlu0(1,3,kk1)
              Aik(2,1)= ALlu0(2,1,kk1)
              Aik(2,2)= ALlu0(2,2,kk1)
              Aik(2,3)= ALlu0(2,3,kk1)
              Aik(3,1)= ALlu0(3,1,kk1)
              Aik(3,2)= ALlu0(3,2,kk1)
              Aik(3,3)= ALlu0(3,3,kk1)
              exit
            endif
          enddo
  
          do jj= INU(k-1)+1, INU(k)
            j= IAU(jj)
            do jj1= inumFI1U(k-1)+1, inumFI1U(k)
              if (j.eq.FI1U(jj1)) then
                Akj(1,1)= AUlu0(1,1,jj1)
                Akj(1,2)= AUlu0(1,2,jj1)
                Akj(1,3)= AUlu0(1,3,jj1)
                Akj(2,1)= AUlu0(2,1,jj1)
                Akj(2,2)= AUlu0(2,2,jj1)
                Akj(2,3)= AUlu0(2,3,jj1)
                Akj(3,1)= AUlu0(3,1,jj1)
                Akj(3,2)= AUlu0(3,2,jj1)
                Akj(3,3)= AUlu0(3,3,jj1)
                exit
              endif
            enddo

            call ILU1b33 (RHS_Aij, DkINV, Aik, Akj)

            if (j.eq.i) then
              Dlu0(1,1,i)= Dlu0(1,1,i) - RHS_Aij(1,1)
              Dlu0(1,2,i)= Dlu0(1,2,i) - RHS_Aij(1,2)
              Dlu0(1,3,i)= Dlu0(1,3,i) - RHS_Aij(1,3)
              Dlu0(2,1,i)= Dlu0(2,1,i) - RHS_Aij(2,1)
              Dlu0(2,2,i)= Dlu0(2,2,i) - RHS_Aij(2,2)
              Dlu0(2,3,i)= Dlu0(2,3,i) - RHS_Aij(2,3)
              Dlu0(3,1,i)= Dlu0(3,1,i) - RHS_Aij(3,1)
              Dlu0(3,2,i)= Dlu0(3,2,i) - RHS_Aij(3,2)
              Dlu0(3,3,i)= Dlu0(3,3,i) - RHS_Aij(3,3)
            endif   

            if (j.lt.i) then
              ij0= IW1(j)
              ALlu0(1,1,ij0)= ALlu0(1,1,ij0) - RHS_Aij(1,1)
              ALlu0(1,2,ij0)= ALlu0(1,2,ij0) - RHS_Aij(1,2)
              ALlu0(1,3,ij0)= ALlu0(1,3,ij0) - RHS_Aij(1,3)
              ALlu0(2,1,ij0)= ALlu0(2,1,ij0) - RHS_Aij(2,1)
              ALlu0(2,2,ij0)= ALlu0(2,2,ij0) - RHS_Aij(2,2)
              ALlu0(2,3,ij0)= ALlu0(2,3,ij0) - RHS_Aij(2,3)
              ALlu0(3,1,ij0)= ALlu0(3,1,ij0) - RHS_Aij(3,1)
              ALlu0(3,2,ij0)= ALlu0(3,2,ij0) - RHS_Aij(3,2)
              ALlu0(3,3,ij0)= ALlu0(3,3,ij0) - RHS_Aij(3,3)
            endif   

            if (j.gt.i) then
              ij0= IW2(j)
              AUlu0(1,1,ij0)= AUlu0(1,1,ij0) - RHS_Aij(1,1)
              AUlu0(1,2,ij0)= AUlu0(1,2,ij0) - RHS_Aij(1,2)
              AUlu0(1,3,ij0)= AUlu0(1,3,ij0) - RHS_Aij(1,3)
              AUlu0(2,1,ij0)= AUlu0(2,1,ij0) - RHS_Aij(2,1)
              AUlu0(2,2,ij0)= AUlu0(2,2,ij0) - RHS_Aij(2,2)
              AUlu0(2,3,ij0)= AUlu0(2,3,ij0) - RHS_Aij(2,3)
              AUlu0(3,1,ij0)= AUlu0(3,1,ij0) - RHS_Aij(3,1)
              AUlu0(3,2,ij0)= AUlu0(3,2,ij0) - RHS_Aij(3,2)
              AUlu0(3,3,ij0)= AUlu0(3,3,ij0) - RHS_Aij(3,3)
            endif   

          enddo
        enddo
      enddo

      deallocate (IW1, IW2)
!C===
      end subroutine FORM_ILU1_33
!
!  ---------------------------------------------------------------------
!
!C
!C***
!C*** FORM_ILU2_33
!C***
!C
!C    form ILU(2) matrix
!C
      subroutine FORM_ILU2_33(NB, NP, NPL, NPU, D,                      &
     &          AL, INL, IAL, AU, INU, IAU)
!
      integer(kind=kint ), intent(in):: NP, NPU, NPL, NB
      integer(kind=kint ), dimension(0:NP) ,intent(in) :: INU, INL
      integer(kind=kint ), dimension(  NPL),intent(in) :: IAL
      integer(kind=kint ), dimension(  NPU),intent(in) :: IAU
      real(kind=kreal), dimension(3,3,NP), intent(in):: D
      real(kind=kreal), dimension(3,3,NPL), intent(in):: AL
      real(kind=kreal), dimension(3,3,NPU), intent(in):: AU
!
      integer(kind=kint), allocatable:: IW1(:) , IW2(:)
      integer(kind=kint), allocatable:: IWsL(:), IWsU(:)
      integer(kind=kint), allocatable:: iconFI1L(:), iconFI1U(:)
      integer(kind=kint), allocatable:: inumFI2L(:), inumFI2U(:)
      integer(kind=kint), allocatable::     FI2L(:),     FI2U(:)
      real (kind=kreal) :: RHS_Aij(3,3), DkINV(3,3), Aik(3,3), Akj(3,3)
!
      real(kind=kreal), allocatable :: AUlu0(:,:,:), ALlu0(:,:,:)
      integer(kind=kint) :: L, NPLf1, NPLf2, NPUf1, NPUf2, iAS, ij0
!
      real   (kind=kreal) :: D11, D12, D13
      real   (kind=kreal) :: D21, D22, D23
      real   (kind=kreal) :: D31, D32, D33
      integer(kind=kint) :: i, k, kk, j, jj, ik
      integer(kind=kint) :: iSj, iEj, iSk, iEk
      integer(kind=kint) :: iSU, iSL
      integer(kind=kint) :: icouU, icouL, icou
      integer(kind=kint) :: icouL1, icouL2, icouL3
      integer(kind=kint) :: icouU1, icouU2, icouU3, iconIK, iconKJ
!C
!C +------------------+  
!C | find fill-in (1) |
!C +------------------+
!C===

!C
!C-- count fill-in
      allocate (IW1(NP) , IW2(NP))
      allocate (inumFI2L(0:NP), inumFI2U(0:NP))

      inumFI2L= 0
      inumFI2U= 0

      NPLf1= 0
      NPLf2= 0
      do i= 2, NP
        icou= 0
        IW1= 0
        IW1(i)= 1
        do L= INL(i-1)+1, INL(i)
          IW1(IAL(L))= 1
        enddo
        do L= INU(i-1)+1, INU(i)
          IW1(IAU(L))= 1
        enddo

        iSk= INL(i-1) + 1
        iEk= INL(i)
        do k= iSk, iEk
          kk= IAL(k)
          iSj= INU(kk-1) + 1
          iEj= INU(kk  )
          do j= iSj, iEj
            jj= IAU(j)
            if (IW1(jj).eq.0 .and. jj.lt.i) then
              inumFI2L(i)= inumFI2L(i)+1
                  IW1(jj)= 1
            endif  
            if (IW1(jj).eq.0 .and. jj.gt.i) then
              inumFI2U(i)= inumFI2U(i)+1
                  IW1(jj)= 1
            endif  
          enddo
        enddo
        NPLf1= NPLf1 + inumFI2L(i)
        NPUf1= NPUf1 + inumFI2U(i)
      enddo

!C
!C-- specify fill-in
      allocate (IWsL(0:NP), IWsU(0:NP))
      allocate (FI2L (NPLf1), FI2U (NPUf1))

      FI2L= 0
      FI2U= 0

      do i= 2, NP
        icouL= 0
        icouU= 0
        inumFI2L(i)= inumFI2L(i-1) + inumFI2L(i)
        inumFI2U(i)= inumFI2U(i-1) + inumFI2U(i)
        icou= 0
        IW1= 0
        IW1(i)= 1
        do L= INL(i-1)+1, INL(i)
          IW1(IAL(L))= 1
        enddo
        do L= INU(i-1)+1, INU(i)
          IW1(IAU(L))= 1
        enddo

        iSk= INL(i-1) + 1
        iEk= INL(i)
        do k= iSk, iEk
          kk= IAL(k)
          iSj= INU(kk-1) + 1
          iEj= INU(kk  )
          do j= iSj, iEj
            jj= IAU(j)
            if (IW1(jj).eq.0 .and. jj.lt.i) then
                   icouL = icouL + 1
              FI2L(icouL+inumFI2L(i-1))= jj
                  IW1(jj)= 1
            endif
            if (IW1(jj).eq.0 .and. jj.gt.i) then
                   icouU = icouU + 1
              FI2U(icouU+inumFI2U(i-1))= jj
                  IW1(jj)= 1
            endif
          enddo
        enddo
      enddo
!C=== 

!C
!C +------------------+  
!C | find fill-in (2) |
!C +------------------+
!C===
      allocate (inumFI1L(0:NP), inumFI1U(0:NP))

      NPLf2= 0
      NPUf2= 0
      inumFI1L= 0
      inumFI1U= 0
!C
!C-- count fill-in
      do i= 2, NP
        IW1= 0
        IW1(i)= 1
        do L= INL(i-1)+1, INL(i)
          IW1(IAL(L))= 2
        enddo
        do L= INU(i-1)+1, INU(i)
          IW1(IAU(L))= 2
        enddo

        do L= inumFI2L(i-1)+1, inumFI2L(i)
          IW1(FI2L(L))= 1
        enddo

        do L= inumFI2U(i-1)+1, inumFI2U(i)
          IW1(FI2U(L))= 1
        enddo

        iSk= INL(i-1) + 1
        iEk= INL(i)
        do k= iSk, iEk
          kk= IAL(k)
          iSj= inumFI2U(kk-1) + 1
          iEj= inumFI2U(kk)
          do j= iSj, iEj
            jj= FI2U(j)
            if (IW1(jj).eq.0 .and. jj.lt.i) then
              inumFI1L(i)= inumFI1L(i) + 1
                  IW1(jj)= 1
            endif  
            if (IW1(jj).eq.0 .and. jj.gt.i) then
              inumFI1U(i)= inumFI1U(i) + 1
                  IW1(jj)= 1
            endif  
          enddo
        enddo

        iSk= inumFI2L(i-1)+1
        iEk= inumFI2L(i)
        do k= iSk, iEk
          kk= FI2L(k)
          iSj= INU(kk-1) + 1
          iEj= INU(kk  )
          do j= iSj, iEj
            jj= IAU(j)
            if (IW1(jj).eq.0 .and. jj.lt.i) then
              inumFI1L(i)= inumFI1L(i) + 1
                  IW1(jj)= 1
            endif  
            if (IW1(jj).eq.0 .and. jj.gt.i) then
              inumFI1U(i)= inumFI1U(i) + 1
                  IW1(jj)= 1
            endif  
          enddo
        enddo
        NPLf2= NPLf2 + inumFI1L(i)
        NPUf2= NPUf2 + inumFI1U(i)
      enddo

!C
!C-- specify fill-in
      allocate (FI1L(NPL+NPLf1+NPLf2))
      allocate (FI1U(NPL+NPLf1+NPLf2))

      allocate (iconFI1L(NPL+NPLf1+NPLf2))
      allocate (iconFI1U(NPL+NPLf1+NPLf2))

      IWsL= 0
      IWsU= 0
      do i= 1, NP
        IWsL(i)= INL(i)-INL(i-1) + inumFI2L(i)-inumFI2L(i-1) +          &
     &                             inumFI1L(i) + IWsL(i-1)
        IWsU(i)= INU(i)-INU(i-1) + inumFI2U(i)-inumFI2U(i-1) +          &
     &                             inumFI1U(i) + IWsU(i-1)
      enddo

      do i= 2, NP
        icouL= 0
        icouU= 0
        inumFI1L(i)= inumFI1L(i-1) + inumFI1L(i)
        inumFI1U(i)= inumFI1U(i-1) + inumFI1U(i)
        icou= 0
        IW1= 0
        IW1(i)= 1
        do L= INL(i-1)+1, INL(i)
          IW1(IAL(L))= 1
        enddo
        do L= INU(i-1)+1, INU(i)
          IW1(IAU(L))= 1
        enddo

        do L= inumFI2L(i-1)+1, inumFI2L(i)
          IW1(FI2L(L))= 1
        enddo

        do L= inumFI2U(i-1)+1, inumFI2U(i)
          IW1(FI2U(L))= 1
        enddo

        iSk= INL(i-1) + 1
        iEk= INL(i)
        do k= iSk, iEk
          kk= IAL(k)
          iSj= inumFI2U(kk-1) + 1
          iEj= inumFI2U(kk  )
          do j= iSj, iEj
            jj= FI2U(j)
            if (IW1(jj).eq.0 .and. jj.lt.i) then
              iAS= INL(i)-INL(i-1)+inumFI2L(i)-inumFI2L(i-1)+IWsL(i-1)
                   icouL     = icouL + 1
              FI1L(icouL+iAS)= jj
                  IW1(jj)    = 1
            endif
            if (IW1(jj).eq.0 .and. jj.gt.i) then
              iAS= INU(i)-INU(i-1)+inumFI2U(i)-inumFI2U(i-1)+IWsU(i-1)
                   icouU     = icouU + 1
              FI1U(icouU+iAS)= jj
                  IW1(jj)    = 1
            endif
          enddo
        enddo

        iSk= inumFI2L(i-1) + 1
        iEk= inumFI2L(i)
        do k= iSk, iEk
          kk= FI2L(k)
          iSj= INU(kk-1) + 1
          iEj= INU(kk  )
          do j= iSj, iEj
            jj= IAU(j)
            if (IW1(jj).eq.0 .and. jj.lt.i) then
              iAS= INL(i)-INL(i-1)+inumFI2L(i)-inumFI2L(i-1)+IWsL(i-1)
                   icouL     = icouL + 1
              FI1L(icouL+iAS)= jj
                  IW1(jj)    = 1
            endif
            if (IW1(jj).eq.0 .and. jj.gt.i) then
              iAS= INU(i)-INU(i-1)+inumFI2U(i)-inumFI2U(i-1)+IWsU(i-1)
                   icouU     = icouU + 1
              FI1U(icouU+iAS)= jj
                  IW1(jj)    = 1
            endif
          enddo
        enddo
      enddo
!C=== 

!C
!C +-------------------------------------------------+
!C | SORT and RECONSTRUCT matrix considering fill-in |
!C +-------------------------------------------------+
!C===
      allocate (ALlu0(NB,NB,NPL+NPLf1+NPLf2))
      allocate (AUlu0(NB,NB,NPL+NPLf1+NPLf2))

      ALlu0= 0.d0
      AUlu0= 0.d0
      iSL  = 0
      iSU  = 0

      iconFI1L= 0
      iconFI1U= 0

      do i= 1, NP
        icouL1=      INL(i) -      INL(i-1)
        icouL2= inumFI2L(i) - inumFI2L(i-1) + icouL1
        icouL3= inumFI1L(i) - inumFI1L(i-1) + icouL2

        icouU1=      INU(i) -      INU(i-1)
        icouU2= inumFI2U(i) - inumFI2U(i-1) + icouU1
        icouU3= inumFI1U(i) - inumFI1U(i-1) + icouU2

!C
!C-- LOWER part
        icou= 0
        do k= INL(i-1)+1, INL(i)
              icou = icou + 1
          IW1(icou)= IAL(k)
        enddo

        icou= 0
        do k= inumFI2L(i-1)+1, inumFI2L(i)
              icou        = icou + 1
          IW1(icou+icouL1)= FI2L(k)
        enddo

        icou= 0
        do k= inumFI1L(i-1)+1, inumFI1L(i)
               icou        = icou + 1
          IW1(icou+icouL2)= FI1L(icou+icouL2+iSL)
        enddo

        do k= 1, icouL3
          IW2(k)= k
        enddo

        call fill_in_S33_SORT (IW1, IW2, icouL3, NP)

        do k= 1, icouL3
          FI1L (k+isL)= IW1(k)
          ik= IW2(k)
          if (ik.le.INL(i)-INL(i-1)) then
            ALlu0(1,1,k+isL)= AL(1,1,ik+INL(i-1))
            ALlu0(1,2,k+isL)= AL(1,2,ik+INL(i-1))
            ALlu0(1,3,k+isL)= AL(1,3,ik+INL(i-1))
            ALlu0(2,1,k+isL)= AL(2,1,ik+INL(i-1))
            ALlu0(2,2,k+isL)= AL(2,2,ik+INL(i-1))
            ALlu0(2,3,k+isL)= AL(2,3,ik+INL(i-1))
            ALlu0(3,1,k+isL)= AL(3,1,ik+INL(i-1))
            ALlu0(3,2,k+isL)= AL(3,2,ik+INL(i-1))
            ALlu0(3,3,k+isL)= AL(3,3,ik+INL(i-1))
          endif
        enddo

        icou= 0
        do k= INL(i-1)+1, INL(i)
              icou = icou + 1
          IW1(icou)= 0
        enddo

        icou= 0
        do k= inumFI2L(i-1)+1, inumFI2L(i)
              icou        = icou + 1
          IW1(icou+icouL1)= 1
        enddo

        icou= 0
        do k= inumFI1L(i-1)+1, inumFI1L(i)
               icou        = icou + 1
          IW1(icou+icouL2)= 2
        enddo

        do k= 1, icouL3
          iconFI1L(k+iSL)= IW1(IW2(k))
        enddo
!C
!C-- UPPER part
        icou= 0
        do k= INU(i-1)+1, INU(i)
              icou = icou + 1
          IW1(icou)= IAU(k)
        enddo

        icou= 0
        do k= inumFI2U(i-1)+1, inumFI2U(i)
              icou        = icou + 1
          IW1(icou+icouU1)= FI2U(k)
        enddo

        icou= 0
        do k= inumFI1U(i-1)+1, inumFI1U(i)
               icou        = icou + 1
          IW1(icou+icouU2)= FI1U(icou+icouU2+iSU)
        enddo

        do k= 1, icouU3
          IW2(k)= k
        enddo
        call fill_in_S33_SORT (IW1, IW2, icouU3, NP)

        do k= 1, icouU3
          FI1U (k+isU)= IW1(k)
          ik= IW2(k)
          if (ik.le.INU(i)-INU(i-1)) then
            AUlu0(1,1,k+isU)= AU(1,1,ik+INU(i-1))
            AUlu0(1,2,k+isU)= AU(1,2,ik+INU(i-1))
            AUlu0(1,3,k+isU)= AU(1,3,ik+INU(i-1))
            AUlu0(2,1,k+isU)= AU(2,1,ik+INU(i-1))
            AUlu0(2,2,k+isU)= AU(2,2,ik+INU(i-1))
            AUlu0(2,3,k+isU)= AU(2,3,ik+INU(i-1))
            AUlu0(3,1,k+isU)= AU(3,1,ik+INU(i-1))
            AUlu0(3,2,k+isU)= AU(3,2,ik+INU(i-1))
            AUlu0(3,3,k+isU)= AU(3,3,ik+INU(i-1))
          endif
        enddo

        icou= 0
        do k= INU(i-1)+1, INU(i)
              icou = icou + 1
          IW1(icou)= 0
        enddo

        icou= 0
        do k= inumFI2U(i-1)+1, inumFI2U(i)
              icou        = icou + 1
          IW1(icou+icouU1)= 1
        enddo

        icou= 0
        do k= inumFI1U(i-1)+1, inumFI1U(i)
               icou        = icou + 1
          IW1(icou+icouU2)= 2
        enddo

        do k= 1, icouU3
          iconFI1U(k+iSU)= IW1(IW2(k))
        enddo

        iSL= iSL + icouL3
        iSU= iSU + icouU3
      enddo
!C===
      do i= 1, NP
        inumFI1L(i)= IWsL(i)
        inumFI1U(i)= IWsU(i)
      enddo

      deallocate (IWsL, IWsU)
      deallocate (inumFI2L, inumFI2U)
      deallocate (    FI2L,     FI2U)

!C
!C +----------------------+
!C | ILU(2) factorization |
!C +----------------------+
!C===
      allocate (Dlu0(NB,NB,NP))
      Dlu0= D

      do i= 2, NP
        IW1= 0
        IW2= 0

        do k= inumFI1L(i-1)+1, inumFI1L(i)
          IW1(FI1L(k))= k
        enddo

        do k= inumFI1U(i-1)+1, inumFI1U(i)
          IW2(FI1U(k))= k
        enddo

        do kk= inumFI1L(i-1)+1, inumFI1L(i)
          k= FI1L(kk)
          iconIK= iconFI1L(kk)

          D11= Dlu0(1,1,k)
          D12= Dlu0(1,2,k)
          D13= Dlu0(1,3,k)
          D21= Dlu0(2,1,k)
          D22= Dlu0(2,2,k)
          D23= Dlu0(2,3,k)
          D31= Dlu0(3,1,k)
          D32= Dlu0(3,2,k)
          D33= Dlu0(3,3,k)

          call ILU1a33 (DkINV, D11,D12,D13,D21,D22,D23,D31,D32,D33)

          Aik(1,1)= ALlu0(1,1,kk)
          Aik(1,2)= ALlu0(1,2,kk)
          Aik(1,3)= ALlu0(1,3,kk)
          Aik(2,1)= ALlu0(2,1,kk)
          Aik(2,2)= ALlu0(2,2,kk)
          Aik(2,3)= ALlu0(2,3,kk)
          Aik(3,1)= ALlu0(3,1,kk)
          Aik(3,2)= ALlu0(3,2,kk)
          Aik(3,3)= ALlu0(3,3,kk)
  
          do jj= inumFI1U(k-1)+1, inumFI1U(k)
            j= FI1U(jj)
            iconKJ= iconFI1U(jj)

            if ((iconIK+iconKJ).lt.2) then
            Akj(1,1)= AUlu0(1,1,jj)
            Akj(1,2)= AUlu0(1,2,jj)
            Akj(1,3)= AUlu0(1,3,jj)
            Akj(2,1)= AUlu0(2,1,jj)
            Akj(2,2)= AUlu0(2,2,jj)
            Akj(2,3)= AUlu0(2,3,jj)
            Akj(3,1)= AUlu0(3,1,jj)
            Akj(3,2)= AUlu0(3,2,jj)
            Akj(3,3)= AUlu0(3,3,jj)

            call ILU1b33 (RHS_Aij, DkINV, Aik, Akj)

            if (j.eq.i) then
              Dlu0(1,1,i)= Dlu0(1,1,i) - RHS_Aij(1,1)
              Dlu0(1,2,i)= Dlu0(1,2,i) - RHS_Aij(1,2)
              Dlu0(1,3,i)= Dlu0(1,3,i) - RHS_Aij(1,3)
              Dlu0(2,1,i)= Dlu0(2,1,i) - RHS_Aij(2,1)
              Dlu0(2,2,i)= Dlu0(2,2,i) - RHS_Aij(2,2)
              Dlu0(2,3,i)= Dlu0(2,3,i) - RHS_Aij(2,3)
              Dlu0(3,1,i)= Dlu0(3,1,i) - RHS_Aij(3,1)
              Dlu0(3,2,i)= Dlu0(3,2,i) - RHS_Aij(3,2)
              Dlu0(3,3,i)= Dlu0(3,3,i) - RHS_Aij(3,3)
            endif   

            if (j.lt.i) then
              ij0= IW1(j)
              ALlu0(1,1,ij0)= ALlu0(1,1,ij0) - RHS_Aij(1,1)
              ALlu0(1,2,ij0)= ALlu0(1,2,ij0) - RHS_Aij(1,2)
              ALlu0(1,3,ij0)= ALlu0(1,3,ij0) - RHS_Aij(1,3)
              ALlu0(2,1,ij0)= ALlu0(2,1,ij0) - RHS_Aij(2,1)
              ALlu0(2,2,ij0)= ALlu0(2,2,ij0) - RHS_Aij(2,2)
              ALlu0(2,3,ij0)= ALlu0(2,3,ij0) - RHS_Aij(2,3)
              ALlu0(3,1,ij0)= ALlu0(3,1,ij0) - RHS_Aij(3,1)
              ALlu0(3,2,ij0)= ALlu0(3,2,ij0) - RHS_Aij(3,2)
              ALlu0(3,3,ij0)= ALlu0(3,3,ij0) - RHS_Aij(3,3)
            endif   

            if (j.gt.i) then
              ij0= IW2(j)
              AUlu0(1,1,ij0)= AUlu0(1,1,ij0) - RHS_Aij(1,1)
              AUlu0(1,2,ij0)= AUlu0(1,2,ij0) - RHS_Aij(1,2)
              AUlu0(1,3,ij0)= AUlu0(1,3,ij0) - RHS_Aij(1,3)
              AUlu0(2,1,ij0)= AUlu0(2,1,ij0) - RHS_Aij(2,1)
              AUlu0(2,2,ij0)= AUlu0(2,2,ij0) - RHS_Aij(2,2)
              AUlu0(2,3,ij0)= AUlu0(2,3,ij0) - RHS_Aij(2,3)
              AUlu0(3,1,ij0)= AUlu0(3,1,ij0) - RHS_Aij(3,1)
              AUlu0(3,2,ij0)= AUlu0(3,2,ij0) - RHS_Aij(3,2)
              AUlu0(3,3,ij0)= AUlu0(3,3,ij0) - RHS_Aij(3,3)
            endif   
          endif
         enddo
        enddo
      enddo

      deallocate (IW1, IW2)
!C===      
      end subroutine FORM_ILU2_33
!
!  ---------------------------------------------------------------------
!C
!C***
!C*** a_SCHWARTZ_33_BSSOR
!C***
!C
!C    additive Schwartz domain decomposition if M=A (off-diag)
!C
      subroutine a_SCHWARTZ_33_BSSOR(iterPRE, iterPREmax, R,            &
     &          indexA, indexB, COEF, N, NP, NPL, NPU, D,               &
     &          AL, INL, IAL, AU, INU, IAU, WW)
!
      integer(kind=kint ), intent(in) :: iterPRE, iterPREmax
      integer(kind=kint ), intent(in) :: indexA, indexB, R
      real   (kind=kreal), intent(in) :: COEF
!
      integer(kind=kint ), intent(in):: N, NP, NPU, NPL
      integer(kind=kint ), dimension(0:NP) ,intent(in) :: INU, INL
      integer(kind=kint ), dimension(  NPL),intent(in) :: IAL
      integer(kind=kint ), dimension(  NPU),intent(in) :: IAU
      real(kind=kreal), dimension(3,3,NP), intent(in):: D
      real(kind=kreal), dimension(3,3,NPL), intent(in):: AL
      real(kind=kreal), dimension(3,3,NPU), intent(in):: AU
!
      real(kind=kreal), dimension(3*NP,4),  intent(inout) :: WW
!
      real   (kind=kreal) :: D11, D22, D33
      real   (kind=kreal) :: X1, X2, X3
      real   (kind=kreal) :: WV1, WV2, WV3
      integer(kind=kint ) :: i, j, k
!
!
      do i= 1, NP
        WW(3*i-2,indexB)= WW(3*i-2,indexB) + WW(3*i-2,indexA)
        WW(3*i-1,indexB)= WW(3*i-1,indexB) + WW(3*i-1,indexA)
        WW(3*i  ,indexB)= WW(3*i  ,indexB) + WW(3*i  ,indexA)
      enddo

      if (iterPRE .eq. iterPREmax) return

      do j= 1, N
         X1= WW(3*j-2,indexB)
         X2= WW(3*j-1,indexB)
         X3= WW(3*j  ,indexB)
        D11= COEF * D(1,1,j)
        D22= COEF * D(2,2,j)
        D33= COEF * D(3,3,j)
        WV1= WW(3*j-2,R) - D11     *X1 - D(1,2,j)*X2 - D(1,3,j)*X3
        WV2= WW(3*j-1,R) - D(2,1,j)*X1 - D22     *X2 - D(2,3,j)*X3
        WV3= WW(3*j  ,R) - D(3,1,j)*X1 - D(3,2,j)*X2 - D33     *X3
        do k= INL(j-1)+1, INL(j)
            i= IAL(k)
           X1= WW(3*i-2,indexB)
           X2= WW(3*i-1,indexB)
           X3= WW(3*i  ,indexB)
          WV1= WV1 - AL(1,1,k)*X1 - AL(1,2,k)*X2 - AL(1,3,k)*X3
          WV2= WV2 - AL(2,1,k)*X1 - AL(2,2,k)*X2 - AL(2,3,k)*X3
          WV3= WV3 - AL(3,1,k)*X1 - AL(3,2,k)*X2 - AL(3,3,k)*X3
        enddo
        do k= INU(j-1)+1, INU(j)
            i= IAU(k)
           X1= WW(3*i-2,indexB)
           X2= WW(3*i-1,indexB)
           X3= WW(3*i  ,indexB)
          WV1= WV1 - AU(1,1,k)*X1 - AU(1,2,k)*X2 - AU(1,3,k)*X3
          WV2= WV2 - AU(2,1,k)*X1 - AU(2,2,k)*X2 - AU(2,3,k)*X3
          WV3= WV3 - AU(3,1,k)*X1 - AU(3,2,k)*X2 - AU(3,3,k)*X3
        enddo
        WW(3*j-2,indexA)= WV1
        WW(3*j-1,indexA)= WV2
        WW(3*j  ,indexA)= WV3
      enddo

      end subroutine a_SCHWARTZ_33_BSSOR
!
!  ---------------------------------------------------------------------
!
!C
!C***
!C*** fill_in_S33_SORT
!C*** 
!C
      subroutine fill_in_S33_SORT (STEM, INUM, N, NP)
!
      integer(kind = kint) :: N, NP
      integer(kind = kint), dimension(NP) :: STEM
      integer(kind = kint), dimension(NP) :: INUM
      integer(kind = kint), dimension(:), allocatable :: ISTACK
!
      integer(kind = kint) :: M, NSTACK, jstack, l, ir, ip, j, i, ii, k
      integer(kind = kint) :: ss, it, temp
!
      allocate (ISTACK(-NP:+NP)) 

      M     = 100
      NSTACK= NP

      jstack= 0
      l     = 1
      ir    = N

      ip= 0
 1    continue
      ip= ip + 1

      if (ir-l.lt.M) then
        do 12 j= l+1, ir
          ss= STEM(j)
          ii= INUM(j)

          do 11 i= j-1,1,-1
            if (STEM(i).le.ss) goto 2
            STEM(i+1)= STEM(i)
            INUM(i+1)= INUM(i)
 11       continue
          i= 0

 2        continue
            STEM(i+1)= ss
            INUM(i+1)= ii
 12     continue

        if (jstack.eq.0) then
          deallocate (ISTACK)
          return
        endif

        ir = ISTACK(jstack)
         l = ISTACK(jstack-1)
        jstack= jstack - 2
       else

        k= (l+ir) / 2
            temp = STEM(k)
        STEM(k)  = STEM(l+1)
        STEM(l+1)= temp

              it = INUM(k)
        INUM(k)  = INUM(l+1)
        INUM(l+1)= it

        if (STEM(l+1).gt.STEM(ir)) then
              temp = STEM(l+1)
          STEM(l+1)= STEM(ir)
          STEM(ir )= temp
                it = INUM(l+1)
          INUM(l+1)= INUM(ir)
          INUM(ir )= it
        endif

        if (STEM(l).gt.STEM(ir)) then
             temp = STEM(l)
          STEM(l )= STEM(ir)
          STEM(ir)= temp
               it = INUM(l)
          INUM(l )= INUM(ir)
          INUM(ir)= it
        endif

        if (STEM(l+1).gt.STEM(l)) then
              temp = STEM(l+1)
          STEM(l+1)= STEM(l)
          STEM(l  )= temp
                it = INUM(l+1)
          INUM(l+1)= INUM(l)
          INUM(l  )= it
        endif

        i= l + 1
        j= ir

        ss= STEM(l)
        ii= INUM(l)

 3      continue
          i= i + 1
          if (STEM(i).lt.ss) goto 3

 4      continue
          j= j - 1
          if (STEM(j).gt.ss) goto 4

        if (j.lt.i)        goto 5

        temp   = STEM(i)
        STEM(i)= STEM(j)
        STEM(j)= temp

        it     = INUM(i)
        INUM(i)= INUM(j)
        INUM(j)= it

        goto 3

 5      continue

        STEM(l)= STEM(j)
        STEM(j)= ss
        INUM(l)= INUM(j)
        INUM(j)= ii

        jstack= jstack + 2

        if (jstack.gt.NSTACK) then
          write (*,*) 'NSTACK overflow'
          stop
        endif

        if (ir-i+1.ge.j-1) then
          ISTACK(jstack  )= ir
          ISTACK(jstack-1)= i
          ir= j-1
         else
          ISTACK(jstack  )= j-1
          ISTACK(jstack-1)= l
          l= i
        endif

      endif

      goto 1

      end subroutine fill_in_S33_SORT
!
!  ---------------------------------------------------------------------
!C
!C***
!C*** ILU1a33
!C***
!C
!C    computes LU factorization of 3*3 Diagonal Block
!C
      subroutine ILU1a33 (ALU, D11,D12,D13,D21,D22,D23,D31,D32,D33)
!
      real(kind = kreal), intent(inout) ::  ALU(3,3)
      real(kind=kreal), intent(in) :: D11, D12, D13
      real(kind=kreal), intent(in) :: D21, D22, D23
      real(kind=kreal), intent(in) :: D31, D32, D33
!
      real(kind = kreal) :: PW(3)
      real(kind=kreal) :: ALO
      integer(kind = kint) :: k, L, i, j
!
!
      ALU(1,1)= D11
      ALU(1,2)= D12
      ALU(1,3)= D13
      ALU(2,1)= D21
      ALU(2,2)= D22
      ALU(2,3)= D23
      ALU(3,1)= D31
      ALU(3,2)= D32
      ALU(3,3)= D33

      do k= 1, 3
         L = k
        ALO= dabs(ALU(L,k))
        do i= k+1, 3
          if (dabs(ALU(i,k)).gt.ALO) then
             L = i
            ALO= dabs(ALU(L,k))
          endif
        enddo

        ALU(k,k)= 1.d0/ALU(k,k)
        do i= k+1, 3
          ALU(i,k)= ALU(i,k) * ALU(k,k)
          do j= k+1, 3
            PW(j)= ALU(i,j) - ALU(i,k)*ALU(k,j)
          enddo
          do j= k+1, 3
            ALU(i,j)= PW(j)
          enddo
        enddo
      enddo

      return
      end subroutine ILU1a33
!
!  ---------------------------------------------------------------------
!C***
!C*** ILU1b33
!C***
!C
!C    computes L_ik * D_k_INV * U_kj at ILU factorization
!C    for 3*3 Block Type Matrix
!C
!
!  ---------------------------------------------------------------------
!
      subroutine ILU1b33 (RHS_Aij, DkINV, Aik, Akj)
!
      real(kind = kreal) ::  RHS_Aij(3,3), DkINV(3,3)
      real(kind = kreal) ::  Aik(3,3), Akj(3,3)
!
      real(kind = kreal) :: X1, X2, X3
!
!C
!C-- 1st Col.
      X1= Akj(1,1)
      X2= Akj(2,1)
      X3= Akj(3,1)

        X2= X2 - DkINV(2,1)*X1
        X3= X3 - DkINV(3,1)*X1 - DkINV(3,2)*X2

        X3= DkINV(3,3)*  X3
        X2= DkINV(2,2)*( X2 - DkINV(2,3)*X3 )
        X1= DkINV(1,1)*( X1 - DkINV(1,3)*X3 - DkINV(1,2)*X2)

        RHS_Aij(1,1)=  Aik(1,1)*X1 + Aik(1,2)*X2 + Aik(1,3)*X3
        RHS_Aij(2,1)=  Aik(2,1)*X1 + Aik(2,2)*X2 + Aik(2,3)*X3
        RHS_Aij(3,1)=  Aik(3,1)*X1 + Aik(3,2)*X2 + Aik(3,3)*X3

!C
!C-- 2nd Col.
      X1= Akj(1,2)
      X2= Akj(2,2)
      X3= Akj(3,2)

        X2= X2 - DkINV(2,1)*X1
        X3= X3 - DkINV(3,1)*X1 - DkINV(3,2)*X2

        X3= DkINV(3,3)*  X3
        X2= DkINV(2,2)*( X2 - DkINV(2,3)*X3 )
        X1= DkINV(1,1)*( X1 - DkINV(1,3)*X3 - DkINV(1,2)*X2)

        RHS_Aij(1,2)=  Aik(1,1)*X1 + Aik(1,2)*X2 + Aik(1,3)*X3
        RHS_Aij(2,2)=  Aik(2,1)*X1 + Aik(2,2)*X2 + Aik(2,3)*X3
        RHS_Aij(3,2)=  Aik(3,1)*X1 + Aik(3,2)*X2 + Aik(3,3)*X3

!C
!C-- 3rd Col.
      X1= Akj(1,3)
      X2= Akj(2,3)
      X3= Akj(3,3)

        X2= X2 - DkINV(2,1)*X1
        X3= X3 - DkINV(3,1)*X1 - DkINV(3,2)*X2

        X3= DkINV(3,3)*  X3
        X2= DkINV(2,2)*( X2 - DkINV(2,3)*X3 )
        X1= DkINV(1,1)*( X1 - DkINV(1,3)*X3 - DkINV(1,2)*X2)

        RHS_Aij(1,3)=  Aik(1,1)*X1 + Aik(1,2)*X2 + Aik(1,3)*X3
        RHS_Aij(2,3)=  Aik(2,1)*X1 + Aik(2,2)*X2 + Aik(2,3)*X3
        RHS_Aij(3,3)=  Aik(3,1)*X1 + Aik(3,2)*X2 + Aik(3,3)*X3

      return
      end subroutine ILU1b33
!
!  ---------------------------------------------------------------------
!
!C***
!C*** GAUSSJ33
!C***
!
!  ---------------------------------------------------------------------
!
      subroutine GAUSSJ33(a, n, np, SOLVER_COMM)

      use calypso_mpi
!
!
      integer(kind=kint) :: SOLVER_COMM
      integer(kind=kint) :: n, np
      real(kind=kreal), dimension(np,np) :: a
!
      integer(kind=kint) :: ierr_g, ierr_l
      integer(kind=kint) :: i, icol, irow, j, k, l, ll, ierr
      integer(kind=kint), dimension(n) :: indxc, indxr, ipiv

      real(kind=kreal) :: big, dum, pivinv

      ipiv= 0

      ierr_l = 0
      do i= 1, n
        big=0.d0
        do j= 1, n
          if (ipiv(j).ne.1) then
            do k= 1, n
              if (ipiv(k).eq.0) then
                if (dabs(a(j,k)).ge.big) then
                  big = dabs(a(j,k))
                  irow= j
                  icol= k
                endif
               else if (ipiv(k).gt.1) then
                write(*,*)'singular matrix in gaussj'
                ierr_l = 1
              end if
            enddo
          endif
        enddo
!
        call MPI_allREDUCE (ierr_l, ierr_g, 1, MPI_INTEGER,             &
     &                    MPI_SUM, SOLVER_COMM, ierr)
        if(ierr_g .gt. 0) return
!
        ipiv(icol)= ipiv(icol) + 1
        if (irow.ne.icol) then
          do L= 1, n
            dum      = a(irow,L)
            a(irow,L)= a(icol,L)
            a(icol,L)= dum
          enddo
        endif

        indxr(i)= irow
        indxc(i)= icol
!
        if (a(icol,icol).eq.0.d0) then
          write(*,*) 'singular matrix in gaussj'
          ierr_l = 1
        end if
        call MPI_allREDUCE (ierr_l, ierr_g, 1, MPI_INTEGER,             &
     &                    MPI_SUM, SOLVER_COMM, ierr)
        if(ierr_g .gt. 0) return
!
        pivinv= 1.d0 / a(icol,icol)
        a(icol,icol)= 1.d0

        do L= 1, n
          a(icol,L)= a(icol,L)*pivinv
        enddo

        do LL= 1, n
          if (LL.ne.icol) then
            dum= a(LL,icol)
            a(LL,icol)=0.d0
            do L= 1, n
              a(LL,L)= a(LL,L) - a(icol,L)*dum
            enddo
          endif
        enddo
      enddo

      do L= n, 1, -1
        if (indxr(L).ne.indxc(L)) then
          do k= 1, n
            dum= a(k,indxr(L))
            a(k,indxr(L))= a(k,indxc(L))
            a(k,indxc(L))= dum
          enddo
        endif
      enddo

      return
      end subroutine GAUSSJ33
!
!  ---------------------------------------------------------------------
!
      end module solver_BLCG_3
!
