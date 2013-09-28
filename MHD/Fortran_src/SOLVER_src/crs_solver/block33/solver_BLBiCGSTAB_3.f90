!
!C*** 
!C*** module solver_BLBiCGSTAB_3
!C***
!
      module solver_BLBiCGSTAB_3
!
      use m_precision
!
      implicit REAL*8(A-H,O-Z)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!C
!C*** BLBiCGSTAB_3
!C
!C    BLBiCGSTAB_3 solves the linear system Ax = b with 3*3 block matrix 
!C    using the Bi-Conjugate Gradient Stabilized 
!C    iterative method with the following FULL-BLOCK TYPE preconditioners :
!C
!C      (1) Block ILU(0)
!C      (2) Block ILU(1)
!C      (3) Block ILU(2)
!C

      subroutine BLBiCGSTAB_3                                           &
     &                 (N, NP, NPL, NPU, D, AL, INL, IAL, AU, INU, IAU, &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  RESID,  ITER, ERROR, iterPREmax,                &
     &                  my_rank, NEIBPETOT, NEIBPE,                     &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT, NSET)

      use calypso_mpi
!
      use solver_SR_3
      use solver_BLCG_3
!
      use vector_calc_solver_33
      use crs_matrix_calcs_33
!
!
      integer(kind=kint ), intent(in):: N, NP, NPU, NPL, my_rank
      integer(kind=kint ), intent(in):: NEIBPETOT, iterPREmax
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

      real(kind=kreal), dimension(:,:),  allocatable       :: WW
      real(kind=kreal), dimension(:,:,:),allocatable, save :: ALU

      real   (kind=kreal), dimension(2)                :: C0, CG

      integer(kind=kint ) :: R, RT, P, PT, S, ST, T, V, MAXIT
      integer(kind=kint ) :: IFLAG
      real   (kind=kreal) :: TOL, W, SS
      data IFLAG/0/

!C
!C-- INIT.
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 301
        return
      endif

      allocate (WW(3*NP,8))

      if (IFLAG.eq.0) then
        allocate (ALU(3,3,NP))
        IFLAG= 1
      endif

       R= 1
      RT= 2
       P= 3
      PT= 4
       S= 5
      ST= 6
       T= 7
       V= 8

      MAXIT = ITER
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
     &       STACK_EXPORT, NOD_EXPORT, WW(1,1) )
        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,2) )
        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,3) )
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

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)

!C
!C-- BEGIN calculation

      call subtruct_crs_matvec_33(NP, N, NPL, NPU, INL, INU, IAL, IAU,  &
     &    D, AL, AU, WW(1,R), B, X)
!
      call copy_vector_33(NP, WW(1,RT), WW(1,R) )
      call clear_vector_solve_33(NP, WW(1,PT) )
      call clear_vector_solve_33(NP, WW(1,ST) )
!
      BNRM20= 0.d0
      do i= 1, N
        BNRM20= BNRM20+B(3*i-2)**2+B(3*i-1)**2+B(3*i)**2
      enddo

      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
      if (BNRM2.eq.0.d0) BNRM2= 1.d0

      iter= 0
!C===

!C
!C*************************************************************** iterative procedures
!C
      do iter= 1, MAXIT
!C
!C +-----------------+
!C | RHO= {r}{r_tld} |
!C +-----------------+
!C===
      RHO0= 0.d0
      do j= 1, N
        RHO0= RHO0+WW(3*j-2,RT)*WW(3*j-2,R)+WW(3*j-1,RT)*WW(3*j-1,R)    &
     &                                     +WW(3*j  ,RT)*WW(3*j  ,R)
      enddo

      call MPI_allREDUCE (RHO0, RHO, 1, MPI_DOUBLE_PRECISION,           &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
!C===

!C
!C +----------------------------------------+
!C | BETA= (RHO/RHO1) * (ALPHA/OMEGA)       |
!C | {p} = {r} + BETA * ( {p} - OMEGA*{v} ) |
!C +----------------------------------------+
!C===
      if (iter.gt.1) then
        BETA= (RHO/RHO1) * (ALPHA/OMEGA)
        do j= 1, N
          WW(3*j-2,P)= WW(3*j-2,R)+BETA*(WW(3*j-2,P)-OMEGA*WW(3*j-2,V))
          WW(3*j-1,P)= WW(3*j-1,R)+BETA*(WW(3*j-1,P)-OMEGA*WW(3*j-1,V))
          WW(3*j  ,P)= WW(3*j  ,R)+BETA*(WW(3*j  ,P)-OMEGA*WW(3*j  ,V))
        enddo
       else
        do j= 1, N
          WW(3*j-2,P)= WW(3*j-2,R)
          WW(3*j-1,P)= WW(3*j-1,R)
          WW(3*j  ,P)= WW(3*j  ,R)
        enddo
      endif
!C===

!C
!C +--------------------+
!C | {p_tld}= [Minv]{p} |
!C +--------------------+
!C===

!C
!C== Block SSOR
      if (PRECOND.eq.'BILU0') then

      do i= 1, N
        WW(3*i-2,PT)= WW(3*i-2,P)
        WW(3*i-1,PT)= WW(3*i-1,P)
        WW(3*i  ,PT)= WW(3*i  ,P)
      enddo

      do i= N+1, NP
        WW(3*i-2,PT)= 0.d0
        WW(3*i-1,PT)= 0.d0
        WW(3*i  ,PT)= 0.d0
      enddo

!C
!C-- FORWARD
        do i= 1, N
          SW1= WW(3*i-2,PT)
          SW2= WW(3*i-1,PT)
          SW3= WW(3*i  ,PT)
          isL= INL(i-1)+1
          ieL= INL(i)
          do j= isL, ieL
              k= IAL(j)
             X1= WW(3*k-2,PT)
             X2= WW(3*k-1,PT)
             X3= WW(3*k  ,PT)
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
          WW(3*i-2,PT)= X1
          WW(3*i-1,PT)= X2
          WW(3*i  ,PT)= X3
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
             X1= WW(3*k-2,PT)
             X2= WW(3*k-1,PT)
             X3= WW(3*k  ,PT)
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
          WW(3*i-2,PT)=  WW(3*i-2,PT) - X1
          WW(3*i-1,PT)=  WW(3*i-1,PT) - X2
          WW(3*i  ,PT)=  WW(3*i  ,PT) - X3
        enddo

      endif

!C
!C== Block ILU
      if (PRECOND.eq.'BILU1'.or.PRECOND.eq.'BILU2') then
      do i= 1, N
        WW(3*i-2,PT)= WW(3*i-2,P)
        WW(3*i-1,PT)= WW(3*i-1,P)
        WW(3*i  ,PT)= WW(3*i  ,P)
      enddo

        do i= 1+N, NP
          WW(3*i-2,PT)= 0.d0
          WW(3*i-1,PT)= 0.d0
          WW(3*i  ,PT)= 0.d0
        enddo

!C
!C-- FORWARD
        do i= 1, N
          SW1= WW(3*i-2,PT)
          SW2= WW(3*i-1,PT)
          SW3= WW(3*i  ,PT)
          isL= inumFI1L(i-1)+1
          ieL= inumFI1L(i)
          do j= isL, ieL
              k= FI1L(j)
             X1= WW(3*k-2,PT)
             X2= WW(3*k-1,PT)
             X3= WW(3*k  ,PT)
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
          WW(3*i-2,PT)=  X1
          WW(3*i-1,PT)=  X2
          WW(3*i  ,PT)=  X3
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
             X1= WW(3*k-2,PT)
             X2= WW(3*k-1,PT)
             X3= WW(3*k  ,PT)
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
          WW(3*i-2,PT)=  WW(3*i-2,PT) - X1
          WW(3*i-1,PT)=  WW(3*i-1,PT) - X2
          WW(3*i  ,PT)=  WW(3*i  ,PT) - X3
        enddo
      endif
!C==
!C===

!C
!C +-------------------+
!C | {v} = [A] {p_tld} |
!C +-------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,PT) )

!C
        call cal_crs_matvec_33 (NP, N, NPL, NPU, INL, INU, IAL, IAU,    &
     &      D, AL, AU, WW(1,V), WW(1,PT))
!C===

!C
!C-- calc. ALPHA

      C20= 0.d0
      do j= 1, N
        C20= C20 + WW(3*j-2,RT)*WW(3*j-2,V) + WW(3*j-1,RT)*WW(3*j-1,V)  &
     &                                      + WW(3*j  ,RT)*WW(3*j  ,V)
      enddo

      call MPI_allREDUCE (C20, C2, 1, MPI_DOUBLE_PRECISION,             &
     &                    MPI_SUM, CALYPSO_COMM, ierr) 
      ALPHA= RHO / C2

!C
!C-- {s}= {r} - ALPHA*{V}
      do j= 1, N
        WW(3*j-2,S)= WW(3*j-2,R) - ALPHA*WW(3*j-2,V)
        WW(3*j-1,S)= WW(3*j-1,R) - ALPHA*WW(3*j-1,V)
        WW(3*j  ,S)= WW(3*j  ,R) - ALPHA*WW(3*j  ,V)
      enddo

!C
!C +--------------------+
!C | {s_tld}= [Minv]{s} |
!C +--------------------+
!C===

!C
!C== Block SSOR
      if (PRECOND.eq.'BILU0') then

      do i= 1, N
        WW(3*i-2,ST)= WW(3*i-2,S)
        WW(3*i-1,ST)= WW(3*i-1,S)
        WW(3*i  ,ST)= WW(3*i  ,S)
      enddo

      do i= N+1, NP
        WW(3*i-2,ST)= 0.d0
        WW(3*i-1,ST)= 0.d0
        WW(3*i  ,ST)= 0.d0
      enddo
!C
!C-- FORWARD
        do i= 1, N
          SW1= WW(3*i-2,ST)
          SW2= WW(3*i-1,ST)
          SW3= WW(3*i  ,ST)
          isL= INL(i-1)+1
          ieL= INL(i)
          do j= isL, ieL
              k= IAL(j)
             X1= WW(3*k-2,ST)
             X2= WW(3*k-1,ST)
             X3= WW(3*k  ,ST)
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
          WW(3*i-2,ST)= X1
          WW(3*i-1,ST)= X2
          WW(3*i  ,ST)= X3
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
             X1= WW(3*k-2,ST)
             X2= WW(3*k-1,ST)
             X3= WW(3*k  ,ST)
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
          WW(3*i-2,ST)=  WW(3*i-2,ST) - X1
          WW(3*i-1,ST)=  WW(3*i-1,ST) - X2
          WW(3*i  ,ST)=  WW(3*i  ,ST) - X3
        enddo
      endif

!C
!C== Block ILU
      if (PRECOND.eq.'BILU1'.or.PRECOND.eq.'BILU2') then
      do i= 1, N
        WW(3*i-2,ST)= WW(3*i-2,S)
        WW(3*i-1,ST)= WW(3*i-1,S)
        WW(3*i  ,ST)= WW(3*i  ,S)
      enddo

      do i= N+1, NP
        WW(3*i-2,ST)= 0.d0
        WW(3*i-1,ST)= 0.d0
        WW(3*i  ,ST)= 0.d0
      enddo

!C
!C-- FORWARD
        do i= 1, N
          SW1= WW(3*i-2,ST)
          SW2= WW(3*i-1,ST)
          SW3= WW(3*i  ,ST)
          isL= inumFI1L(i-1)+1
          ieL= inumFI1L(i)
          do j= isL, ieL
              k= FI1L(j)
             X1= WW(3*k-2,ST)
             X2= WW(3*k-1,ST)
             X3= WW(3*k  ,ST)
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
          WW(3*i-2,ST)=  X1
          WW(3*i-1,ST)=  X2
          WW(3*i  ,ST)=  X3
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
             X1= WW(3*k-2,ST)
             X2= WW(3*k-1,ST)
             X3= WW(3*k  ,ST)
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
          WW(3*i-2,ST)=  WW(3*i-2,ST) - X1
          WW(3*i-1,ST)=  WW(3*i-1,ST) - X2
          WW(3*i  ,ST)=  WW(3*i  ,ST) - X3
        enddo
      endif
!C==
!C===

!C
!C +------------------+
!C | {t} = [A]{s_tld} |
!C +------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,ST) )

!C
        call cal_crs_matvec_33(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      D, AL, AU, WW(1,T), WW(1,ST) )
!C===

!C
!C +----------------------------+
!C | OMEGA= ({t}{s}) / ({t}{t}) |
!C +----------------------------+
!C===
      C0(1)= 0.d0
      C0(2)= 0.d0
      CG(1)= 0.d0
      CG(2)= 0.d0

      do j= 1, N
        C0(1)= C0(1) + WW(3*j-2,T)*WW(3*j-2,S) + WW(3*j-1,T)*WW(3*j-1,S)&
     &                                         + WW(3*j  ,T)*WW(3*j  ,S)
        C0(2)= C0(2) + WW(3*j-2,T)*WW(3*j-2,T) + WW(3*j-1,T)*WW(3*j-1,T)&
     &                                         + WW(3*j  ,T)*WW(3*j  ,T)
      enddo

      call MPI_allREDUCE (C0, CG, 2, MPI_DOUBLE_PRECISION,              &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
      OMEGA= CG(1) / CG(2)
!C===

!C
!C +----------------+
!C | update {x},{r} |
!C +----------------+
!C===
      DNRM20= 0.d0
      do j= 1, N
        X (3*j-2)= X(3*j-2) + ALPHA*WW(3*j-2,PT) + OMEGA*WW(3*j-2,ST)
        X (3*j-1)= X(3*j-1) + ALPHA*WW(3*j-1,PT) + OMEGA*WW(3*j-1,ST)
        X (3*j  )= X(3*j  ) + ALPHA*WW(3*j  ,PT) + OMEGA*WW(3*j  ,ST)
        WW(3*j-2,R)= WW(3*j-2,S) - OMEGA*WW(3*j-2,T)
        WW(3*j-1,R)= WW(3*j-1,S) - OMEGA*WW(3*j-1,T)
        WW(3*j  ,R)= WW(3*j  ,S) - OMEGA*WW(3*j  ,T)
        DNRM20= DNRM20+WW(3*j-2,S)**2+WW(3*j-1,S)**2+WW(3*j,S)**2
      enddo

      RHO1= RHO

      call MPI_allREDUCE  (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,      &
     &                     MPI_SUM, CALYPSO_COMM, ierr)
      RESID= dsqrt(DNRM2/BNRM2)

!C##### ITERATION HISTORY
!        if (my_rank.eq.0) write (*, 1000) ITER, RESID
! 1000   format (i5, 1pe16.6)
!C#####

      if (RESID.le.TOL   ) exit
      if ( ITER.eq.MAXIT ) ERROR= -300
!C===

      enddo

!C
!C-- INTERFACE data EXCHANGE
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)

      deallocate (WW)

      end subroutine        BLBiCGSTAB_3
      end module     solver_BLBiCGSTAB_3

