!
!C*** 
!C*** module solver_GMRES_3
!C***
!
      module solver_GMRES_3
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
!C*** GMRES_3
!C
      subroutine GMRES_3                                                &
     &                 (N, NP,  NPL, NPU,                               &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA, NREST,        &
     &                  RESID,  ITER, ERROR,                            &
     &                  my_rank, NEIBPETOT, NEIBPE,                     &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT,                       &
     &                  SOLVER_COMM , NSET)     

! \beginSUBROUTINE
!     GMRES solves the linear system Ax = b using the
!     Generalized Minimal RESidual iterative method with preconditioning.
!     3*3 Block Matrix by OVERLAPPING method.
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on Dec. 1999 (ver 3.0)
!    \end{flushright}     

      use calypso_mpi
!
      use m_constants
      use solver_SR_3
!

      integer(kind=kint ),                   intent(in   )::  N
      integer(kind=kint ),                   intent(in   )::  NP
      integer(kind=kint ),                   intent(in   )::  NPU, NPL

      real   (kind=kreal),                   intent(inout)::  RESID
      real   (kind=kreal),                   intent(in   )::  SIGMA_DIAG
      real   (kind=kreal),                   intent(in   )::  SIGMA
      integer(kind=kint ),                   intent(inout)::  ITER
      integer(kind=kint ),                   intent(inout)::  ERROR
      integer(kind=kint ),                   intent(in   )::  my_rank

      integer                              , intent(in)   :: SOLVER_COMM
      integer(kind=kint )                  , intent(in)   :: NSET

      real   (kind=kreal), dimension(3*NP) , intent(inout)::  B
      real   (kind=kreal), dimension(3*NP) , intent(inout)::  X

      real   (kind=kreal), dimension(3,3,NPL), intent(inout)::  AL
      real   (kind=kreal), dimension(3,3,NPU), intent(inout)::  AU
      real   (kind=kreal), dimension(3,3,NP ), intent(inout)::  D

      integer(kind=kint ), dimension(0:NP), intent(in) :: INU, INL
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

      real(kind=kreal), dimension(:),    allocatable, save ::  DD
      real(kind=kreal), dimension(:,:),  allocatable       ::  WW
      real(kind=kreal), dimension(:,:,:),allocatable, save :: ALU

      real   (kind=kreal), dimension(:),   allocatable :: SS
      real   (kind=kreal), dimension(:,:), allocatable :: H

      integer(kind=kint ) :: IFLAG, AV, CS, SN, R, S, V, W, Y, MAXIT
      real   (kind=kreal) :: TOL, WVAL, BB
      real   (kind=kreal), dimension(3) :: PW

      data IFLAG/0/
!C
!C-- INIT.
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 301
        return
      endif

      NRK= NREST + 4

      allocate (H (NRK,NRK))
      allocate (WW(3*NP,NRK))
      allocate (SS(NRK))

      LDH= NREST + 2
      LDW= N

      MAXIT = ITER
      TOL   = RESID

      if (IFLAG.eq.0) then
        allocate (DD   (3*NP))
        if (PRECOND(1:4).eq.'BLOC') then
          allocate (ALU(3,3,N))
        endif
        IFLAG= 1
      endif

      R  = 1
      S  = R + 1
      W  = S + 1
      Y  = W
      AV = Y
      V  = AV + 1

!C
!C-- Store the Givens parameters in matrix H.
      CS= NREST + 1
      SN= CS    + 1

      if (NSET.ge.1) then
!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
      do ik= 1, NP
        DD(3*ik-2  )= 0.d0
        DD(3*ik-1  )= 0.d0
        DD(3*ik    )= 0.d0
        WW(3*ik-2,R)= D(1,1,ik)
        WW(3*ik-1,R)= D(2,2,ik)
        WW(3*ik  ,R)= D(3,3,ik)
      enddo

        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,R))

        do ik= N+1, NP
          D(1,1,ik)= WW(3*ik-2,R)
          D(2,2,ik)= WW(3*ik-1,R)
          D(3,3,ik)= WW(3*ik  ,R)
        enddo

      if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
        ik= 1
        DD(ik*3-2)= 1.d0/(D(1,1,ik)*SIGMA_DIAG)

        W2= D(2,2,ik)*SIGMA_DIAG - D(2,1,ik)**2*DD(ik*3-2)
        DD(ik*3-1)= 1.d0/W2
        W3= D(3,3,ik)*SIGMA_DIAG - D(3,1,ik)**2*DD(ik*3-2)              &
     &                           - D(3,2,ik)**2*DD(ik*3-1)        
        DD(ik*3  )= 1.d0/W3

        do ik= 2, NP
          W1= D(1,1,ik) * SIGMA_DIAG
          W2= D(2,2,ik) * SIGMA_DIAG
          W3= D(3,3,ik) * SIGMA_DIAG

          do k= INL(ik-1)+1, INL(ik)
            in= IAL(k)
            W1= W1 - AL(1,1,k)**2*DD(3*in-2) - AL(1,2,k)**2*DD(3*in-1)  &
     &             - AL(1,3,k)**2*DD(3*in  )                          
            W2= W2 - AL(2,1,k)**2*DD(3*in-2) - AL(2,2,k)**2*DD(3*in-1)  &
     &             - AL(2,3,k)**2*DD(3*in  )                       
            W3= W3 - AL(3,1,k)**2*DD(3*in-2) - AL(3,2,k)**2*DD(3*in-1)  &
     &             - AL(3,3,k)**2*DD(3*in  )
          enddo
          DD(3*ik-2)= 1.d0/W1
          W2        = W2-D(2,1,ik)**2*DD(3*ik-2)               
          DD(3*ik-1)= 1.d0/W2
          W3        = W3-D(3,1,ik)**2*DD(3*ik-2)-D(3,2,ik)**2*DD(3*ik-1)
          DD(3*ik  )= 1.d0/W3
        enddo
      endif

      if (PRECOND(1:4).eq.'SSOR' .or. PRECOND(1:4).eq.'DIAG') then
        do ik= 1, NP
          DD(3*ik-2)= 1.d0/(D(1,1,ik)*SIGMA_DIAG)
          DD(3*ik-1)= 1.d0/(D(2,2,ik)*SIGMA_DIAG)
          DD(3*ik  )= 1.d0/(D(3,3,ik)*SIGMA_DIAG)
        enddo
      endif

      if (PRECOND(1:4).eq.'BLOC') then
        do ip= 1, N
          ALU(1,1,ip)= D(1,1,ip)
          ALU(1,2,ip)= D(1,2,ip)
          ALU(1,3,ip)= D(1,3,ip)
          ALU(2,1,ip)= D(2,1,ip)
          ALU(2,2,ip)= D(2,2,ip)
          ALU(2,3,ip)= D(2,3,ip)
          ALU(3,1,ip)= D(3,1,ip)
          ALU(3,2,ip)= D(3,2,ip)
          ALU(3,3,ip)= D(3,3,ip)

          do k= 1, 3
             L = k
            ALO= dabs(ALU(L,k,ip))
            do i= k+1, 3
              if (dabs(ALU(i,k,ip)).gt.ALO) then
                 L = i
                ALO= dabs(ALU(L,k,ip))
              endif
            enddo

            ALU(k,k,ip)= 1.d0/ALU(k,k,ip)
            do ik= k+1, 3
              ALU(ik,k,ip)= ALU(ik,k,ip) * ALU(k,k,ip)
              do j= k+1, 3
                PW(j)= ALU(ik,j,ip) - ALU(ik,k,ip)*ALU(k,j,ip)
              enddo
              do j= k+1, 3
                ALU(ik,j,ip)= PW(j)
              enddo
            enddo
          enddo
        enddo
      endif
!C===
      endif

!C
!C +-----------------------+
!C | [M]{r}= {b} - [A]{x0} |
!C +-----------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)

!C
!C-- BEGIN calculation
      do j= 1, N
           X1= X(3*j-2)
           X2= X(3*j-1)
           X3= X(3*j  )
        WVAL1= B(3*j-2) - D(1,1,j)*X1 - D(1,2,j)*X2 - D(1,3,j)*X3
        WVAL2= B(3*j-1) - D(2,1,j)*X1 - D(2,2,j)*X2 - D(2,3,j)*X3
        WVAL3= B(3*j  ) - D(3,1,j)*X1 - D(3,2,j)*X2 - D(3,3,j)*X3

        do k= INL(j-1)+1, INL(j)
          ik= IAL(k)
          X1= X(3*ik-2)
          X2= X(3*ik-1)
          X3= X(3*ik  )
          WVAL1= WVAL1 - AL(1,1,k)*X1 - AL(1,2,k)*X2 - AL(1,3,k)*X3
          WVAL2= WVAL2 - AL(2,1,k)*X1 - AL(2,2,k)*X2 - AL(2,3,k)*X3
          WVAL3= WVAL3 - AL(3,1,k)*X1 - AL(3,2,k)*X2 - AL(3,3,k)*X3
        enddo

        do k= INU(j-1)+1, INU(j)
          ik= IAU(k)
          X1= X(3*ik-2)
          X2= X(3*ik-1)
          X3= X(3*ik  )
          WVAL1= WVAL1 - AU(1,1,k)*X1 - AU(1,2,k)*X2 - AU(1,3,k)*X3
          WVAL2= WVAL2 - AU(2,1,k)*X1 - AU(2,2,k)*X2 - AU(2,3,k)*X3
          WVAL3= WVAL3 - AU(3,1,k)*X1 - AU(3,2,k)*X2 - AU(3,3,k)*X3
        enddo

        WW(3*j-2,AV)= WVAL1
        WW(3*j-1,AV)= WVAL2
        WW(3*j  ,AV)= WVAL3

      enddo

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT,NOD_EXPORT, WW(1,AV))

      do ik= 1, NP
        WW(3*ik-2,R)= WW(3*ik-2,AV)
        WW(3*ik-1,R)= WW(3*ik-1,AV)
        WW(3*ik  ,R)= WW(3*ik  ,AV)
      enddo
!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      WW(3*1-2,R)=  WW(3*1-2,R)                        *DD(3*1-2)
      WW(3*1-1,R)= (WW(3*1-1,R) - D(2,1,1)*WW(3*1-2,R))*DD(3*1-1)
      WW(3*1  ,R)= (WW(3*1  ,R) - D(3,1,1)*WW(3*1-2,R)                  &
     &                          - D(3,2,1)*WW(3*1-1,R))*DD(3*1  )

      do ik= 2, NP
        SW1= 0.0d0
        SW2= 0.0d0
        SW3= 0.0d0
        isL= INL(ik-1)+1
        ieL= INL(ik)
        do j= isL, ieL
            k= IAL(j)
           X1= WW(3*k-2,R)
           X2= WW(3*k-1,R)
           X3= WW(3*k  ,R)
          SW1= SW1 - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
          SW2= SW2 - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
          SW3= SW3 - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
        enddo

        WW(3*ik-2,R)= (WW(3*ik-2,R)+SW1) * DD(3*ik-2)
                SW2 =  SW2   - D(2,1,ik) * WW(3*ik-2,R)
        WW(3*ik-1,R)= (WW(3*ik-1,R)+SW2) * DD(3*ik-1)
                SW3 =  SW3   - D(3,1,ik) * WW(3*ik-2,R)                 &
     &                       - D(3,2,ik) * WW(3*ik-1,R)
        WW(3*ik  ,R)= (WW(3*ik  ,R)+SW3) * DD(3*ik  )
      enddo
      
      WW(3*NP  ,R)= WW(3*NP  ,R)
      WW(3*NP-1,R)= WW(3*NP-1,R)- WW(3*NP,R)  *D(2,3,NP) *DD(3*NP-1)   
      WW(3*NP-2,R)= WW(3*NP-2,R)-(WW(3*NP,R)  *D(1,3,NP) +              &
     &                            WW(3*NP-1,R)*D(1,2,NP))*DD(3*NP-2)

      do ik= NP-1, 1, -1
        isU= INU(ik-1) + 1
        ieU= INU(ik) 

        SW1= 0.d0
        SW2= 0.d0
        SW3= 0.d0
        do j= ieU, isU, -1
            k= IAU(j)
           X1= WW(3*k-2,R)
           X2= WW(3*k-1,R)
           X3= WW(3*k  ,R)
          SW1= SW1 + AU(1,1,j)*X1 + AU(1,2,j)*X2 + AU(1,3,j)*X3
          SW2= SW2 + AU(2,1,j)*X1 + AU(2,2,j)*X2 + AU(2,3,j)*X3
          SW3= SW3 + AU(3,1,j)*X1 + AU(3,2,j)*X2 + AU(3,3,j)*X3
        enddo

        WW(3*ik  ,R)= WW(3*ik  ,R)-SW3*DD(3*ik  )
               SW2  = SW2+D(2,3,ik)* WW(3*ik,R)
        WW(3*ik-1,R)= WW(3*ik-1,R)-SW2*DD(3*ik-1)
               SW1  = SW1+D(1,3,ik)* WW(3*ik  ,R)+D(1,2,ik)*WW(3*ik-1,R)
        WW(3*ik-2,R)= WW(3*ik-2,R)-SW1*DD(3*ik-2)
      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do ik= 1, N
        WW(3*ik-2,R)= WW(3*ik-2,R)*DD(3*ik-2)
        WW(3*ik-1,R)= WW(3*ik-1,R)*DD(3*ik-1)
        WW(3*ik  ,R)= WW(3*ik  ,R)*DD(3*ik  )
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do ik= 1, N
        X1= WW(3*ik-2,R)
        X2= WW(3*ik-1,R)
        X3= WW(3*ik  ,R)

        X2= X2 - ALU(2,1,ik)*X1
        X3= X3 - ALU(3,1,ik)*X1 - ALU(3,2,ik)*X2

        X3= ALU(3,3,ik)*  X3
        X2= ALU(2,2,ik)*( X2 - ALU(2,3,ik)*X3 )
        X1= ALU(1,1,ik)*( X1 - ALU(1,3,ik)*X3 - ALU(1,2,ik)*X2)

        WW(3*ik-2,R)= X1
        WW(3*ik-1,R)= X2
        WW(3*ik  ,R)= X3
      enddo
      endif      
!C===
      BNRM20= ZERO
      do ik= 1, N
        BNRM20= BNRM20+B(3*ik-2)**2+B(3*ik-1)**2+B(3*ik)**2
      enddo

      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      if (BNRM2.eq.ZERO) BNRM2= ONE
!C===
      
      ITER= 0
   10 continue

!C
!C************************************************ GMRES Iteration
!C
        I= 0
!C
!C +---------------+
!C | {v1}= {r}/|r| |
!C +---------------+
!C===
        DNRM20= ZERO
        do ik= 1, N
          DNRM20= DNRM20+WW(3*ik-2,R)**2+WW(3*ik-1,R)**2+WW(3*ik,R)**2
        enddo
        call MPI_allREDUCE (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,     &
     &                      MPI_SUM, SOLVER_COMM, ierr)

        RNORM= dsqrt(DNRM2)
        coef= ONE/RNORM
        do ik= 1, N
          WW(3*ik-2,V)= WW(3*ik-2,R) * coef
          WW(3*ik-1,V)= WW(3*ik-1,R) * coef
          WW(3*ik  ,V)= WW(3*ik  ,R) * coef
        enddo
!C===

!C
!C +--------------+
!C | {s}= |r|{e1} |
!C +--------------+
!C===
        WW(3*1-2,S) = RNORM         
        WW(3*1-1,S) = ZERO
        WW(3*1  ,S) = ZERO
        do k = 2, N
          WW(3*k-2,S) = ZERO
          WW(3*k-1,S) = ZERO
          WW(3*k  ,S) = ZERO
        enddo
!C===

!C************************************************ GMRES(m) restart
   30   continue
        
        I   = I    + 1
        ITER= ITER + 1

!C
!C +----------------+
!C | [M]{w}= [A]{v} |
!C +----------------+
!C===
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &    STACK_EXPORT, NOD_EXPORT, WW(1,V+I-1))

      do j= 1, N
           X1= WW(3*j-2,V+I-1)
           X2= WW(3*j-1,V+I-1)
           X3= WW(3*j  ,V+I-1)
        WVAL1= D(1,1,j)*X1 + D(1,2,j)*X2 + D(1,3,j)*X3
        WVAL2= D(2,1,j)*X1 + D(2,2,j)*X2 + D(2,3,j)*X3
        WVAL3= D(3,1,j)*X1 + D(3,2,j)*X2 + D(3,3,j)*X3

        do k= INL(j-1)+1, INL(j)
          ik= IAL(k)
          X1= WW(3*ik-2,V+I-1)
          X2= WW(3*ik-1,V+I-1)
          X3= WW(3*ik  ,V+I-1)
          WVAL1= WVAL1 + AL(1,1,k)*X1 + AL(1,2,k)*X2 + AL(1,3,k)*X3
          WVAL2= WVAL2 + AL(2,1,k)*X1 + AL(2,2,k)*X2 + AL(2,3,k)*X3
          WVAL3= WVAL3 + AL(3,1,k)*X1 + AL(3,2,k)*X2 + AL(3,3,k)*X3
        enddo

        do k= INU(j-1)+1, INU(j)
          ik= IAU(k)
          X1= WW(3*ik-2,V+I-1)
          X2= WW(3*ik-1,V+I-1)
          X3= WW(3*ik  ,V+I-1)
          WVAL1= WVAL1 + AU(1,1,k)*X1 + AU(1,2,k)*X2 + AU(1,3,k)*X3
          WVAL2= WVAL2 + AU(2,1,k)*X1 + AU(2,2,k)*X2 + AU(2,3,k)*X3
          WVAL3= WVAL3 + AU(3,1,k)*X1 + AU(3,2,k)*X2 + AU(3,3,k)*X3
        enddo

        WW(3*j-2,W)= WVAL1
        WW(3*j-1,W)= WVAL2
        WW(3*j  ,W)= WVAL3
      enddo

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,W))

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      WW(3*1-2,W)=  WW(3*1-2,W)                        *DD(3*1-2)
      WW(3*1-1,W)= (WW(3*1-1,W) - D(2,1,1)*WW(3*1-2,W))*DD(3*1-1)
      WW(3*1  ,W)= (WW(3*1  ,W) - D(3,1,1)*WW(3*1-2,W)                  &
     &                          - D(3,2,1)*WW(3*1-1,W))*DD(3*1  )

      do ik= 2, NP
        SW1= 0.0d0
        SW2= 0.0d0
        SW3= 0.0d0
        isL= INL(ik-1)+1
        ieL= INL(ik)
        do j= isL, ieL
            k= IAL(j)
           X1= WW(3*k-2,W)
           X2= WW(3*k-1,W)
           X3= WW(3*k  ,W)
          SW1= SW1 - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
          SW2= SW2 - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
          SW3= SW3 - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
        enddo

        WW(3*ik-2,W)= (WW(3*ik-2,W)+SW1) * DD(3*ik-2)
                SW2 =  SW2   - D(2,1,ik) * WW(3*ik-2,W)
        WW(3*ik-1,W)= (WW(3*ik-1,W)+SW2) * DD(3*ik-1)
                SW3 =  SW3   - D(3,1,ik) * WW(3*ik-2,W)                 &
     &                       - D(3,2,ik) * WW(3*ik-1,W)
        WW(3*ik  ,W)= (WW(3*ik  ,W)+SW3) * DD(3*ik  )
      enddo
      
      WW(3*NP  ,W)= WW(3*NP  ,W)
      WW(3*NP-1,W)= WW(3*NP-1,W)- WW(3*NP,W)  *D(2,3,NP) *DD(3*NP-1)   
      WW(3*NP-2,W)= WW(3*NP-2,W)-(WW(3*NP,W)  *D(1,3,NP) +              &
     &                            WW(3*NP-1,W)*D(1,2,NP))*DD(3*NP-2)

      do ik= NP-1, 1, -1
        isU= INU(ik-1) + 1
        ieU= INU(ik) 

        SW1= 0.d0
        SW2= 0.d0
        SW3= 0.d0
        do j= ieU, isU, -1
            k= IAU(j)
           X1= WW(3*k-2,W)
           X2= WW(3*k-1,W)
           X3= WW(3*k  ,W)
          SW1= SW1 + AU(1,1,j)*X1 + AU(1,2,j)*X2 + AU(1,3,j)*X3
          SW2= SW2 + AU(2,1,j)*X1 + AU(2,2,j)*X2 + AU(2,3,j)*X3
          SW3= SW3 + AU(3,1,j)*X1 + AU(3,2,j)*X2 + AU(3,3,j)*X3
        enddo

        WW(3*ik  ,W)= WW(3*ik  ,W)-SW3*DD(3*ik  )
               SW2  = SW2+D(2,3,ik)* WW(3*ik,W)
        WW(3*ik-1,W)= WW(3*ik-1,W)-SW2*DD(3*ik-1)
               SW1  = SW1+D(1,3,ik)* WW(3*ik  ,W)+D(1,2,ik)*WW(3*ik-1,W)
        WW(3*ik-2,W)= WW(3*ik-2,W)-SW1*DD(3*ik-2)
      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do ik= 1, N
        WW(3*ik-2,W)= WW(3*ik-2,W)*DD(3*ik-2)
        WW(3*ik-1,W)= WW(3*ik-1,W)*DD(3*ik-1)
        WW(3*ik  ,W)= WW(3*ik  ,W)*DD(3*ik  )
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do ik= 1, N
        X1= WW(3*ik-2,W)
        X2= WW(3*ik-1,W)
        X3= WW(3*ik  ,W)

        X2= X2 - ALU(2,1,ik)*X1
        X3= X3 - ALU(3,1,ik)*X1 - ALU(3,2,ik)*X2

        X3= ALU(3,3,ik)*  X3
        X2= ALU(2,2,ik)*( X2 - ALU(2,3,ik)*X3 )
        X1= ALU(1,1,ik)*( X1 - ALU(1,3,ik)*X3 - ALU(1,2,ik)*X2)

        WW(3*ik-2,W)= X1
        WW(3*ik-1,W)= X2
        WW(3*ik  ,W)= X3
      enddo
      endif      
!C===

!C
!C +------------------------------+
!C | ORTH. BASIS by GRAMM-SCHMIDT |
!C +------------------------------+
!C   Construct the I-th column of the upper Hessenberg matrix H
!C   using the Gram-Schmidt process on V and W.
!C===
        do K= 1, I
          VAL0= 0.d0
          do ik= 1, N
            VAL0= VAL0 + WW(3*ik-2,W)*WW(3*ik-2,V+K-1)                  &
     &                 + WW(3*ik-1,W)*WW(3*ik-1,V+K-1)                  &
     &                 + WW(3*ik  ,W)*WW(3*ik  ,V+K-1)                 

          enddo
          call MPI_allREDUCE (VAL0, VAL, 1, MPI_DOUBLE_PRECISION,       &
     &                        MPI_SUM, SOLVER_COMM, ierr)
 
          do ik= 1, N
            WW(3*ik-2,W)= WW(3*ik-2,W) - VAL * WW(3*ik-2,V+K-1)
            WW(3*ik-1,W)= WW(3*ik-1,W) - VAL * WW(3*ik-1,V+K-1)
            WW(3*ik  ,W)= WW(3*ik  ,W) - VAL * WW(3*ik  ,V+K-1)
          enddo
          H(K,I)= VAL
        enddo
        
        VAL0= 0.d0
        do ik= 1, N
          VAL0= VAL0+WW(3*ik-2,W)**2+WW(3*ik-1,W)**2+WW(3*ik,W)**2
        enddo
        call MPI_allREDUCE (VAL0, VAL, 1, MPI_DOUBLE_PRECISION,         &
     &                      MPI_SUM, SOLVER_COMM, ierr)

        H(I+1,I)= dsqrt(VAL)
        coef= ONE / H(I+1,I)
        do ik= 1, N
          WW(3*ik-2,V+I+1-1)= WW(3*ik-2,W) * coef
          WW(3*ik-1,V+I+1-1)= WW(3*ik-1,W) * coef
          WW(3*ik  ,V+I+1-1)= WW(3*ik  ,W) * coef
        enddo
!C===

!C
!C +-----------------+
!C | GIVENS ROTARION |
!C +-----------------+
!C===

!C
!C-- Plane Rotation
           do k = 1, I-1 
             VCS= H(k,CS)
             VSN= H(k,SN)
             DTEMP   = VCS*H(k  ,I) + VSN*H(k+1,I)
             H(k+1,I)= VCS*H(k+1,I) - VSN*H(k  ,I)
             H(k  ,I)= DTEMP
           enddo

!C
!C-- Construct Givens Plane Rotation
           AA = H(I  ,I)
           BB = H(I+1,I)
           R0= BB
           if (dabs(AA).gt.dabs(BB)) R0= AA
           SCALE= dabs(AA) + dabs(BB)

           if (SCALE.ne.0.d0) then
             RR= SCALE * dsqrt((AA/SCALE)**2+(BB/SCALE)**2)
             RR= dsign(1.d0,R0)*RR
             H(I,CS)= AA/RR
             H(I,SN)= BB/RR
            else
             H(I,CS)= 1.d0
             H(I,SN)= 0.d0
             RR     = 0.d0
           endif

!C
!C-- Plane Rotation
           VCS= H(I,CS)
           VSN= H(I,SN)
           DTEMP    = VCS*H(I  ,I) + VSN*H(I+1,I)
           H (I+1,I)= VCS*H(I+1,I) - VSN*H(I  ,I)
           H (I  ,I)= DTEMP

           DTEMP    = VCS*WW(I  ,S) + VSN*WW(I+1,S)
           WW(I+1,S)= VCS*WW(I+1,S) - VSN*WW(I  ,S)
           WW(I  ,S)= DTEMP

           RESID = dabs ( WW(I+1,S))/dsqrt(BNRM2)

!           if (my_rank.eq.0) write (*, '(i8, 1pe16.6)') iter, RESID

           if ( RESID.le.TOL ) then
!C-- [H]{y}= {s_tld}
              do ik= 1, I
                SS(ik)= WW(ik,S)
              enddo
              IROW= I
              WW(IROW,Y)= SS(IROW) / H(IROW,IROW)

              do kk= IROW-1, 1, -1
                do jj= IROW, kk+1, -1
                  SS(kk)= SS(kk) - H(kk,jj)*WW(jj,Y)
                enddo
                  WW(kk,Y)= SS(kk) / H(kk,kk)
              enddo

!C-- {x}= {x} + {y}{V}
              jj= IROW
              do jj= 1, IROW
              do kk= 1, N
                X(3*kk-2)= X(3*kk-2) + WW(jj,Y)*WW(3*kk-2,V+jj-1)
                X(3*kk-1)= X(3*kk-1) + WW(jj,Y)*WW(3*kk-1,V+jj-1)
                X(3*kk  )= X(3*kk  ) + WW(jj,Y)*WW(3*kk  ,V+jj-1)
              enddo
              enddo

             goto 70                     
           endif                                                         

           if ( ITER.gt.MAXIT ) goto 60
           if ( I   .lt.NREST ) goto 30
!C===

!C
!C +------------------+
!C | CURRENT SOLUTION |
!C +------------------+
!C===

!C-- [H]{y}= {s_tld}
              do ik= 1, NREST
                SS(ik)= WW(ik,S)
              enddo
              IROW= NREST
              WW(IROW,Y)= SS(IROW) / H(IROW,IROW)

              do kk= IROW-1, 1, -1
                do jj= IROW, kk+1, -1
                  SS(kk)= SS(kk) - H(kk,jj)*WW(jj,Y)
                enddo
                  WW(kk,Y)= SS(kk) / H(kk,kk)
              enddo
!C-- {x}= {x} + {y}{V}
              jj= IROW
              do jj= 1, IROW
              do kk= 1, N
                X(3*kk-2)= X(3*kk-2) + WW(jj,Y)*WW(3*kk-2,V+jj-1)
                X(3*kk-1)= X(3*kk-1) + WW(jj,Y)*WW(3*kk-1,V+jj-1)
                X(3*kk  )= X(3*kk  ) + WW(jj,Y)*WW(3*kk  ,V+jj-1)
              enddo
              enddo

!C
!C-- Compute residual vector R, find norm, then check for tolerance.        

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &    STACK_EXPORT, NOD_EXPORT, X)

      do j= 1, N
           X1= X(3*j-2)
           X2= X(3*j-1)
           X3= X(3*j  )
        WVAL1= B(3*j-2) - D(1,1,j)*X1 - D(1,2,j)*X2 - D(1,3,j)*X3
        WVAL2= B(3*j-1) - D(2,1,j)*X1 - D(2,2,j)*X2 - D(2,3,j)*X3
        WVAL3= B(3*j  ) - D(3,1,j)*X1 - D(3,2,j)*X2 - D(3,3,j)*X3

        do k= INL(j-1)+1, INL(j)
          ik= IAL(k)
          X1= X(3*ik-2)
          X2= X(3*ik-1)
          X3= X(3*ik  )
          WVAL1= WVAL1 - AL(1,1,k)*X1 - AL(1,2,k)*X2 - AL(1,3,k)*X3
          WVAL2= WVAL2 - AL(2,1,k)*X1 - AL(2,2,k)*X2 - AL(2,3,k)*X3
          WVAL3= WVAL3 - AL(3,1,k)*X1 - AL(3,2,k)*X2 - AL(3,3,k)*X3
        enddo

        do k= INU(j-1)+1, INU(j)
          ik= IAU(k)
          X1= X(3*ik-2)
          X2= X(3*ik-1)
          X3= X(3*ik  )
          WVAL1= WVAL1 - AU(1,1,k)*X1 - AU(1,2,k)*X2 - AU(1,3,k)*X3
          WVAL2= WVAL2 - AU(2,1,k)*X1 - AU(2,2,k)*X2 - AU(2,3,k)*X3
          WVAL3= WVAL3 - AU(3,1,k)*X1 - AU(3,2,k)*X2 - AU(3,3,k)*X3
        enddo

        WW(3*j-2,AV)= WVAL1
        WW(3*j-1,AV)= WVAL2
        WW(3*j  ,AV)= WVAL3
      enddo

      do ik= 1, N
        WW(3*ik-2,R)= WW(3*ik-2,AV)
        WW(3*ik-1,R)= WW(3*ik-1,AV)
        WW(3*ik  ,R)= WW(3*ik  ,AV)
      enddo

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &    STACK_EXPORT, NOD_EXPORT, WW(1,R))

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      WW(3*1-2,R)=  WW(3*1-2,R)                        *DD(3*1-2)
      WW(3*1-1,R)= (WW(3*1-1,R) - D(2,1,1)*WW(3*1-2,R))*DD(3*1-1)
      WW(3*1  ,R)= (WW(3*1  ,R) - D(3,1,1)*WW(3*1-2,R)                  &
     &                          - D(3,2,1)*WW(3*1-1,R))*DD(3*1  )

      do ik= 2, NP
        SW1= 0.0d0
        SW2= 0.0d0
        SW3= 0.0d0
        isL= INL(ik-1)+1
        ieL= INL(ik)
        do j= isL, ieL
            k= IAL(j)
           X1= WW(3*k-2,R)
           X2= WW(3*k-1,R)
           X3= WW(3*k  ,R)
          SW1= SW1 - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
          SW2= SW2 - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
          SW3= SW3 - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
        enddo

        WW(3*ik-2,R)= (WW(3*ik-2,R)+SW1) * DD(3*ik-2)
                SW2 =  SW2   - D(2,1,ik) * WW(3*ik-2,R)
        WW(3*ik-1,R)= (WW(3*ik-1,R)+SW2) * DD(3*ik-1)
                SW3 =  SW3   - D(3,1,ik) * WW(3*ik-2,R)                 &
     &                       - D(3,2,ik) * WW(3*ik-1,R)
        WW(3*ik  ,R)= (WW(3*ik  ,R)+SW3) * DD(3*ik  )
      enddo
      
      WW(3*NP  ,R)= WW(3*NP  ,R)
      WW(3*NP-1,R)= WW(3*NP-1,R)- WW(3*NP,R)  *D(2,3,NP) *DD(3*NP-1)   
      WW(3*NP-2,R)= WW(3*NP-2,R)-(WW(3*NP,R)  *D(1,3,NP) +              &
     &                            WW(3*NP-1,R)*D(1,2,NP))*DD(3*NP-2)

      do ik= NP-1, 1, -1
        isU= INU(ik-1) + 1
        ieU= INU(ik) 

        SW1= 0.d0
        SW2= 0.d0
        SW3= 0.d0
        do j= ieU, isU, -1
            k= IAU(j)
           X1= WW(3*k-2,R)
           X2= WW(3*k-1,R)
           X3= WW(3*k  ,R)
          SW1= SW1 + AU(1,1,j)*X1 + AU(1,2,j)*X2 + AU(1,3,j)*X3
          SW2= SW2 + AU(2,1,j)*X1 + AU(2,2,j)*X2 + AU(2,3,j)*X3
          SW3= SW3 + AU(3,1,j)*X1 + AU(3,2,j)*X2 + AU(3,3,j)*X3
        enddo

        WW(3*ik  ,R)= WW(3*ik  ,R)-SW3*DD(3*ik  )
               SW2  = SW2+D(2,3,ik)* WW(3*ik,R)
        WW(3*ik-1,R)= WW(3*ik-1,R)-SW2*DD(3*ik-1)
               SW1  = SW1+D(1,3,ik)* WW(3*ik  ,R)+D(1,2,ik)*WW(3*ik-1,R)
        WW(3*ik-2,R)= WW(3*ik-2,R)-SW1*DD(3*ik-2)
      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do ik= 1, N
        WW(3*ik-2,R)= WW(3*ik-2,R)*DD(3*ik-2)
        WW(3*ik-1,R)= WW(3*ik-1,R)*DD(3*ik-1)
        WW(3*ik  ,R)= WW(3*ik  ,R)*DD(3*ik  )
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do ik= 1, N
        X1= WW(3*ik-2,R)
        X2= WW(3*ik-1,R)
        X3= WW(3*ik  ,R)

        X2= X2 - ALU(2,1,ik)*X1
        X3= X3 - ALU(3,1,ik)*X1 - ALU(3,2,ik)*X2

        X3= ALU(3,3,ik)*  X3
        X2= ALU(2,2,ik)*( X2 - ALU(2,3,ik)*X3 )
        X1= ALU(1,1,ik)*( X1 - ALU(1,3,ik)*X3 - ALU(1,2,ik)*X2)

        WW(3*ik-2,R)= X1
        WW(3*ik-1,R)= X2
        WW(3*ik  ,R)= X3
      enddo
      endif      

        DNRM20= ZERO
        do ik= 1, N
          DNRM20= DNRM20+WW(3*ik-2,R)**2+WW(3*ik-1,R)**2+WW(3*ik,R)**2
        enddo

        call MPI_allREDUCE  (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,    &
     &                       MPI_SUM, SOLVER_COMM, ierr)

        WW(I+1,S)= dsqrt(DNRM2/BNRM2)
        RESID    = WW( I+1,S )

!        if ( RESID.le.TOL )   goto 70
        if ( ITER .gt.MAXIT ) goto 60
!C
!C-- RESTART
        goto 10

!C
!C-- iteration FAILED

   60 continue
      ERROR= -300
      INFO = ITER

!C-- [H]{y}= {s_tld}
              do ik= 1, I
                SS(ik)= WW(ik,S)
              enddo
              IROW= I
              WW(IROW,Y)= SS(IROW) / H(IROW,IROW)

              do kk= IROW-1, 1, -1
                do jj= IROW, kk+1, -1
                  SS(kk)= SS(kk) - H(kk,jj)*WW(jj,Y)
                enddo
                  WW(kk,Y)= SS(kk) / H(kk,kk)
              enddo

!C-- {x}= {x} + {y}{V}
              jj= IROW
              do jj= 1, IROW
              do kk= 1, N
                X(3*kk-2)= X(3*kk-2) + WW(jj,Y)*WW(3*kk-2,V+jj-1)
                X(3*kk-1)= X(3*kk-1) + WW(jj,Y)*WW(3*kk-1,V+jj-1)
                X(3*kk  )= X(3*kk  ) + WW(jj,Y)*WW(3*kk  ,V+jj-1)
              enddo
              enddo

   70 continue
!C
!C-- INTERFACE data EXCHANGE
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)

      deallocate (H)
      deallocate (WW)
      deallocate (SS)

      end subroutine        GMRES_3
      end module     solver_GMRES_3
