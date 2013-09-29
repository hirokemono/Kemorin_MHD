!
!C*** 
!C*** module solver_GPBiCG_3
!C***
!
      module solver_GPBiCG_3
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
!C*** GPBiCG_3
!C
      subroutine GPBiCG_3                                               &
     &                 (N, NP,  NPL, NPU,                               &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  RESID,  ITER, ERROR, NEIBPETOT, NEIBPE,         &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT, NSET)

! \beginSUBROUTINE
!     GPBiCG_3 solves the linear system Ax = b using the
!     GPBiCG iterative method with preconditioning.
!     3*3 Block Matrix by OVERLAPPING method.
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on Dec.1999 (ver 3.0)
!    \end{flushright}     

      use calypso_mpi
!
      use  solver_SR_3
!

      integer(kind=kint ),                   intent(in   )::  N
      integer(kind=kint ),                   intent(in   )::  NP
      integer(kind=kint ),                   intent(in   )::  NPU, NPL

      real   (kind=kreal),                   intent(inout)::  RESID
      real   (kind=kreal),                   intent(in   )::  SIGMA_DIAG
      real   (kind=kreal),                   intent(in   )::  SIGMA
      integer(kind=kint ),                   intent(inout)::  ITER
      integer(kind=kint ),                   intent(inout)::  ERROR

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

      real(kind=kreal), dimension(5) :: C0, CG
      real(kind=kreal), dimension(2) :: EQ

      integer(kind=kint ) :: IFLAG, R, RT, T, T0, TT, P, PT
      integer(kind=kint ) ::  U, W1, Y, Z, WK, W2, MAXIT
      real   (kind=kreal) :: TOL, W, SS
      real   (kind=kreal), dimension(3) :: PW
      data IFLAG/0/

!C
!C-- INIT.
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 301
        return
      endif

      allocate (WW(3*NP,13))

      if (IFLAG.eq.0) then
        allocate (DD   (3*NP))
        if (PRECOND(1:4).eq.'BLOC') then
          allocate (ALU(3,3,N))
        endif
        IFLAG= 1
      endif

      MAXIT = ITER
      TOL   = RESID

      ERROR= 0

       R= 1
      RT= 2
       T= 3
      TT= 4
      T0= 5
       P= 6
      PT= 7
       U= 8
      W1= 9
       Y=10
       Z=11
      WK=12
      W2=13

      if (NSET.ge.1) then
!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
      do i= 1, NP
        DD(3*i-2)= 0.d0
        DD(3*i-1)= 0.d0
        DD(3*i  )= 0.d0
        WW(3*i-2,R)= D(1,1,i)
        WW(3*i-1,R)= D(2,2,i)
        WW(3*i  ,R)= D(3,3,i)
      enddo

        call SOLVER_SEND_RECV_3                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,R) )
        do i= N+1, NP
          D(1,1,i)= WW(3*i-2,R)
          D(2,2,i)= WW(3*i-1,R)
          D(3,3,i)= WW(3*i  ,R)
        enddo

      if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
        i= 1
        DD(i*3-2)= 1.d0/(D(1,1,i)*SIGMA_DIAG)

        SW2= D(2,2,i)*SIGMA_DIAG - D(2,1,i)**2*DD(i*3-2)
        DD(i*3-1)= 1.d0/SW2
        SW3= D(3,3,i)*SIGMA_DIAG - D(3,1,i)**2*DD(i*3-2)                &
     &                           - D(3,2,i)**2*DD(i*3-1)        
        DD(i*3  )= 1.d0/SW3

        do i= 2, NP
          SW1= D(1,1,i) * SIGMA_DIAG
          SW2= D(2,2,i) * SIGMA_DIAG
          SW3= D(3,3,i) * SIGMA_DIAG

          do k= INL(i-1)+1, INL(i)
            in= IAL(k)
            SW1= SW1 - AL(1,1,k)**2*DD(3*in-2) - AL(1,2,k)**2*DD(3*in-1)&
     &               - AL(1,3,k)**2*DD(3*in  )                          
            SW2= SW2 - AL(2,1,k)**2*DD(3*in-2) - AL(2,2,k)**2*DD(3*in-1)&
     &               - AL(2,3,k)**2*DD(3*in  )                       
            SW3= SW3 - AL(3,1,k)**2*DD(3*in-2) - AL(3,2,k)**2*DD(3*in-1)&
     &               - AL(3,3,k)**2*DD(3*in  )
          enddo
          DD(3*i-2)= 1.d0/SW1
          SW2      = SW2 - D(2,1,i)**2*DD(3*i-2)               
          DD(3*i-1)= 1.d0/SW2
          SW3      = SW3 - D(3,1,i)**2*DD(3*i-2) - D(3,2,i)**2*DD(3*i-1)
          DD(3*i  )= 1.d0/SW3
        enddo
      endif

      if (PRECOND(1:4).eq.'SSOR' .or. PRECOND(1:4).eq.'DIAG') then
        do i= 1, NP
          DD(i*3-2)= 1.d0/(D(1,1,i)*SIGMA_DIAG)
          DD(i*3-1)= 1.d0/(D(2,2,i)*SIGMA_DIAG)
          DD(i*3  )= 1.d0/(D(3,3,i)*SIGMA_DIAG)
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
            do i= k+1, 3
              ALU(i,k,ip)= ALU(i,k,ip) * ALU(k,k,ip)
              do j= k+1, 3
                PW(j)= ALU(i,j,ip) - ALU(i,k,ip)*ALU(k,j,ip)
              enddo
              do j= k+1, 3
                ALU(i,j,ip)= PW(j)
              enddo
            enddo
          enddo
        enddo
      endif
!C===
      endif

!C
!C +----------------------+
!C | {r}= {b} - [A]{xini} |
!C +----------------------+
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
          i= IAL(k)
          X1= X(3*i-2)
          X2= X(3*i-1)
          X3= X(3*i  )
          WVAL1= WVAL1 - AL(1,1,k)*X1 - AL(1,2,k)*X2 - AL(1,3,k)*X3
          WVAL2= WVAL2 - AL(2,1,k)*X1 - AL(2,2,k)*X2 - AL(2,3,k)*X3
          WVAL3= WVAL3 - AL(3,1,k)*X1 - AL(3,2,k)*X2 - AL(3,3,k)*X3
        enddo

        do k= INU(j-1)+1, INU(j)
          i= IAU(k)
          X1= X(3*i-2)
          X2= X(3*i-1)
          X3= X(3*i  )
          WVAL1= WVAL1 - AU(1,1,k)*X1 - AU(1,2,k)*X2 - AU(1,3,k)*X3
          WVAL2= WVAL2 - AU(2,1,k)*X1 - AU(2,2,k)*X2 - AU(2,3,k)*X3
          WVAL3= WVAL3 - AU(3,1,k)*X1 - AU(3,2,k)*X2 - AU(3,3,k)*X3
        enddo

        WW(3*j-2,R )= WVAL1
        WW(3*j-1,R )= WVAL2
        WW(3*j  ,R )= WVAL3
        WW(3*j-2,RT)= WVAL1
        WW(3*j-1,RT)= WVAL2
        WW(3*j  ,RT)= WVAL3

        WW(3*j-2,T)= 0.d0
        WW(3*j-1,T)= 0.d0
        WW(3*j  ,T)= 0.d0
        WW(3*j-2,T0)= 0.d0
        WW(3*j-1,T0)= 0.d0
        WW(3*j  ,T0)= 0.d0
        WW(3*j-2,W1)= 0.d0
        WW(3*j-1,W1)= 0.d0
        WW(3*j  ,W1)= 0.d0
      enddo


      BNRM20= 0.d0
      RHO0  = 0.0d0
      do i= 1, N
        BNRM20= BNRM20+B(3*i-2)**2+B(3*i-1)**2+B(3*i)**2
        RHO0  = RHO0 + WW(3*i-2,RT)*WW(3*i-2,R)+WW(3*i-1,RT)*WW(3*i-1,R)&
     &                                         +WW(3*i  ,RT)*WW(3*i  ,R)
      enddo

      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
      call MPI_allREDUCE (RHO0  , RHO,   1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr)

      if (BNRM2.eq.0.d0) BNRM2= 1.d0      
!C===

!C
!C*************************************************************** ITERATIVE PROC.
!C
      do iter= 1, MAXIT
!C
!C-- INIT.
      do j= 1, N
        WW(3*j-2,WK)=  WW(3*j-2,R)
        WW(3*j-1,WK)=  WW(3*j-1,R)
        WW(3*j  ,WK)=  WW(3*j  ,R)
      enddo

!C
!C +----------------+
!C | {r}= [Minv]{r} |
!C +----------------+
!C===

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,R) )

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      WW(3*1-2,R)=  WW(3*1-2,R)                        *DD(3*1-2)
      WW(3*1-1,R)= (WW(3*1-1,R) - D(2,1,1)*WW(3*1-2,R))*DD(3*1-1)
      WW(3*1  ,R)= (WW(3*1  ,R) - D(3,1,1)*WW(3*1-2,R)                  &
     &                          - D(3,2,1)*WW(3*1-1,R))*DD(3*1  )

      do i= 2, NP
        SW1= 0.0d0
        SW2= 0.0d0
        SW3= 0.0d0
        isL= INL(i-1)+1
        ieL= INL(i)
        do j= isL, ieL
            k= IAL(j)
           X1= WW(3*k-2,R)
           X2= WW(3*k-1,R)
           X3= WW(3*k  ,R)
          SW1= SW1 - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
          SW2= SW2 - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
          SW3= SW3 - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
        enddo

        WW(3*i-2,R)= (WW(3*i-2,R)+SW1) * DD(3*i-2)
               SW2 =  SW2   - D(2,1,i) * WW(3*i-2,R)
        WW(3*i-1,R)= (WW(3*i-1,R)+SW2) * DD(3*i-1)
               SW3 =  SW3   - D(3,1,i) * WW(3*i-2,R)                    &
     &                      - D(3,2,i) * WW(3*i-1,R)
        WW(3*i  ,R)= (WW(3*i  ,R)+SW3) * DD(3*i  )
      enddo
      
      WW(3*NP  ,R)= WW(3*NP  ,R)
      WW(3*NP-1,R)= WW(3*NP-1,R)- WW(3*NP  ,R)*D(2,3,NP) *DD(3*NP-1)   
      WW(3*NP-2,R)= WW(3*NP-2,R)-(WW(3*NP  ,R)*D(1,3,NP) +              &
     &                            WW(3*NP-1,R)*D(1,2,NP))*DD(3*NP-2)

      do i= NP-1, 1, -1
        isU= INU(i-1) + 1
        ieU= INU(i) 

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

        WW(3*i  ,R)= WW(3*i  ,R)-SW3*DD(3*i  )
               SW2 = SW2 + D(2,3,i)* WW(3*i,R)
        WW(3*i-1,R)= WW(3*i-1,R)-SW2*DD(3*i-1)
               SW1 = SW1 + D(1,3,i)* WW(3*i,R)+D(1,2,i)*WW(3*i-1,R)
        WW(3*i-2,R)= WW(3*i-2,R)-SW1*DD(3*i-2)
      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        WW(3*i-2,R)= WW(3*i-2,R)*DD(3*i-2)
        WW(3*i-1,R)= WW(3*i-1,R)*DD(3*i-1)
        WW(3*i  ,R)= WW(3*i  ,R)*DD(3*i  )
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do i= 1, N
        X1= WW(3*i-2,R)
        X2= WW(3*i-1,R)
        X3= WW(3*i  ,R)

        X2= X2 - ALU(2,1,i)*X1
        X3= X3 - ALU(3,1,i)*X1 - ALU(3,2,i)*X2

        X3= ALU(3,3,i)*  X3
        X2= ALU(2,2,i)*( X2 - ALU(2,3,i)*X3 )
        X1= ALU(1,1,i)*( X1 - ALU(1,3,i)*X3 - ALU(1,2,i)*X2)

        WW(3*i-2,R)=  X1
        WW(3*i-1,R)=  X2
        WW(3*i  ,R)=  X3
      enddo
      endif      
!C===

!C
!C +----------------------------------+
!C | {p} = {r} + BETA * ( {p} - {u} ) |
!C +----------------------------------+
!C===
      if (iter.gt.1) then
        do j= 1, N
          WW(3*j-2,P)= WW(3*j-2,R) + BETA*( WW(3*j-2,P)-WW(3*j-2,U))
          WW(3*j-1,P)= WW(3*j-1,R) + BETA*( WW(3*j-1,P)-WW(3*j-1,U))
          WW(3*j  ,P)= WW(3*j  ,R) + BETA*( WW(3*j  ,P)-WW(3*j  ,U))
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
!C +--------------------------------+
!C | ALPHA= {r_tld}{r}/{r_tld} A{p} |
!C +--------------------------------+
!C===

!C
!C-- calc. {p_tld}= A{p}

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,P) )

      do j= 1, N
           X1= WW(3*j-2,P)
           X2= WW(3*j-1,P)
           X3= WW(3*j  ,P)
        WVAL1= D(1,1,j)*X1 + D(1,2,j)*X2 + D(1,3,j)*X3
        WVAL2= D(2,1,j)*X1 + D(2,2,j)*X2 + D(2,3,j)*X3
        WVAL3= D(3,1,j)*X1 + D(3,2,j)*X2 + D(3,3,j)*X3

        do k= INL(j-1)+1, INL(j)
           i= IAL(k)
          X1= WW(3*i-2,P)
          X2= WW(3*i-1,P)
          X3= WW(3*i  ,P)
          WVAL1= WVAL1 + AL(1,1,k)*X1 + AL(1,2,k)*X2 + AL(1,3,k)*X3
          WVAL2= WVAL2 + AL(2,1,k)*X1 + AL(2,2,k)*X2 + AL(2,3,k)*X3
          WVAL3= WVAL3 + AL(3,1,k)*X1 + AL(3,2,k)*X2 + AL(3,3,k)*X3
        enddo

        do k= INU(j-1)+1, INU(j)
           i= IAU(k)
          X1= WW(3*i-2,P)
          X2= WW(3*i-1,P)
          X3= WW(3*i  ,P)
          WVAL1= WVAL1 + AU(1,1,k)*X1 + AU(1,2,k)*X2 + AU(1,3,k)*X3
          WVAL2= WVAL2 + AU(2,1,k)*X1 + AU(2,2,k)*X2 + AU(2,3,k)*X3
          WVAL3= WVAL3 + AU(3,1,k)*X1 + AU(3,2,k)*X2 + AU(3,3,k)*X3
        enddo

        WW(3*j-2,PT)= WVAL1
        WW(3*j-1,PT)= WVAL2
        WW(3*j  ,PT)= WVAL3
      enddo

!C
!C-- calc. ALPHA

      RHO10= 0.d0
      do j= 1, N
        RHO10= RHO10+WW(3*j-2,RT)*WW(3*j-2,PT)+WW(3*j-1,RT)*WW(3*j-1,PT)&
     &                                        +WW(3*j  ,RT)*WW(3*j  ,PT)
      enddo

      call MPI_allREDUCE (RHO10, RHO1, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr)

      ALPHA= RHO / RHO1
!C===

!C
!C +------------------------------------------+
!C | {y}= {t} - {r} - ALPHA{w} + ALPHA{p_tld} |
!C | {t}= {r}                  - ALPHA{p_tld} |
!C +------------------------------------------+
!C===
      do j= 1, N
        WW(3*j-2,Y)= WW(3*j-2,T) - WW(3*j-2,WK)                         &
     &                   + ALPHA*(-WW(3*j-2,W1) + WW(3*j-2,PT))
        WW(3*j-1,Y)= WW(3*j-1,T) - WW(3*j-1,WK)                         &
     &                   + ALPHA*(-WW(3*j-1,W1) + WW(3*j-1,PT))
        WW(3*j  ,Y)= WW(3*j  ,T) - WW(3*j  ,WK)                         &
     &                   + ALPHA*(-WW(3*j  ,W1) + WW(3*j  ,PT))

        WW(3*j-2,T)= WW(3*j-2,WK) - ALPHA*WW(3*j-2,PT)
        WW(3*j-1,T)= WW(3*j-1,WK) - ALPHA*WW(3*j-1,PT)
        WW(3*j  ,T)= WW(3*j  ,WK) - ALPHA*WW(3*j  ,PT)
      enddo
!C===

!C
!C +-----------------------+
!C | {t_tld}= [A][Minv]{t} |
!C +-----------------------+
!C===

!C
!C-- calc. {t_tld} and {t0} by [M] inversion
!C         {W2}   = [Minv]{p_tld} 
!C

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,PT) )

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,T) )

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,T0) )

      do i= 1, NP
        WW(3*i-2,TT)= WW(3*i-2,T)
        WW(3*i-1,TT)= WW(3*i-1,T)
        WW(3*i  ,TT)= WW(3*i  ,T)
        WW(3*i-2,W2)= WW(3*i-2,PT)
        WW(3*i-1,W2)= WW(3*i-1,PT)
        WW(3*i  ,W2)= WW(3*i  ,PT)
      enddo

!C
!C-- incomplete CHOLESKY

      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      WW(3*1-2,TT)=  WW(3*1-2,TT)                         *DD(3*1-2)
      WW(3*1-1,TT)= (WW(3*1-1,TT) - D(2,1,1)*WW(3*1-2,TT))*DD(3*1-1)
      WW(3*1  ,TT)= (WW(3*1  ,TT) - D(3,1,1)*WW(3*1-2,TT)               &
     &                            - D(3,2,1)*WW(3*1-1,TT))*DD(3*1  )

      WW(3*1-2,W2)=  WW(3*1-2,W2)                         *DD(3*1-2)
      WW(3*1-1,W2)= (WW(3*1-1,W2) - D(2,1,1)*WW(3*1-2,W2))*DD(3*1-1)
      WW(3*1  ,W2)= (WW(3*1  ,W2) - D(3,1,1)*WW(3*1-2,W2)               &
     &                            - D(3,2,1)*WW(3*1-1,W2))*DD(3*1  )

      WW(3*1-2,T0)=  WW(3*1-2,T0)                         *DD(3*1-2)
      WW(3*1-1,T0)= (WW(3*1-1,T0) - D(2,1,1)*WW(3*1-2,T0))*DD(3*1-1)
      WW(3*1  ,T0)= (WW(3*1  ,T0) - D(3,1,1)*WW(3*1-2,T0)               &
     &                            - D(3,2,1)*WW(3*1-1,T0))*DD(3*1  )

      do i= 2, NP
        SW1_TT= 0.0d0
        SW2_TT= 0.0d0
        SW3_TT= 0.0d0
        SW1_W2= 0.0d0
        SW2_W2= 0.0d0
        SW3_W2= 0.0d0
        SW1_T0= 0.0d0
        SW2_T0= 0.0d0
        SW3_T0= 0.0d0
        isL= INL(i-1)+1
        ieL= INL(i)
        do j= isL, ieL
            k= IAL(j)
              X1= WW(3*k-2,TT)
              X2= WW(3*k-1,TT)
              X3= WW(3*k  ,TT)
          SW1_TT= SW1_TT - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
          SW2_TT= SW2_TT - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
          SW3_TT= SW3_TT - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
              X1= WW(3*k-2,W2)
              X2= WW(3*k-1,W2)
              X3= WW(3*k  ,W2)
          SW1_W2= SW1_W2 - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
          SW2_W2= SW2_W2 - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
          SW3_W2= SW3_W2 - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
              X1= WW(3*k-2,T0)
              X2= WW(3*k-1,T0)
              X3= WW(3*k  ,T0)
          SW1_T0= SW1_T0 - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
          SW2_T0= SW2_T0 - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
          SW3_T0= SW3_T0 - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
        enddo

        WW(3*i-2,TT)= (WW(3*i-2,TT)+SW1_TT) * DD(3*i-2)
             SW2_TT = SW2_TT -  D(2,1,i) * WW(3*i-2,TT)
        WW(3*i-1,TT)= (WW(3*i-1,TT)+SW2_TT) * DD(3*i-1)
             SW3_TT = SW3_TT -  D(3,1,i) * WW(3*i-2,TT)                 &
     &                      -   D(3,2,i) * WW(3*i-1,TT)
        WW(3*i  ,TT)= (WW(3*i  ,TT)+SW3_TT) * DD(3*i  )

        WW(3*i-2,W2)= (WW(3*i-2,W2)+SW1_W2) * DD(3*i-2)
             SW2_W2 = SW2_W2 -  D(2,1,i) * WW(3*i-2,W2)
        WW(3*i-1,W2)= (WW(3*i-1,W2)+SW2_W2) * DD(3*i-1)
             SW3_W2 = SW3_W2 -  D(3,1,i) * WW(3*i-2,W2)                 &
     &                      -   D(3,2,i) * WW(3*i-1,W2)
        WW(3*i  ,W2)= (WW(3*i  ,W2)+SW3_W2) * DD(3*i  )

        WW(3*i-2,T0)= (WW(3*i-2,T0)+SW1_T0) * DD(3*i-2)
             SW2_T0 = SW2_T0 -  D(2,1,i) * WW(3*i-2,T0)
        WW(3*i-1,T0)= (WW(3*i-1,T0)+SW2_T0) * DD(3*i-1)
             SW3_T0 = SW3_T0 -  D(3,1,i) * WW(3*i-2,T0)                 &
     &                      -   D(3,2,i) * WW(3*i-1,T0)
        WW(3*i  ,T0)= (WW(3*i  ,T0)+SW3_T0) * DD(3*i  )
      enddo
      
      WW(3*NP  ,TT)= WW(3*NP  ,TT)
      WW(3*NP-1,TT)= WW(3*NP-1,TT)- WW(3*NP  ,TT)*D(2,3,NP) *DD(3*NP-1)   
      WW(3*NP-2,TT)= WW(3*NP-2,TT)-(WW(3*NP  ,TT)*D(1,3,NP) +              &
     &                              WW(3*NP-1,TT)*D(1,2,NP))*DD(3*NP-2)

      WW(3*NP  ,W2)= WW(3*NP  ,W2)
      WW(3*NP-1,W2)= WW(3*NP-1,W2)- WW(3*NP  ,W2)*D(2,3,NP) *DD(3*NP-1)   
      WW(3*NP-2,W2)= WW(3*NP-2,W2)-(WW(3*NP  ,W2)*D(1,3,NP) +              &
     &                              WW(3*NP-1,W2)*D(1,2,NP))*DD(3*NP-2)

      WW(3*NP  ,T0)= WW(3*NP  ,T0)
      WW(3*NP-1,T0)= WW(3*NP-1,T0)- WW(3*NP  ,T0)*D(2,3,NP) *DD(3*NP-1)   
      WW(3*NP-2,T0)= WW(3*NP-2,T0)-(WW(3*NP  ,T0)*D(1,3,NP) +              &
     &                              WW(3*NP-1,T0)*D(1,2,NP))*DD(3*NP-2)

      do i= NP-1, 1, -1
        isU= INU(i-1) + 1
        ieU= INU(i) 

        SW1_TT= 0.0d0
        SW2_TT= 0.0d0
        SW3_TT= 0.0d0
        SW1_W2= 0.0d0
        SW2_W2= 0.0d0
        SW3_W2= 0.0d0
        SW1_T0= 0.0d0
        SW2_T0= 0.0d0
        SW3_T0= 0.0d0
        do j= ieU, isU, -1
            k= IAU(j)
              X1= WW(3*k-2,TT)
              X2= WW(3*k-1,TT)
              X3= WW(3*k  ,TT)
          SW1_TT= SW1_TT + AU(1,1,j)*X1 + AU(1,2,j)*X2 + AU(1,3,j)*X3
          SW2_TT= SW2_TT + AU(2,1,j)*X1 + AU(2,2,j)*X2 + AU(2,3,j)*X3
          SW3_TT= SW3_TT + AU(3,1,j)*X1 + AU(3,2,j)*X2 + AU(3,3,j)*X3
              X1= WW(3*k-2,W2)
              X2= WW(3*k-1,W2)
              X3= WW(3*k  ,W2)
          SW1_W2= SW1_W2 + AU(1,1,j)*X1 + AU(1,2,j)*X2 + AU(1,3,j)*X3
          SW2_W2= SW2_W2 + AU(2,1,j)*X1 + AU(2,2,j)*X2 + AU(2,3,j)*X3
          SW3_W2= SW3_W2 + AU(3,1,j)*X1 + AU(3,2,j)*X2 + AU(3,3,j)*X3
              X1= WW(3*k-2,T0)
              X2= WW(3*k-1,T0)
              X3= WW(3*k  ,T0)
          SW1_T0= SW1_T0 + AU(1,1,j)*X1 + AU(1,2,j)*X2 + AU(1,3,j)*X3
          SW2_T0= SW2_T0 + AU(2,1,j)*X1 + AU(2,2,j)*X2 + AU(2,3,j)*X3
          SW3_T0= SW3_T0 + AU(3,1,j)*X1 + AU(3,2,j)*X2 + AU(3,3,j)*X3
        enddo

        WW(3*i  ,TT)= WW(3*i  ,TT) - SW3_TT*DD(3*i  )
             SW2_TT = SW2_TT+D(2,3,i)* WW(3*i,TT)
        WW(3*i-1,TT)= WW(3*i-1,TT)-SW2_TT*DD(3*i-1)
             SW1_TT = SW1_TT+D(1,3,i)* WW(3*i,TT)+D(1,2,i)*WW(3*i-1,TT)
        WW(3*i-2,TT)= WW(3*i-2,TT)-SW1_TT*DD(3*i-2)

        WW(3*i  ,W2)= WW(3*i  ,W2) - SW3_W2*DD(3*i  )
             SW2_W2 = SW2_W2+D(2,3,i)* WW(3*i,W2)
        WW(3*i-1,W2)= WW(3*i-1,W2)-SW2_W2*DD(3*i-1)
             SW1_W2 = SW1_W2+D(1,3,i)* WW(3*i,W2)+D(1,2,i)*WW(3*i-1,W2)
        WW(3*i-2,W2)= WW(3*i-2,W2)-SW1_W2*DD(3*i-2)

        WW(3*i  ,T0)= WW(3*i  ,T0) - SW3_T0*DD(3*i  )
             SW2_T0 = SW2_T0+D(2,3,i)* WW(3*i,T0)
        WW(3*i-1,T0)= WW(3*i-1,T0)-SW2_T0*DD(3*i-1)
             SW1_T0 = SW1_T0+D(1,3,i)* WW(3*i,T0)+D(1,2,i)*WW(3*i-1,T0)
        WW(3*i-2,T0)= WW(3*i-2,T0)-SW1_T0*DD(3*i-2)

      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        WW(3*i-2,TT)= WW(3*i-2,TT)*DD(3*i-2)
        WW(3*i-1,TT)= WW(3*i-1,TT)*DD(3*i-1)
        WW(3*i  ,TT)= WW(3*i  ,TT)*DD(3*i  )
        WW(3*i-2,W2)= WW(3*i-2,W2)*DD(3*i-2)
        WW(3*i-1,W2)= WW(3*i-1,W2)*DD(3*i-1)
        WW(3*i  ,W2)= WW(3*i  ,W2)*DD(3*i  )
        WW(3*i-2,T0)= WW(3*i-2,T0)*DD(3*i-2)
        WW(3*i-1,T0)= WW(3*i-1,T0)*DD(3*i-1)
        WW(3*i  ,T0)= WW(3*i  ,T0)*DD(3*i  )
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do i= 1, N
        X1= WW(3*i-2,TT)
        X2= WW(3*i-1,TT)
        X3= WW(3*i  ,TT)

        X2= X2 - ALU(2,1,i)*X1
        X3= X3 - ALU(3,1,i)*X1 - ALU(3,2,i)*X2

        X3= ALU(3,3,i)*  X3
        X2= ALU(2,2,i)*( X2 - ALU(2,3,i)*X3 )
        X1= ALU(1,1,i)*( X1 - ALU(1,3,i)*X3 - ALU(1,2,i)*X2)

        WW(3*i-2,TT)=  X1
        WW(3*i-1,TT)=  X2
        WW(3*i  ,TT)=  X3

        X1= WW(3*i-2,W2)
        X2= WW(3*i-1,W2)
        X3= WW(3*i  ,W2)

        X2= X2 - ALU(2,1,i)*X1
        X3= X3 - ALU(3,1,i)*X1 - ALU(3,2,i)*X2

        X3= ALU(3,3,i)*  X3
        X2= ALU(2,2,i)*( X2 - ALU(2,3,i)*X3 )
        X1= ALU(1,1,i)*( X1 - ALU(1,3,i)*X3 - ALU(1,2,i)*X2)

        WW(3*i-2,W2)=  X1
        WW(3*i-1,W2)=  X2
        WW(3*i  ,W2)=  X3

        X1= WW(3*i-2,T0)
        X2= WW(3*i-1,T0)
        X3= WW(3*i  ,T0)

        X2= X2 - ALU(2,1,i)*X1
        X3= X3 - ALU(3,1,i)*X1 - ALU(3,2,i)*X2

        X3= ALU(3,3,i)*  X3
        X2= ALU(2,2,i)*( X2 - ALU(2,3,i)*X3 )
        X1= ALU(1,1,i)*( X1 - ALU(1,3,i)*X3 - ALU(1,2,i)*X2)

        WW(3*i-2,T0)=  X1
        WW(3*i-1,T0)=  X2
        WW(3*i  ,T0)=  X3
      enddo
      endif      
!C===

!C
!C-- calc. [A]{t_tld}
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,TT) )

      do j= 1, N
           X1= WW(3*j-2,TT)
           X2= WW(3*j-1,TT)
           X3= WW(3*j  ,TT)
        WVAL1= D(1,1,j)*X1 + D(1,2,j)*X2 + D(1,3,j)*X3
        WVAL2= D(2,1,j)*X1 + D(2,2,j)*X2 + D(2,3,j)*X3
        WVAL3= D(3,1,j)*X1 + D(3,2,j)*X2 + D(3,3,j)*X3

        do k= INL(j-1)+1, INL(j)
           i= IAL(k)
          X1= WW(3*i-2,TT)
          X2= WW(3*i-1,TT)
          X3= WW(3*i  ,TT)
          WVAL1= WVAL1 + AL(1,1,k)*X1 + AL(1,2,k)*X2 + AL(1,3,k)*X3
          WVAL2= WVAL2 + AL(2,1,k)*X1 + AL(2,2,k)*X2 + AL(2,3,k)*X3
          WVAL3= WVAL3 + AL(3,1,k)*X1 + AL(3,2,k)*X2 + AL(3,3,k)*X3
        enddo

        do k= INU(j-1)+1, INU(j)
           i= IAU(k)
          X1= WW(3*i-2,TT)
          X2= WW(3*i-1,TT)
          X3= WW(3*i  ,TT)
          WVAL1= WVAL1 + AU(1,1,k)*X1 + AU(1,2,k)*X2 + AU(1,3,k)*X3
          WVAL2= WVAL2 + AU(2,1,k)*X1 + AU(2,2,k)*X2 + AU(2,3,k)*X3
          WVAL3= WVAL3 + AU(3,1,k)*X1 + AU(3,2,k)*X2 + AU(3,3,k)*X3
        enddo

        WW(3*j-2,WK)= WVAL1
        WW(3*j-1,WK)= WVAL2
        WW(3*j  ,WK)= WVAL3
      enddo

      do j= 1, N
        WW(3*j-2,TT)= WW(3*j-2,WK)
        WW(3*j-1,TT)= WW(3*j-1,WK)
        WW(3*j  ,TT)= WW(3*j  ,WK)
      enddo
!C===

!C
!C +-------------------+
!C | calc. QSI and ETA |
!C +-------------------+
!C===
      C0(1)= 0.0d0
      C0(2)= 0.0d0
      C0(3)= 0.0d0
      C0(4)= 0.0d0
      C0(5)= 0.0d0
      CG(1)= 0.0d0
      CG(2)= 0.0d0
      CG(3)= 0.0d0
      CG(4)= 0.0d0
      CG(5)= 0.0d0

      do j= 1, N
        C0(1)= C0(1)+WW(3*j-2, Y)*WW(3*j-2, Y)+WW(3*j-1, Y)*WW(3*j-1, Y)&
     &                                        +WW(3*j  , Y)*WW(3*j  , Y)
        C0(2)= C0(2)+WW(3*j-2,TT)*WW(3*j-2, T)+WW(3*j-1,TT)*WW(3*j-1, T)&
     &                                        +WW(3*j  ,TT)*WW(3*j  , T)
        C0(3)= C0(3)+WW(3*j-2, Y)*WW(3*j-2, T)+WW(3*j-1, Y)*WW(3*j-1, T)&
     &                                        +WW(3*j  , Y)*WW(3*j  , T)
        C0(4)= C0(4)+WW(3*j-2,TT)*WW(3*j-2, Y)+WW(3*j-1,TT)*WW(3*j-1, Y)&
     &                                        +WW(3*j  ,TT)*WW(3*j  , Y)
        C0(5)= C0(5)+WW(3*j-2,TT)*WW(3*j-2,TT)+WW(3*j-1,TT)*WW(3*j-1,TT)&
     &                                        +WW(3*j  ,TT)*WW(3*j  ,TT)
      enddo

      call MPI_allREDUCE (C0, CG,  5, CALYPSO_REAL,                     &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
      if (iter.eq.1) then
        EQ(1)= CG(2)/CG(5)
        EQ(2)= 0.d0
       else
        EQ(1)= (CG(1)*CG(2)-CG(3)*CG(4)) / (CG(5)*CG(1)-CG(4)*CG(4))
        EQ(2)= (CG(5)*CG(3)-CG(4)*CG(2)) / (CG(5)*CG(1)-CG(4)*CG(4))
      endif

      QSI= EQ(1)
      ETA= EQ(2)
!C===

!C
!C +----------------------------------------------------------+
!C | {u} = QSI [Minv]{pt} + ETA([Minv]{t0}-[Minv]{r}+BETA*{u} |
!C | {z} = QSI [Minv]{r}  + ETA*{z} - ALPHA*{u}               |
!C +----------------------------------------------------------+
!C===

!C
!C-- compute. {u},{z}

      do j= 1, N
        WW(3*j-2,U)= QSI* WW(3*j-2,W2) +                                &
     &               ETA*(WW(3*j-2,T0) - WW(3*j-2,R) + BETA*WW(3*j-2,U))
        WW(3*j-2,Z)= QSI* WW(3*j-2, R) +                                &
     &               ETA* WW(3*j-2, Z) -              ALPHA*WW(3*j-2,U)
        WW(3*j-1,U)= QSI* WW(3*j-1,W2) +                                &
     &               ETA*(WW(3*j-1,T0) - WW(3*j-1,R) + BETA*WW(3*j-1,U))
        WW(3*j-1,Z)= QSI* WW(3*j-1, R) +                                &
     &               ETA* WW(3*j-1, Z) -              ALPHA*WW(3*j-1,U)
        WW(3*j  ,U)= QSI* WW(3*j  ,W2) +                                &
     &               ETA*(WW(3*j  ,T0) - WW(3*j  ,R) + BETA*WW(3*j  ,U))
        WW(3*j  ,Z)= QSI* WW(3*j  , R) +                                &
     &               ETA* WW(3*j  , Z) -              ALPHA*WW(3*j  ,U)
      enddo
!C===

!C
!C +--------------------+
!C | update {x},{r},{w} |
!C +--------------------+
!C===
      DNRM20= 0.d0
      COEF10= 0.d0

      do j= 1, N
        X (3*j-2)= X(3*j-2) + ALPHA*WW(3*j-2,P) + WW(3*j-2,Z)
        X (3*j-1)= X(3*j-1) + ALPHA*WW(3*j-1,P) + WW(3*j-1,Z)
        X (3*j  )= X(3*j  ) + ALPHA*WW(3*j  ,P) + WW(3*j  ,Z)

        WW(3*j-2,R)= WW(3*j-2,T) - ETA*WW(3*j-2,Y) - QSI*WW(3*j-2,TT)
        WW(3*j-1,R)= WW(3*j-1,T) - ETA*WW(3*j-1,Y) - QSI*WW(3*j-1,TT)
        WW(3*j  ,R)= WW(3*j  ,T) - ETA*WW(3*j  ,Y) - QSI*WW(3*j  ,TT)

        WW(3*j-2,T0)= WW(3*j-2,T)
        WW(3*j-1,T0)= WW(3*j-1,T)
        WW(3*j  ,T0)= WW(3*j  ,T)

        DNRM20= DNRM20 + WW(3*j-2,R)**2+ WW(3*j-1,R)**2+ WW(3*j,R)**2
        COEF10= COEF10 + WW(3*j-2,R)*WW(3*j-2,RT)                       &
     &                 + WW(3*j-1,R)*WW(3*j-1,RT) + WW(3*j,R)*WW(3*j,RT)
      enddo

      call MPI_allREDUCE  (DNRM20, DNRM2, 1, CALYPSO_REAL,              &
     &                     MPI_SUM, CALYPSO_COMM, ierr)
      call MPI_allREDUCE  (COEF10, COEF1, 1, CALYPSO_REAL,              &
     &                     MPI_SUM, CALYPSO_COMM, ierr)

      BETA = ALPHA*COEF1 / (QSI*RHO)
      do j= 1, N
        WW(3*j-2,W1)= WW(3*j-2,TT) + BETA*WW(3*j-2,PT)
        WW(3*j-1,W1)= WW(3*j-1,TT) + BETA*WW(3*j-1,PT)
        WW(3*j  ,W1)= WW(3*j  ,TT) + BETA*WW(3*j  ,PT)
      enddo

      RESID= dsqrt(DNRM2/BNRM2)
      RHO  = COEF1

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
     &     STACK_EXPORT, NOD_EXPORT, X )

      deallocate (WW)

      end subroutine        GPBiCG_3
      end module     solver_GPBiCG_3
