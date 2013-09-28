!
!C*** 
!C*** module solver_CG_3
!C***
!
      module solver_CG_3
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
!C*** CG_3
!C
      subroutine CG_3  (N, NP, NPL, NPU, D, AL, INL, IAL, AU, INU, IAU, &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  RESID,  ITER, ERROR,                            &
     &                  my_rank, NEIBPETOT, NEIBPE,                     &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT, NSET)
!
! \beginSUBROUTINE
!     CG_3 solves the linear system Ax = b using the
!     Conjugate Gradient iterative method with preconditioning.
!     3*3 Block Matrix.
!    \begin{flushright}
!     coded by K.Nakajima (RIST) on Nov. 1999 (ver 3.0)     
!    \end{flushright}     

!
!      1 | 2 | 3
!     ---+---+---
!      4 | 5 | 6
!     ---+---+---
!      7 | 8 | 9
!
!
      use calypso_mpi
!
      use solver_SR_3
      use crs_matrix_calcs_33
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

      real(kind=kreal), dimension(:),    allocatable, save ::  DD, SCALE
      real(kind=kreal), dimension(:,:),  allocatable       ::  WW
      real(kind=kreal), dimension(:,:,:),allocatable, save :: ALU

      integer(kind=kint ) :: P, Q, R, Z, MAXIT, IFLAG
      real   (kind=kreal) :: TOL, W, SS
      real   (kind=kreal), dimension(3)                    :: PW
      data IFLAG/0/

!C
!C-- INIT. 
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 301
        return
      endif

      allocate (WW(3*NP,3))

      if (IFLAG.eq.0) then
        allocate (DD   (3*NP))
        allocate (SCALE(3*NP))
        if (PRECOND(1:4).eq.'BLOC') then
          allocate (ALU(3,3,N))
        endif
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
      
      MAXIT  = ITER
       TOL   = RESID           

      if (NSET.ge.1) then
!C
!C-- SCALING
      if (NSET.eq.2) then
      do i= 1, N
        SCALE (3*i-2)= 1.d0/dsqrt(dabs(D(1,1,i)))
        SCALE (3*i-1)= 1.d0/dsqrt(dabs(D(2,2,i)))
        SCALE (3*i  )= 1.d0/dsqrt(dabs(D(3,3,i)))
      end do

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SCALE)

      do i= 1, N
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

!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
      do i= 1, NP
        DD(3*i-2)= 0.d0
        DD(3*i-1)= 0.d0
        DD(3*i  )= 0.d0
      enddo

      if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
        i= 1
        DD(i*3-2)= 1.d0/(D(1,1,i)*SIGMA_DIAG)

        W2= D(2,2,i)*SIGMA_DIAG - D(2,1,i)**2*DD(i*3-2)
        DD(i*3-1)= 1.d0/W2
        W3= D(3,3,i)*SIGMA_DIAG - D(3,1,i)**2*DD(i*3-2)                 &
     &                          - D(3,2,i)**2*DD(i*3-1)        
        DD(i*3  )= 1.d0/W3

        do i= 2, N
          W1= D(1,1,i) * SIGMA_DIAG
          W2= D(2,2,i) * SIGMA_DIAG
          W3= D(3,3,i) * SIGMA_DIAG

          do k= INL(i-1)+1, INL(i)
            in= IAL(k)
            W1= W1 - AL(1,1,k)**2*DD(3*in-2) - AL(1,2,k)**2*DD(3*in-1)  &
     &             - AL(1,3,k)**2*DD(3*in  )                          
            W2= W2 - AL(2,1,k)**2*DD(3*in-2) - AL(2,2,k)**2*DD(3*in-1)  &
     &             - AL(2,3,k)**2*DD(3*in  )                       
            W3= W3 - AL(3,1,k)**2*DD(3*in-2) - AL(3,2,k)**2*DD(3*in-1)  &
     &             - AL(3,3,k)**2*DD(3*in  )
          enddo
          DD(3*i-2)= 1.d0/W1
          W2       = W2 - D(2,1,i)**2*DD(3*i-2)               
          DD(3*i-1)= 1.d0/W2
          W3       = W3 - D(3,1,i)**2*DD(3*i-2) - D(3,2,i)**2*DD(3*i-1)
          DD(3*i  )= 1.d0/W3
        enddo
      endif

      if (PRECOND(1:4).eq.'SSOR' .or. PRECOND(1:4).eq.'DIAG') then
        do i= 1, N
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
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
      do i= 1, N
        B     (3*i-2)= B(3*i-2) * SCALE(3*i-2)
        B     (3*i-1)= B(3*i-1) * SCALE(3*i-1)
        B     (3*i  )= B(3*i  ) * SCALE(3*i  )
      enddo
!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)

!C
!C-- BEGIN calculation
!
      call subtruct_crs_matvec_33 (NP, N, NPL, NPU,                     &
     &           INL, INU, IAL, IAU, D, AL, AU, WW(1,R), B, X)
!
      BNRM20= 0.d0
      do i= 1, N
        BNRM20= BNRM20+B(3*i-2)**2+B(3*i-1)**2+B(3*i)**2
      enddo

      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
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
      do i= 1, N
        WW(3*i-2,Z)= WW(3*i-2,R)
        WW(3*i-1,Z)= WW(3*i-1,R)
        WW(3*i  ,Z)= WW(3*i  ,R)
      enddo

      do i= 1+N, NP
        WW(3*i-2,Z)= 0.d0
        WW(3*i-1,Z)= 0.d0
        WW(3*i  ,Z)= 0.d0
      enddo

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      WW(3*1-2,Z)=  WW(3*1-2,R)                        *DD(3*1-2)
      WW(3*1-1,Z)= (WW(3*1-1,R) - D(2,1,1)*WW(3*1-2,Z))*DD(3*1-1)
      WW(3*1  ,Z)= (WW(3*1  ,R) - D(3,1,1)*WW(3*1-2,Z)                  &
     &                          - D(3,2,1)*WW(3*1-1,Z))*DD(3*1  )

      do i= 2, N
        SW1= 0.0d0
        SW2= 0.0d0
        SW3= 0.0d0
        isL= INL(i-1)+1
        ieL= INL(i)
        do j= isL, ieL
            k= IAL(j)
           X1= WW(3*k-2,Z)
           X2= WW(3*k-1,Z)
           X3= WW(3*k  ,Z)
          SW1= SW1 - AL(1,1,j)*X1 - AL(1,2,j)*X2 - AL(1,3,j)*X3
          SW2= SW2 - AL(2,1,j)*X1 - AL(2,2,j)*X2 - AL(2,3,j)*X3
          SW3= SW3 - AL(3,1,j)*X1 - AL(3,2,j)*X2 - AL(3,3,j)*X3
        enddo

        WW(3*i-2,Z)= (WW(3*i-2,Z)+SW1) * DD(3*i-2)
               SW2 =  SW2   - D(2,1,i) * WW(3*i-2,Z)
        WW(3*i-1,Z)= (WW(3*i-1,Z)+SW2) * DD(3*i-1)
               SW3 =  SW3   - D(3,1,i) * WW(3*i-2,Z)                    &
     &                      - D(3,2,i) * WW(3*i-1,Z)
        WW(3*i  ,Z)= (WW(3*i  ,Z)+SW3) * DD(3*i  )
      enddo
      
      WW(3*N  ,Z)= WW(3*N  ,Z)
      WW(3*N-1,Z)= WW(3*N-1,Z)- WW(3*N,Z)  *D(2,3,N) *DD(3*N-1)   
      WW(3*N-2,Z)= WW(3*N-2,Z)-(WW(3*N,Z)  *D(1,3,N) +                  &
     &                          WW(3*N-1,Z)*D(1,2,N))*DD(3*N-2)

      do i= N-1, 1, -1
        isU= INU(i-1) + 1
        ieU= INU(i) 

        SW1= 0.d0
        SW2= 0.d0
        SW3= 0.d0
        do j= ieU, isU, -1
            k= IAU(j)
           X1= WW(3*k-2,Z)
           X2= WW(3*k-1,Z)
           X3= WW(3*k  ,Z)
          SW1= SW1 + AU(1,1,j)*X1 + AU(1,2,j)*X2 + AU(1,3,j)*X3
          SW2= SW2 + AU(2,1,j)*X1 + AU(2,2,j)*X2 + AU(2,3,j)*X3
          SW3= SW3 + AU(3,1,j)*X1 + AU(3,2,j)*X2 + AU(3,3,j)*X3
        enddo

        WW(3*i  ,Z)= WW(3*i  ,Z)-SW3*DD(3*i  )
               SW2 = SW2 + D(2,3,i)* WW(3*i,Z)
        WW(3*i-1,Z)= WW(3*i-1,Z)-SW2*DD(3*i-1)
               SW1 = SW1 + D(1,3,i)* WW(3*i  ,Z) + D(1,2,i)* WW(3*i-1,Z)
        WW(3*i-2,Z)= WW(3*i-2,Z)-SW1*DD(3*i-2)
      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        WW(3*i-2,Z)= WW(3*i-2,R)*DD(3*i-2)
        WW(3*i-1,Z)= WW(3*i-1,R)*DD(3*i-1)
        WW(3*i  ,Z)= WW(3*i  ,R)*DD(3*i  )
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

        WW(3*i-2,Z)=  X1
        WW(3*i-1,Z)=  X2
        WW(3*i  ,Z)=  X3
      enddo
      endif      
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
     &                    MPI_SUM, CALYPSO_COMM, ierr)
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
     &     STACK_EXPORT, NOD_EXPORT, WW(1,P) )

!C
        call cal_crs_matvec_33(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      D, AL, AU, WW(1,Q), WW(1,P) )
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
     &                    MPI_SUM, CALYPSO_COMM, ierr)
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
     &                    MPI_SUM, CALYPSO_COMM, ierr)

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
   30 continue

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

      end subroutine        CG_3
!
!
!
      end module     solver_CG_3
