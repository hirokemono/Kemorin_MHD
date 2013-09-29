!
!  module solver_CG_N
!
!C*** 
!C*** module solver_CG_N
!C***
!
      module solver_CG_N
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
!C*** CG_N
!C
      subroutine CG_N  (N, NP, NB, NPL, NPU, D, AL, INL, IAL, AU,       &
     &                  INU, IAU, B, X, PRECOND, SIGMA_DIAG,SIGMA,      &
     &                  RESID,  ITER, ERROR, NEIBPETOT, NEIBPE,         &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT, NSET)

! \beginSUBROUTINE
!     CG_N solves the linear system Ax = b using the
!     Conjugate Gradient iterative method with preconditioning.
!     NB*NB Block Matrix.
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
      use  solver_SR_N
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

      real   (kind=kreal), dimension(NB*NP) , intent(inout)::  B
      real   (kind=kreal), dimension(NB*NP) , intent(inout)::  X

      real   (kind=kreal), dimension(NB,NB,NPL), intent(inout)::  AL
      real   (kind=kreal), dimension(NB,NB,NPU), intent(inout)::  AU
      real   (kind=kreal), dimension(NB,NB,NP ), intent(inout)::  D

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

      real(kind=kreal), dimension(:),    allocatable       ::  WB, PW
      real(kind=kreal), dimension(:),    allocatable, save ::  DD, SCALE
      real(kind=kreal), dimension(:,:),  allocatable       ::  WW
      real(kind=kreal), dimension(:,:,:),allocatable, save :: ALU

      integer(kind=kint ) :: P, Q, R, Z, MAXIT, IFLAG
      real   (kind=kreal) :: TOL, W, SS, WNB
      data IFLAG/0/

!C
!C-- INIT. 
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 301
        return
      endif

      allocate (WW(NB*NP,3))
      allocate (WB(NB), PW(NB))

      if (IFLAG.eq.0) then
        allocate (DD   (NB*NP))
        allocate (SCALE(NB*NP))
        if (PRECOND(1:4).eq.'BLOC') then
          allocate (ALU(NB,NB,N))
        endif
        IFLAG= 1
        do i= 1, NP
          ii= NB*i-NB
        do k= 1, NB
          SCALE(ii+k)= 1.d0
        enddo
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
        ii= NB*i - NB
        do k= 1, NB
          SCALE (ii+k)= 1.d0/dsqrt(dabs(D(k,k,i)))
        enddo
      enddo

      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SCALE)

      do i= 1, N
        ii= NB*i- NB
        do k1= 1, NB  
        do k2= 1, NB  
          ip1= ii + k1
          ip2= ii + k2
          D(k1,k2,i)= D(k1,k2,i)*SCALE(ip1)*SCALE(ip2)
        enddo
        enddo

        isL= INL(i-1) + 1
        ieL= INL(i  ) 
        do k= isL, ieL
          in= NB*IAL(k) - NB
          do k1= 1, NB  
          do k2= 1, NB  
            ip1= ii + k1
            iq1= in + k2
            AL(k1,k2,k)= AL(k1,k2,k)*SCALE(ip1)*SCALE(iq1)
          enddo
          enddo
        enddo

        isU= INU(i-1) + 1
        ieU= INU(i  ) 
        do k= isU, ieU
          in= NB*IAU(k) - NB
          do k1= 1, NB  
          do k2= 1, NB  
            ip1= ii + k1
            iq1= in + k2
            AU(k1,k2,k)= AU(k1,k2,k)*SCALE(ip1)*SCALE(iq1)
          enddo
          enddo
        enddo
      enddo
      endif

!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
      do i= 1, NP
        ii= NB*i- NB
        do k= 1, NB
        DD(ii+k)= 0.d0
        enddo
      enddo

      if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
         i= 1
        ii= NB*i- NB
        DD(ii+1)= 1.d0/(D(1,1,i)*SIGMA_DIAG)

        do k1= 2, NB
          SS= 0.d0
          do k2= 1, k1-1
            SS= SS + D(k1,k2,i)**2*DD(ii+k2)
          enddo
          WNB= D(k1,k1,i)*SIGMA_DIAG - SS
          DD(ii+k1)= 1.d0/WNB
        enddo

        do i= 2, N
          ii= NB*i-NB
          do j= 1, NB
            WB(j)= D(j,j,i) * SIGMA_DIAG
          enddo

          do k= INL(i-1)+1, INL(i)
            in= NB*IAL(k)-NB
            do j1= 1, NB
            do j2= 1, NB
              WB(j1)= WB(j1) - AL(j1,j2,k)**2*DD(in+j2)
            enddo
            enddo
          enddo

          DD(ii+1)= 1.d0/WB(1)
          do j1= 2, NB
            do j2= 1, j1-1
              WB(j1)= WB(j1) - D(j1,j2,i)**2*DD(ii+j2)               
            enddo
            DD(ii+j1)= 1.d0/WB(j1)
          enddo
        enddo
      endif

      if (PRECOND(1:4).eq.'SSOR' .or. PRECOND(1:4).eq.'DIAG') then
        do i= 1, N
          ii= NB*i-NB
        do k= 1, NB
          DD(ii+k)= 1.d0/(D(k,k,i)*SIGMA_DIAG)
        enddo
        enddo
      endif

      if (PRECOND(1:4).eq.'BLOC') then
        do ip= 1, N
          do i= 1, NB
          do j= 1, NB
            ALU(i,j,ip)= D(i,j,ip)
          enddo
          enddo

          do k= 1, NB
             L = k
            ALO= dabs(ALU(L,k,ip))
            do i= k+1, NB
              if (dabs(ALU(i,k,ip)).gt.ALO) then
                 L = i
                ALO= dabs(ALU(L,k,ip))
              endif
            enddo

            ALU(k,k,ip)= 1.d0/ALU(k,k,ip)
            do i= k+1, NB
              ALU(i,k,ip)= ALU(i,k,ip) * ALU(k,k,ip)
              do j= k+1, NB
                PW(j)= ALU(i,j,ip) - ALU(i,k,ip)*ALU(k,j,ip)
              enddo
              do j= k+1, NB
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
        ii= NB*i-NB
        do k= 1, NB
          B(ii+k)= B(ii+k) * SCALE(ii+k)
        enddo
      enddo
!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, X)

!C
!C-- BEGIN calculation

      do j= 1, N
        jj= NB*j-NB

        do k1= 1, NB
          WB(k1)= 0.d0
          do k2= 1, NB
            WB(k1)= B(jj+k1) - D(k1,k2,j)*X(jj+k2)
          enddo
        enddo

        do k= INL(j-1)+1, INL(j)
          ii= NB*IAL(k)-NB
          do k1= 1, NB
          do k2= 1, NB
            WB(k1)= WB(k1) - AL(k1,k2,k)*X(ii+k2)
          enddo
          enddo
        enddo

        do k= INU(j-1)+1, INU(j)
          ii= NB*IAU(k)-NB
          do k1= 1, NB
          do k2= 1, NB
            WB(k1)= WB(k1) - AU(k1,k2,k)*X(ii+k2)
          enddo
          enddo
        enddo

        do k1= 1, NB
          WW(jj+k1,R)= WB(k1)
        enddo
      enddo

      BNRM20= 0.d0
      do j= 1, N
        jj= NB*j-NB
        do k= 1, NB
          BNRM20= BNRM20 + B(jj+k)**2
        enddo
      enddo

      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
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
        ii= NB*i-NB
        do k= 1, NB
          WW(ii+k,Z)= WW(ii+k,R)
        enddo
      enddo

      do i= N+1, NP
        ii= NB*i-NB
        do k= 1, NB
          WW(ii+k,Z)= 0.d0
        enddo
      enddo

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
       i= 1
      ii= NB*i-NB
      WW(ii+1,Z)=  WW(ii+1,R)*DD(ii+1)
      do k1= 2, NB
        SS= WW(ii+k1,R)
        do k2= 1, k1-1
          SS= SS - D(k1,k2,i)*WW(ii+k2,Z)
        enddo
        WW(ii+k1,Z)= SS * DD(ii+k1)
      enddo

      do i= 2, N
        do k1= 1, NB
          WB(k1)= 0.0d0
        enddo

        isL= INL(i-1)+1
        ieL= INL(i)
        do j= isL, ieL
           k= IAL(j)
          in= NB*k - NB
          do k1= 1, NB
          do k2= 1, NB
            WB(k1)= WB(k1) - AL(k1,k2,j)*WW(in+k2,Z)
          enddo
          enddo
        enddo

        ii= NB*i - NB
        WW(ii+1,Z)= (WW(ii+1,Z)+WB(1)) * DD(ii+1)
          do k1= 2, NB
            do k2= 1, k1-1
              WB(k1)= WB(k1) - D(k1,k2,i)*WW(ii+k2,Z)
            enddo
            WW(ii+k1,Z)= (WW(ii+k1,Z)+WB(k1)) * DD(ii+k1) 
          enddo

      enddo

      WW(NB*N  ,Z)= WW(NB*N  ,Z)
      ii= NB*N
      do k1= 1, NB-1
        WB(k1)= 0.d0
        do k2= 0, k1-1
          WB(k1)= WB(k1) + WW(ii-k2,Z)*D(NB-k1,NB-k2,N)
        enddo
        WW(ii-k1,Z)= WW(ii-k1,Z) - WB(k1)*DD(ii-k1)
      enddo

      do i= N-1, 1, -1
        isU= INU(i-1) + 1
        ieU= INU(i) 

        do k= 1, NB
          WB(k)= 0.d0
        enddo

        do j= ieU, isU, -1
            k= IAU(j)
           ii= NB*k - NB
          do k1= 1, NB
          do k2= 1, NB
            WB(k1)= WB(k1) + AU(k1,k2,j)*WW(ii+k2,Z)
          enddo
          enddo
        enddo

        ii= NB*i
        WW(ii,Z)= WW(ii,Z) - WB(NB)*DD(ii)

        ii= NB*i - NB
        do k1= NB-1, 1, -1
          do k2= NB, k1+1, -1
            WB(k1)= WB(k1) + D(k1,k2,i)*WW(ii+k2,Z)
          enddo
          WW(ii+k1,Z)= WW(ii+k1,Z) - WB(k1)*DD(ii+k1)
        enddo
      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        ii= NB*i-NB
        do k= 1, NB
          WW(ii+k,Z)= WW(ii+k,R)*DD(ii+k)
        enddo
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do i= 1, N
        ii= NB*i - NB
        do k= 1, NB
          PW(k)= WW(ii+k,R)
        enddo

        do k1= 2, NB
          do k2= 1, k1-1
            PW(k1)= PW(k1) - ALU(k1,k2,i)*PW(k2)
          enddo
        enddo

        PW(NB)= ALU(NB,NB,i) * PW(NB)
        WW(ii+NB,Z)= PW(NB)
        do k1= NB-1, 1, -1
          do k2= NB, k1+1, -1
            PW(k1)= PW(k1) - ALU(k1,k2,i)*PW(k2)
          enddo
          PW(   k1  )= PW(k1) * ALU(k1,k1,i)
          WW(ii+k1,Z)= PW(k1)
        enddo
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
        ii= NB*i - NB
      do k= 1, NB
        RHO0= RHO0 + WW(ii+k,R)*WW(ii+k,Z)
      enddo
      enddo

      call MPI_allREDUCE (RHO0, RHO, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
!C===

!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C +-----------------------------+
!C===
      if ( ITER.eq.1 ) then
        do i= 1, N
          ii= NB*i-NB
        do k= 1, NB
          WW(ii+k,P)= WW(ii+k,Z)
        enddo
        enddo
       else
         BETA= RHO / RHO1
         do i= 1, N
           ii= NB*i-NB
         do k= 1, NB
           WW(ii+k,P)= WW(ii+k,Z) + BETA*WW(ii+k,P)
         enddo
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

      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,P))

!C
!C-- BEGIN calculation
      do j= 1, N
        jj= j*NB - NB
        do k1= 1, NB
          WB(k1)= 0.d0
        do k2= 1, NB
          WB(k1)= WB(k1) + D(k1,k2,j)*WW(jj+k2,P)
        enddo
        enddo

        do k= INL(j-1)+1, INL(j)
          ii= NB*IAL(k)-NB
          do k1= 1, NB
          do k2= 1, NB
            WB(k1)= WB(k1) + AL(k1,k2,k)*WW(ii+k2,P)
          enddo
          enddo
        enddo

        do k= INU(j-1)+1, INU(j)
          ii= NB*IAU(k)-NB
          do k1= 1, NB
          do k2= 1, NB
            WB(k1)= WB(k1) + AU(k1,k2,k)*WW(ii+k2,P)
          enddo
          enddo
        enddo

        do k= 1, NB
          WW(jj+k,Q)= WB(k)
        enddo

      enddo
!C===

!C
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
      C10= 0.d0
      do i= 1, N
        ii= NB*i-NB
      do k= 1, NB
        C10= C10 + WW(ii+k,P)*WW(ii+k,Q)
      enddo
      enddo

      call MPI_allREDUCE (C10, C1, 1, CALYPSO_REAL,                     &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)

      ALPHA= RHO / C1
!C===

!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C +----------------------+
!C===
      do i= 1, N
        ii= NB*i - NB
      do k= 1, NB
         X(ii+k)  = X (ii+k)   + ALPHA * WW(ii+k,P)
        WW(ii+k,R)= WW(ii+k,R) - ALPHA * WW(ii+k,Q)
      enddo
      enddo

      DNRM20= 0.d0
      do i= 1, N
        ii= NB*i - NB
      do k= 1, NB
        DNRM20= DNRM20 + WW(ii+k,R)**2
      enddo
      enddo

      call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)

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

      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, X)

      do i= 1, N
        ii= NB*i - NB
        do k= 1, NB
          X(ii+k)= X(ii+k)*SCALE(ii+k)
          B(ii+k)= B(ii+k)/SCALE(ii+k)
        enddo
      enddo

      deallocate (WW)

      end subroutine        CG_N
      end module     solver_CG_N






