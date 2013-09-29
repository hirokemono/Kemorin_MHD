!
! \beginFILE
!  module solver_GPBiCG_N
!
! \beginMODULE
!
!C*** 
!C*** module solver_GPBiCG_N
!C***
!
      module solver_GPBiCG_N
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
!C*** GPBiCG_N
!C
      subroutine GPBiCG_N                                               &
     &                 (N, NP, NB, NPL, NPU,                            &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  RESID,  ITER, ERROR, NEIBPETOT, NEIBPE,         &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT, NSET)

! \beginSUBROUTINE
!     GPBiCG_N solves the linear system Ax = b using the
!     GPBiCG iterative method with preconditioning.
!     n*n Block Matrix by OVERLAPPING method.
!    \begin{flushright}     
!     coded by H.Matsui (U chicago) on Feb.2005 (ver 1.0)
!    \end{flushright}     

      use calypso_mpi
!
      use  solver_SR_N
!

      integer(kind=kint ),                   intent(in   )::  N
      integer(kind=kint ),                   intent(in   )::  NB
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


      integer(kind=kint ) :: NEIBPETOT
      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &      :: NOD_IMPORT
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &      :: NOD_EXPORT

      real(kind=kreal), dimension(:),    allocatable, save ::  DD
      real(kind=kreal), dimension(:,:),  allocatable       ::  WW
      real(kind=kreal), dimension(:,:,:),allocatable, save :: ALU

      real(kind=kreal), dimension(5) :: C0, CG
      real(kind=kreal), dimension(2) :: EQ


      integer(kind=kint ) :: IFLAG, MAXIT


      integer(kind=kint ), parameter :: R = 1, RT = 2
      integer(kind=kint ), parameter :: T = 3, TT = 4, T0 = 5
      integer(kind=kint ), parameter :: P = 6, PT = 7
      integer(kind=kint ), parameter :: U = 8, W1 = 9, Y = 10, Z = 11
      integer(kind=kint ), parameter :: WK = 12, W2 = 13

      real(kind=kreal), dimension(NB) :: SWN, SWN_TT, SWN_W2, SWN_T0
      real   (kind=kreal) :: TOL, W, SS, WNB
      real   (kind=kreal), dimension(NB) :: PW, XN, WVAL
      data IFLAG/0/

!C
!C-- INIT.
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 301
        return
      endif

      allocate (WW(NB*NP,13))

      if (IFLAG.eq.0) then
        allocate (DD   (NB*NP))
        if (PRECOND(1:4).eq.'BLOC') then
          allocate (ALU(NB,NB,N))
        endif
        IFLAG= 1
      endif

      MAXIT = ITER
      TOL   = RESID

      ERROR= 0

      if (NSET.ge.1) then
!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
      do k1 = 1, NB
       do i= 1, NP
        ii = NB*(i-1)
        DD(ii+k1)= 0.d0
        WW(ii+k1,R)= D(k1,k1,i)
       enddo
      enddo

        call SOLVER_SEND_RECV_n                                         &
     &     ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,       &
     &       STACK_EXPORT, NOD_EXPORT, WW(1,R))
        do k1 = 1, NB
         do i= N+1, NP
          ii = NB*(i-1)
          D(k1,k1,i)= WW(ii+k1,R)
         enddo
        enddo

      if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
        i= 1
        ii = (i-1)*NB

        DD(ii+1)= 1.d0/(D(1,1,i)*SIGMA_DIAG)

        do k1= 2, NB
          SS= 0.0d0
          do k2= 1, k1-1
           SS = SS + D(k1,k2,i)**2*DD(ii+k2)
          end do
          WNB = D(k1,k1,i)*SIGMA_DIAG - SS
          DD(ii+k1)= 1.d0/WNB
        end do

        do i= 2, NP
          ii = (i-1)*NB
          do k1 = 1, NB
           SWN(k1) = D(k1,k1,i) * SIGMA_DIAG
          end do

          do k= INL(i-1)+1, INL(i)
            in= NB*(IAL(k)-1)
            do j1= 1, NB
             do j2= 1, NB
              SWN(j1) = SWN(j1) - AL(j1,j2,k)**2*DD(in+j2)
             end do
            end do
          end do

          DD(ii+1)= 1.d0/SWN(1)
          do j1= 2, NB
            do j2= 1, j1-1
              SWN(j1)= SWN(j1) - D(j1,j2,i)**2*DD(ii+j2)
            enddo
            DD(ii+j1)= 1.d0/SWN(j1)
          enddo
        enddo
      endif

      if (PRECOND(1:4).eq.'SSOR' .or. PRECOND(1:4).eq.'DIAG') then
        do i= 1, NP
          ii = NB*(i-1)
          do k= 1, NB
           DD(ii+k)= 1.d0/(D(k,k,i)*SIGMA_DIAG)
          enddo
        enddo
      endif

      if (PRECOND(1:4).eq.'BLOC') then
        do ip= 1, N
         do k1= 1, NB
          do k2= 1, NB
           ALU(k1,k2,ip)= D(k1,k2,ip)
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
!C +----------------------+
!C | {r}= {b} - [A]{xini} |
!C +----------------------+
!C===
!C
!C-- INTERFACE data EXCHANGE
      call SOLVER_SEND_RECV_n                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, X)

!C
!C-- BEGIN calculation

      do j= 1, N

        jj = NB*(j-1)

        do k1 = 1, NB
          XN(k1) = X(jj+k1)
          WVAL(k1) = B(jj+k1)
        end do
        do k1 = 1, NB
          do k2 = 1, NB
            WVAL(k1) = WVAL(k1) - D(k1,k2,j)*XN(k2)
          end do
        end do

        do k= INL(j-1)+1, INL(j)
          ii= NB*( IAL(k)-1 )
          do k1 = 1, NB
            XN(k1) = X(ii+k1)
          end do
          do k1= 1, NB
            do k2= 1, NB
              WVAL(k1) = WVAL(k1) - AL(k1,k2,k)*XN(k2)
            end do
          end do
        enddo

        do k= INU(j-1)+1, INU(j)
          ii= NB*( IAU(k) - 1 )
          do k1 = 1, NB
            XN(k1) = X(ii+k1)
          end do
          do k1= 1, NB
            do k2= 1, NB
              WVAL(k1) = WVAL(k1) - AU(k1,k2,k)*XN(k2)
            end do
          end do
        enddo

        do k1= 1, NB
          WW(jj+k1,R )= WVAL(k1)
          WW(jj+k1,RT)= WVAL(k1)
          WW(jj+k1,T )= 0.d0
          WW(jj+k1,T0)= 0.d0
          WW(jj+k1,W1)= 0.d0
        end do

      enddo


      BNRM20= 0.d0
      RHO0  = 0.0d0
      do i= 1, N
        ii = NB* (i-1)
        do k1= 1, NB
          BNRM20= BNRM20 + B(ii+k1)**2
          RHO0  = RHO0 + WW(ii+k1,RT)*WW(ii+k1,R)
        end do
      enddo

      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
      call MPI_allREDUCE (RHO0  , RHO,   1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr)

      if (BNRM2.eq.0.d0) BNRM2= 1.d0      
!C===

!C
!C******************************************* ITERATIVE PROC.
!C
      do iter= 1, MAXIT
!C
!C-- INIT.
      do j= 1, N
        jj = NB*(j-1)
        do k1= 1, NB
         WW(jj+k1,WK)=  WW(jj+k1,R)
        enddo
      enddo

!C
!C +----------------+
!C | {r}= [Minv]{r} |
!C +----------------+
!C===

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV_n                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,R))

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then

      i = 1
      ii = NB*(i-1)
      WW(ii+1,R)=  WW(ii+1,R) * DD(ii+1)
      do k1= 2, NB
        SS = WW(ii+k1,R)
        do k2 = 1, k1-1
          SS = SS - D(k1,k2,i)*WW(ii+k2,R)
        end do
        WW(ii+k1,R) = SS * DD(ii+k1)
      end do

      do i= 2, NP
        do k1= 1, NB
          SWN(k1) = 0.0d0
        end do

        isL= INL(i-1)+1
        ieL= INL(i)
        do j= isL, ieL
          kk= NB*(IAL(j)-1)
          do k1 = 1, NB
            XN(k1) = WW(kk+k1,R)
          end do
          do k1 = 1, NB
            do k2= 1, NB
              SWN(k1) = SWN(k1) - AL(k1,k2,j)*XN(k2)
            end do
          end do
        enddo

        ii = NB*(i-1)
        WW(ii+1,R)= (WW(ii+1,R)+SWN(1)) * DD(ii+1)
        do k1 = 2, NB
          do k2= 1, k1-1
            SWN(k1) = SWN(k1) - D(k1,k2,i) * WW(ii+k2,R)
          end do
          WW(ii+k1,R)= (WW(ii+k1,R)+SWN(k1)) * DD(ii+k1)
        end do
      enddo
      
      ii = NB*NP
      WW(ii,R)= WW(ii,R)
      do k1 = 1, NB-1
        SWN(k1) = 0.0d0
        do k2 = 0, k1-1
          SWN(k1) = SWN(k1) + WW(ii-k2,R)*D(NB-k1,NB-k2,NP)
        end do
        WW(ii-k1,R) = WW(ii-k1,R) - SWN(k1)*DD(ii-k1)
      end do

      do i= NP-1, 1, -1
        isU= INU(i-1) + 1
        ieU= INU(i) 

        do k1 = 1, NB
          SWN(k1) = 0.0d0
        end do

        do j= ieU, isU, -1
          kk= NB*(IAU(j)-1)
          do k1 = 1, NB
            XN(k1) =  WW(kk+k1,R)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              SWN(k1)= SWN(k1) + AU(k1,k2,j)*XN(k2)
            end do
          end do
        enddo

        ii = NB * (i-1)
        WW(ii+NB,R)= WW(ii+NB,R) - SWN(NB)*DD(ii+NB)
        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
           SWN(k1) =  SWN(k1) + D(k1,k2,i)* WW(ii+k2,R)
          end do
          WW(ii+k1,R)= WW(ii+k1,R) - SWN(k1)*DD(ii+k1)
        end do
      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        ii = NB*(i-1)
        do k1 = 1, NB
          WW(ii+k1,R)= WW(ii+k1,R)*DD(ii+k1)
        end do
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do i= 1, N
        ii = NB*(i-1)
        do k1 = 1, NB
          XN(k1)= WW(ii+k1,R)
        end do

        do k1 = 2, NB
          do k2 = 1, k1-1
            XN(k1) = XN(k1) - ALU(k1,k2,i)*XN(k2)
          end do
        end do

        XN(NB) = ALU(NB,NB,i) * XN(NB)
        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
            XN(k1) = XN(k1) - ALU(k1,k2,i)*XN(k2)
          end do
          XN(k1) = ALU(k1,k1,i)*XN(k1)
        end do

        do k1 = 1, NB
          WW(ii+k1,R)=  XN(k1)
        end do
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
          jj = NB*(j-1)
          do k1 = 1, NB
            WW(jj+k1,P)= WW(jj+k1,R) + BETA*( WW(jj+k1,P)-WW(jj+k1,U))
          end do
        enddo
       else
        do j= 1, N
          jj = NB*(j-1)
          do k1 = 1, NB
            WW(jj+k1,P)= WW(jj+k1,R)
          end do
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

      call SOLVER_SEND_RECV_n                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,P))

      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          XN(k1) = WW(jj+k1,P)
        end do
        do k1 = 1, NB
          WVAL(k1) = 0.0d0
          do k2 = 1, NB
            WVAL(k1) = WVAL(k1) + D(k1,k2,j)*XN(k2)
          end do
        end do

        do k= INL(j-1)+1, INL(j)
          ii= NB*(IAL(k)-1)
          do k1 = 1, NB
            XN(k1) = WW(ii+k1,P)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              WVAL(k1) = WVAL(k1) + AL(k1,k2,k)*XN(k2)
            end do
          end do
        enddo

        do k= INU(j-1)+1, INU(j)
          ii= NB*(IAU(k)-1)
          do k1 = 1, NB
            XN(k1) = WW(ii+k1,P)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              WVAL(k1) = WVAL(k1) + AU(k1,k2,k)*XN(k2)
            end do
          end do
        enddo

        do k1 = 1, NB
          WW(jj+k1,PT)= WVAL(k1)
        end do
      enddo

!C
!C-- calc. ALPHA

      RHO10= 0.d0
      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          RHO10 = RHO10 + WW(jj+k1,RT)*WW(jj+k1,PT)
        end do
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
        jj = NB*(j-1)
        do k1 = 1, NB
          WW(jj+k1,Y)= WW(jj+k1,T) - WW(jj+k1,WK)                       &
     &                   + ALPHA*(-WW(jj+k1,W1) + WW(jj+k1,PT))
          WW(jj+k1,T)= WW(jj+k1,WK) - ALPHA*WW(jj+k1,PT)
        end do
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

      call SOLVER_SEND_RECV_n                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,PT))

      call SOLVER_SEND_RECV_n                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,T))

      call SOLVER_SEND_RECV_n                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,T0))

      do i= 1, NP
        ii = NB*(i-1)
        do k1 = 1, NB
          WW(ii+k1,TT)= WW(ii+k1,T )
          WW(ii+k1,W2)= WW(ii+k1,PT)
        end do
      enddo

!C
!C-- incomplete CHOLESKY

      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then

      i = 1
      ii = NB*(i-1)
      WW(ii+1,TT)=  WW(ii+1,TT) * DD(ii+1)
      WW(ii+1,W2)=  WW(ii+1,W2) * DD(ii+1)
      WW(ii+1,T0)=  WW(ii+1,T0) * DD(ii+1)
      do k1= 2, NB
        SWN_TT(k1) = WW(ii+k1,TT)
        SWN_W2(k1) = WW(ii+k1,W2)
        SWN_T0(k1) = WW(ii+k1,T0)
        do k2 = 1, k1-1
          SWN_TT(k1) = SWN_TT(k1) - D(k1,k2,i)*WW(ii+k2,TT)
          SWN_W2(k1) = SWN_W2(k1) - D(k1,k2,i)*WW(ii+k2,W2)
          SWN_T0(k1) = SWN_T0(k1) - D(k1,k2,i)*WW(ii+k2,T0)
        end do
        WW(ii+k1,TT) = SWN_TT(k1) * DD(ii+k1)
        WW(ii+k1,W2) = SWN_W2(k1) * DD(ii+k1)
        WW(ii+k1,T0) = SWN_T0(k1) * DD(ii+k1)
      end do


      do i= 2, NP
        do k1 = 1, NB
          SWN_TT(k1) = 0.0d0
          SWN_W2(k1) = 0.0d0
          SWN_T0(k1) = 0.0d0
        end do
        isL= INL(i-1)+1
        ieL= INL(i)
        do j= isL, ieL
           kk= NB*(IAL(j)-1)
           do k1 = 1, NB
             XN(k1) = WW(kk+k1,TT)
           end do
           do k1 = 1, NB
             do k2= 1, NB
               SWN_TT(k1) = SWN_TT(k1) - AL(k1,k2,j)*XN(k2)
             end do
           end do

           do k1 = 1, NB
             XN(k1) = WW(kk+k1,W2)
           end do
           do k1 = 1, NB
             do k2= 1, NB
               SWN_W2(k1) = SWN_W2(k1) - AL(k1,k2,j)*XN(k2)
             end do
           end do

           do k1 = 1, NB
             XN(k1) = WW(kk+k1,T0)
           end do
           do k1 = 1, NB
             do k2= 1, NB
               SWN_T0(k1) = SWN_T0(k1) - AL(k1,k2,j)*XN(k2)
             end do
           end do
        enddo

        ii = NB*(i-1)
        WW(ii+1,TT)= (WW(ii+1,TT)+SWN_TT(1)) * DD(ii+1)
        WW(ii+1,W2)= (WW(ii+1,W2)+SWN_W2(1)) * DD(ii+1)
        WW(ii+1,T0)= (WW(ii+1,T0)+SWN_T0(1)) * DD(ii+1)
        do k1 = 2, NB
          do k2= 1, k1-1
            SWN_TT(k1) =  SWN_TT(k1) - D(k1,k2,i) * WW(ii+k2,TT)
            SWN_W2(k1) =  SWN_W2(k1) - D(k1,k2,i) * WW(ii+k2,W2)
            SWN_T0(k1) =  SWN_T0(k1) - D(k1,k2,i) * WW(ii+k2,T0)
          end do
          WW(ii+k1,TT)= (WW(ii+k1,TT)+SWN_TT(k1)) * DD(ii+k1)
          WW(ii+k1,W2)= (WW(ii+k1,W2)+SWN_W2(k1)) * DD(ii+k1)
          WW(ii+k1,T0)= (WW(ii+k1,T0)+SWN_T0(k1)) * DD(ii+k1)
        end do

      enddo
      
      ii = NB*NP

      WW(ii,TT)= WW(ii,TT)
      WW(ii,W2)= WW(ii,W2)
      WW(ii,T0)= WW(ii,T0)
      do k1 = 1, NB-1
        SWN_TT(k1) = 0.0d0
        SWN_W2(k1) = 0.0d0
        SWN_T0(k1) = 0.0d0
        do k2 = 0, k1-1
          SWN_TT(k1) = SWN_TT(k1) + WW(ii-k2,TT)*D(NB-k1,NB-k2,NP)
          SWN_W2(k1) = SWN_W2(k1) + WW(ii-k2,W2)*D(NB-k1,NB-k2,NP)
          SWN_T0(k1) = SWN_T0(k1) + WW(ii-k2,T0)*D(NB-k1,NB-k2,NP)
        end do
        WW(ii-k1,TT) = WW(ii-k1,TT) - SWN_TT(k1)*DD(ii-k1)
        WW(ii-k1,W2) = WW(ii-k1,W2) - SWN_W2(k1)*DD(ii-k1)
        WW(ii-k1,T0) = WW(ii-k1,T0) - SWN_T0(k1)*DD(ii-k1)
      end do

      do i= NP-1, 1, -1
        isU= INU(i-1) + 1
        ieU= INU(i) 

        do k1 = 1, NB
          SWN_TT(k1) = 0.0d0
          SWN_W2(k1) = 0.0d0
          SWN_T0(k1) = 0.0d0
        end do

        do j= ieU, isU, -1
          kk= NB*(IAU(j)-1)
          do k1 = 1, NB
            XN(k1) =  WW(kk+k1,TT)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              SWN_TT(k1)= SWN_TT(k1) + AU(k1,k2,j)*XN(k2)
            end do
          end do

          do k1 = 1, NB
            XN(k1) =  WW(kk+k1,W2)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              SWN_W2(k1)= SWN_W2(k1) + AU(k1,k2,j)*XN(k2)
            end do
          end do

          do k1 = 1, NB
            XN(k1) =  WW(kk+k1,T0)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              SWN_T0(k1)= SWN_T0(k1) + AU(k1,k2,j)*XN(k2)
            end do
          end do

        enddo

        ii = NB * (i-1)
        WW(ii+NB,TT)= WW(ii+NB,TT) - SWN_TT(NB)*DD(ii+NB)
        WW(ii+NB,W2)= WW(ii+NB,W2) - SWN_W2(NB)*DD(ii+NB)
        WW(ii+NB,T0)= WW(ii+NB,T0) - SWN_T0(NB)*DD(ii+NB)
        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
           SWN_TT(k1) =  SWN_TT(k1) + D(k1,k2,i)* WW(ii+k2,TT)
           SWN_W2(k1) =  SWN_W2(k1) + D(k1,k2,i)* WW(ii+k2,W2)
           SWN_T0(k1) =  SWN_T0(k1) + D(k1,k2,i)* WW(ii+k2,T0)
          end do
          WW(ii+k1,TT)= WW(ii+k1,TT) - SWN_TT(k1)*DD(ii+k1)
          WW(ii+k1,W2)= WW(ii+k1,W2) - SWN_W2(k1)*DD(ii+k1)
          WW(ii+k1,T0)= WW(ii+k1,T0) - SWN_T0(k1)*DD(ii+k1)
        end do

      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        ii = NB*(i-1)
        do k1 = 1, NB
          WW(ii+k1,TT)= WW(ii+k1,TT)*DD(ii+k1)
          WW(ii+k1,W2)= WW(ii+k1,W2)*DD(ii+k1)
          WW(ii+k1,T0)= WW(ii+k1,T0)*DD(ii+k1)
        end do
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do i= 1, N
        ii = NB*(i-1)
        do k1 = 1, NB
          XN(k1)= WW(ii+k1,TT)
        end do

        do k1 = 2, NB
          do k2 = 1, k1-1
            XN(k1) = XN(k1) - ALU(k1,k2,i)*XN(k2)
          end do
        end do

        XN(NB) = ALU(NB,NB,i) * XN(NB)
        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
            XN(k1) = XN(k1) - ALU(k1,k2,i)*XN(k2)
          end do
          XN(k1) = ALU(k1,k1,i)*XN(k1)
        end do

        do k1 = 1, NB
          WW(ii+k1,TT)=  XN(k1)
        end do

        do k1 = 1, NB
          XN(k1)= WW(ii+k1,W2)
        end do

        do k1 = 2, NB
          do k2 = 1, k1-1
            XN(k1) = XN(k1) - ALU(k1,k2,i)*XN(k2)
          end do
        end do

        XN(NB) = ALU(NB,NB,i) * XN(NB)
        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
            XN(k1) = XN(k1) - ALU(k1,k2,i)*XN(k2)
          end do
          XN(k1) = ALU(k1,k1,i)*XN(k1)
        end do

        do k1 = 1, NB
          WW(ii+k1,W2)=  XN(k1)
        end do

        do k1 = 1, NB
          XN(k1)= WW(ii+k1,T0)
        end do

        do k1 = 2, NB
          do k2 = 1, k1-1
            XN(k1) = XN(k1) - ALU(k1,k2,i)*XN(k2)
          end do
        end do

        XN(NB) = ALU(NB,NB,i) * XN(NB)
        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
            XN(k1) = XN(k1) - ALU(k1,k2,i)*XN(k2)
          end do
          XN(k1) = ALU(k1,k1,i)*XN(k1)
        end do

        do k1 = 1, NB
          WW(ii+k1,T0)=  XN(k1)
        end do

      enddo
      endif      
!C===

!C
!C-- calc. [A]{t_tld}
      call SOLVER_SEND_RECV_n                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,TT))

      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          XN(k1) = WW(jj+k1,TT)
          WVAL(k1) = 0.0d0
        end do
        do k1 = 1, NB
          do k2 = 1, NB
            WVAL(k1) = WVAL(k1) + D(k1,k2,j)*XN(k2)
          end do
        end do

        do k= INL(j-1)+1, INL(j)
          ii = NB*(IAL(k)-1)
          do k1 = 1, NB
            XN(k1) = WW(ii+k1,TT)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              WVAL(k1) = WVAL(k1) + AL(k1,k2,k)*XN(k2)
            end do
          end do
        enddo

        do k= INU(j-1)+1, INU(j)
          ii = NB*(IAU(k)-1)
          do k1 = 1, NB
            XN(k1) = WW(ii+k1,TT)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              WVAL(k1) = WVAL(k1) + AU(k1,k2,k)*XN(k2)
            end do
          end do
        enddo

        do k1 = 1, NB
          WW(jj+k1,WK)= WVAL(k1)
        end do
      enddo

      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          WW(jj+k1,TT)= WW(jj+k1,WK)
        end do
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
        jj = NB*(j-1)
        do k1 = 1, NB
          C0(1)= C0(1) + WW(jj+k1, Y)*WW(jj+k1, Y)
          C0(2)= C0(2) + WW(jj+k1,TT)*WW(jj+k1, T)
          C0(3)= C0(3) + WW(jj+k1, Y)*WW(jj+k1, T)
          C0(4)= C0(4) + WW(jj+k1,TT)*WW(jj+k1, Y)
          C0(5)= C0(5) + WW(jj+k1,TT)*WW(jj+k1,TT)
        end do
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
        jj = NB*(j-1)
        do k1 = 1, NB
          WW(jj+k1,U) =  QSI * WW(jj+k1,W2)                             &
     &                 + ETA * ( WW(jj+k1,T0) - WW(jj+k1,R)             &
     &                          + BETA*WW(jj+k1,U) )
          WW(jj+k1,Z) =  QSI * WW(jj+k1, R)                             &
     &                 + ETA * WW(jj+k1, Z) - ALPHA * WW(jj+k1,U)
        end do

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
        jj = NB*(j-1)
        do k1 = 1, NB
          X (jj+k1)= X(jj+k1) + ALPHA*WW(jj+k1,P) + WW(jj+k1,Z)
          WW(jj+k1,R)= WW(jj+k1,T) - ETA*WW(jj+k1,Y) - QSI*WW(jj+k1,TT)
          WW(jj+k1,T0)= WW(jj+k1,T)
          DNRM20= DNRM20 + WW(jj+k1,R)**2
          COEF10= COEF10 + WW(jj+k1,R)*WW(jj+k1,RT)
        end do

      enddo

      call MPI_allREDUCE  (DNRM20, DNRM2, 1, CALYPSO_REAL,              &
     &                     MPI_SUM, CALYPSO_COMM, ierr)
      call MPI_allREDUCE  (COEF10, COEF1, 1, CALYPSO_REAL,              &
     &                     MPI_SUM, CALYPSO_COMM, ierr)

!C
!C +--------------------------------------+
!C | BETA= ALPHA*({r}Minv{r}) / (QSI*RHO) |
!C | {w} = {t_tld} + BETA{p_tld}          |
!C +--------------------------------------+
!C===
      BETA = ALPHA*COEF1 / (QSI*RHO)
      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          WW(jj+k1,W1)= WW(jj+k1,TT) + BETA*WW(jj+k1,PT)
        end do
      enddo

      RESID= dsqrt(DNRM2/BNRM2)
      RHO  = COEF1

!C##### ITERATION HISTORY
        if (my_rank.eq.0) write (*, 1000) ITER, RESID
 1000   format ('GPBiCG: ', i5, 1pe16.6)
!C#####

      if (RESID.le.TOL   ) exit
      if ( ITER.eq.MAXIT ) ERROR= -300
!C===
      enddo

!C
!C-- INTERFACE data EXCHANGE
      call SOLVER_SEND_RECV_n                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, X)

      deallocate (WW)

      end subroutine        GPBiCG_N
      end module     solver_GPBiCG_N
