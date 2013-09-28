!
!  module solver_BiCGSTAB_N_1PE
!
!C*** 
!C*** module solver_BiCGSTAB_N_1PE
!C***
!
!      subroutine BiCGSTAB_N_1PE                                        &
!     &                 (N, NP, NB,  NPL, NPU,                          &
!     &                  D,  AL, INL, IAL, AU, INU, IAU,                &
!     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,              &
!     &                  RESID,  ITER, ERROR,                           &
!     &                  NSET)
!
      module solver_BiCGSTAB_N_1PE
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
!C*** BiCGSTAB_N_1PE
!C
      subroutine BiCGSTAB_N_1PE                                         &
     &                 (N, NP, NB,  NPL, NPU,                           &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  RESID,  ITER, ERROR,                            &
     &                  NSET)

! \beginSUBROUTINE
!     BiCGSTAB_N_1PE solves the linear system Ax = b using the
!     Bi-Conjugate Gradient Stabilized iterative method with preconditioning.
!     3*3 Block Matrix by OVERLAPPING method.
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on Dec. 1999 (ver 3.0)
!    \end{flushright}     

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
!
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

!      integer(kind=kint ) :: NEIBPETOT
!      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE
!      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
!      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))          &
!     &      :: NOD_IMPORT
!      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
!      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))          &
!     &      :: NOD_EXPORT

      real(kind=kreal), dimension(:),    allocatable, save ::  DD
      real(kind=kreal), dimension(:,:),  allocatable       ::  WW
      real(kind=kreal), dimension(:,:,:),allocatable, save :: ALU

      real   (kind=kreal), dimension(2)                :: C0, CG

      integer(kind=kint ) :: IFLAG, MAXIT
      integer(kind=kint ), parameter :: R= 1, RT= 2, P= 3, PT= 4
      integer(kind=kint ), parameter :: S= 5, ST= 6, T= 7, V= 8

      real(kind=kreal), dimension(NB) :: SWN, XN, WVAL
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

      allocate (WW(NB*NP,8))

      if (IFLAG.eq.0) then
        allocate (DD   (NB*NP))
        if (PRECOND(1:4).eq.'BLOC') then
          allocate (ALU(NB,NB,N))
        endif
        IFLAG= 1
      endif

      MAXIT = ITER
      TOL   = RESID

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
!
!        call SOLVER_SEND_RECV_n                                        &
!     &     ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!     &       STACK_EXPORT, NOD_EXPORT, WW(1,R))
!
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
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE
!
!        call SOLVER_SEND_RECV_n                                        &
!     &     ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!     &     STACK_EXPORT, NOD_EXPORT, X)

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
          i= IAL(k)
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
          WW(jj+k1,PT)= 0.d0
          WW(jj+k1,ST)= 0.d0
        end do

      enddo

      BNRM20= 0.d0
      do i= 1, N
        ii = NB* (i-1)
        do k1= 1, NB
          BNRM20= BNRM20 + B(ii+k1)**2
        end do
      enddo

      BNRM2 = BNRM20
      if (BNRM2.eq.0.d0) BNRM2= 1.d0

      iter= 0
!C===

!C
!C******************************************* iterative procedures
!C
      do iter= 1, MAXIT
!C
!C +-----------------+
!C | RHO= {r}{r_tld} |
!C +-----------------+
!C===
      RHO0= 0.d0
      do j= 1, N
          jj = NB*(j-1)
        do k1= 1, NB
          RHO0  = RHO0 + WW(jj+k1,RT)*WW(jj+k1,R)
        end do
      enddo

      RHO = RHO0
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
          jj = NB*(j-1)
          do k1= 1, NB
            WW(jj+k1,P)=  WW(jj+k1,R)                                   &
     &                  + BETA*(WW(jj+k1,P)-OMEGA*WW(jj+k1,V))
          enddo
        enddo
       else
        do j= 1, N
          jj = NB*(j-1)
          do k1= 1, NB
            WW(jj+k1,P)=  WW(jj+k1,R)
          enddo
        enddo
      endif
!C===

!C
!C +--------------------+
!C | {p_tld}= [Minv]{p} |
!C +--------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE
!
!        call SOLVER_SEND_RECV_n                                        &
!     &     ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!     &     STACK_EXPORT, NOD_EXPORT, WW(1,P))

      do j= 1, NP
        jj = NB*(j-1)
        do k1= 1, NB
         WW(jj+k1,PT)=  WW(jj+k1,P)
        enddo
      enddo

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then

      i = 1
      ii = NB*(i-1)
      WW(ii+1,PT)=  WW(ii+1,P) * DD(ii+1)
      do k1= 2, NB
        SS = WW(ii+k1,P)
        do k2 = 1, k1-1
          SS = SS - D(k1,k2,i)*WW(ii+k2,PT)
        end do
        WW(ii+k1,PT) = SS * DD(ii+k1)
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
            XN(k1) = WW(kk+k1,PT)
          end do
          do k1 = 1, NB
            do k2= 1, NB
              SWN(k1) = SWN(k1) - AL(k1,k2,j)*XN(k2)
            end do
          end do
        enddo

        ii = NB*(i-1)
        WW(ii+1,PT)= (WW(ii+1,P)+SWN(1)) * DD(ii+1)
        do k1 = 2, NB
          do k2= 1, k1-1
            SWN(k1) = SWN(k1) - D(k1,k2,i) * WW(ii+k2,PT)
          end do
          WW(ii+k1,PT)= (WW(ii+k1,P)+SWN(k1)) * DD(ii+k1)
        end do
      enddo
      
      ii = NB*NP
      WW(ii,PT)= WW(ii,PT)
      do k1 = 1, NB-1
        SWN(k1) = 0.0d0
        do k2 = 0, k1-1
          SWN(k1) = SWN(k1) + WW(ii-k2,PT)*D(NB-k1,NB-k2,NP)
        end do
        WW(ii-k1,PT) = WW(ii-k1,PT) - SWN(k1)*DD(ii-k1)
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
            XN(k1) =  WW(kk+k1,PT)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              SWN(k1)= SWN(k1) + AU(k1,k2,j)*XN(k2)
            end do
          end do
        enddo

        ii = NB * (i-1)
        WW(ii+NB,PT)= WW(ii+NB,PT) - SWN(NB)*DD(ii+NB)
        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
           SWN(k1) =  SWN(k1) + D(k1,k2,i)* WW(ii+k2,PT)
          end do
          WW(ii+k1,PT)= WW(ii+k1,PT) - SWN(k1)*DD(ii+k1)
        end do
      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        ii = NB*(i-1)
        do k1 = 1, NB
          WW(ii+k1,PT)= WW(ii+k1,P)*DD(ii+k1)
        end do
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do i= 1, N
        ii = NB*(i-1)
        do k1 = 1, NB
          XN(k1)= WW(ii+k1,P)
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
          WW(ii+k1,PT)=  XN(k1)
        end do
      enddo
      endif      
!C===

!C
!C +-------------------+
!C | {v} = [A] {p_tld} |
!C +-------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE
!
!        call SOLVER_SEND_RECV_n                                        &
!     &     ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!     &     STACK_EXPORT, NOD_EXPORT, WW(1,PT))

!C
!C-- BEGIN calculation
      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          XN(k1) = WW(jj+k1,PT)
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
            XN(k1) = WW(ii+k1,PT)
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
            XN(k1) = WW(ii+k1,PT)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              WVAL(k1) = WVAL(k1) + AU(k1,k2,k)*XN(k2)
            end do
          end do
        enddo

        do k1 = 1, NB
          WW(jj+k1,V)= WVAL(k1)
        end do
      enddo
!C===

!C
!C-- calc. ALPHA

      C20= 0.d0
      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          C20= C20 + WW(jj+k1,RT)*WW(jj+k1,V)
        end do
      enddo

      C2 = C20
      ALPHA= RHO / C2

!C
!C-- {s}= {r} - ALPHA*{V}
      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          WW(jj+k1,S)= WW(jj+k1,R) - ALPHA*WW(jj+k1,V)
        end do
      enddo

!C
!C +--------------------+
!C | {s_tld}= [Minv]{s} |
!C +--------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE
!
!        call SOLVER_SEND_RECV_n                                        &
!     &     ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!     &     STACK_EXPORT, NOD_EXPORT, WW(1,S))

      do j= 1, NP
        jj = NB*(j-1)
        do k1 = 1, NB
          WW(jj+k1,ST)= WW(jj+k1,S)
        end do
      enddo

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then

      i = 1
      ii = NB*(i-1)
      WW(ii+1,ST)=  WW(ii+1,S) * DD(ii+1)
      do k1= 2, NB
        SWN(k1) = WW(ii+k1,S)
        do k2 = 1, k1-1
          SWN(k1) = SWN(k1) - D(k1,k2,i)*WW(ii+k2,ST)
        end do
        WW(ii+k1,ST) = SWN(k1) * DD(ii+k1)
      end do

      do i= 2, NP
        do k1 = 1, NB
          SWN(k1) = 0.0d0
        end do
        isL= INL(i-1)+1
        ieL= INL(i)
        do j= isL, ieL
           kk= NB*(IAL(j)-1)
           do k1 = 1, NB
             XN(k1) = WW(kk+k1,ST)
           end do
           do k1 = 1, NB
             do k2= 1, NB
               SWN(k1) = SWN(k1) - AL(k1,k2,j)*XN(k2)
             end do
           end do
        enddo

        ii = NB*(i-1)
        WW(ii+1,ST)= (WW(ii+1,S)+SWN(1)) * DD(ii+1)
        do k1 = 2, NB
          do k2= 1, k1-1
            SWN(k1) =  SWN(k1) - D(k1,k2,i) * WW(ii+k2,ST)
          end do
          WW(ii+k1,ST)= (WW(ii+k1,S)+SWN(k1)) * DD(ii+k1)
        end do
      enddo
      
      ii = NB*NP
      WW(ii,ST)= WW(ii,ST)
      do k1 = 1, NB-1
        SWN(k1) = 0.0d0
        do k2 = 0, k1-1
          SWN(k1) = SWN(k1) + WW(ii-k2,ST)*D(NB-k1,NB-k2,NP)
        end do
        WW(ii-k1,ST) = WW(ii-k1,ST) - SWN(k1)*DD(ii-k1)
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
            XN(k1) =  WW(kk+k1,ST)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              SWN(k1)= SWN(k1) + AU(k1,k2,j)*XN(k2)
            end do
          end do
        enddo

        ii = NB * (i-1)
        WW(ii+NB,ST)= WW(ii+NB,ST) - SWN(NB)*DD(ii+NB)
        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
           SWN(k1) =  SWN(k1) + D(k1,k2,i)* WW(ii+k2,ST)
          end do
          WW(ii+k1,ST)= WW(ii+k1,ST) - SWN(k1)*DD(ii+k1)
        end do

      enddo
      endif

      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        ii = NB*(i-1)
        do k1 = 1, NB
          WW(ii+k1,ST)= WW(ii+k1,S)*DD(ii+k1)
        end do
      enddo
      endif

!C
!C-- Block
      if (PRECOND(1:4).eq.'BLOC') then
      do i= 1, N
        ii = NB*(i-1)
        do k1 = 1, NB
          XN(k1)= WW(ii+k1,S)
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
          WW(ii+k1,ST)=  XN(k1)
        end do
      enddo
      endif      
!C===

!C
!C +------------------+
!C | {t} = [A]{s_tld} |
!C +------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE
!        call SOLVER_SEND_RECV_n                                        &
!     &     ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!     &     STACK_EXPORT, NOD_EXPORT, WW(1,ST))

!C
!C-- BEGIN calculation
      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          XN(k1) = WW(jj+k1,ST)
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
            XN(k1) = WW(ii+k1,ST)
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
            XN(k1) = WW(ii+k1,ST)
          end do
          do k1 = 1, NB
            do k2 = 1, NB
              WVAL(k1) = WVAL(k1) + AU(k1,k2,k)*XN(k2)
            end do
          end do
        enddo

        do k1 = 1, NB
          WW(jj+k1,T)= WVAL(k1)
        end do
      enddo
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
        jj = NB*(j-1)
        do k1 = 1, NB
          C0(1)= C0(1) + WW(jj+k1,T)*WW(jj+k1,S)
          C0(2)= C0(2) + WW(jj+k1,T)*WW(jj+k1,T)
        end do
      enddo

      CG(1:2) = C0(1:2)
      OMEGA= CG(1) / CG(2)
!C===

!C
!C +----------------+
!C | update {x},{r} |
!C +----------------+
!C===
      DNRM20= 0.d0
      do j= 1, N
        jj = NB*(j-1)
        do k1 = 1, NB
          X (jj+k1)= X(jj+k1) + ALPHA*WW(jj+k1,PT) + OMEGA*WW(jj+k1,ST)
          WW(jj+k1,R)= WW(jj+k1,S) - OMEGA*WW(jj+k1,T)
          DNRM20= DNRM20+WW(jj+k1,S)**2
        end do
      enddo

      RHO1= RHO

      DNRM2 = DNRM20
      RESID= dsqrt(DNRM2/BNRM2)

!C##### ITERATION HISTORY
        write (*, 1000) ITER, RESID
 1000   format ('BiCGSTAB: ', i5, 1pe16.6)
!C#####

      if (RESID.le.TOL   ) exit
      if ( ITER.eq.MAXIT ) ERROR= -300
!C===

      end do
!C
!C-- INTERFACE data EXCHANGE
!        call SOLVER_SEND_RECV_n                                        &
!     &     ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!     &     STACK_EXPORT, NOD_EXPORT, X)


      deallocate (WW)

      end subroutine        BiCGSTAB_N_1PE
      end module     solver_BiCGSTAB_N_1PE

