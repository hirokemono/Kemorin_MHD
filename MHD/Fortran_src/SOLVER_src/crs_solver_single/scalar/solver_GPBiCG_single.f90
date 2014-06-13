!
!  module solver_GPBiCG_single
!
!C*** 
!C*** module solver_GPBiCG_single
!C***
!
      module solver_GPBiCG_single
!
      use m_precision
!
      implicit REAL*8(A-H,O-Z)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!C
!C*** GPBiCG
!C
      subroutine GPBiCG                                                 &
     &                 (N, NP,  NPL, NPU,                               &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  RESID,  ITER, ERROR,                            &
     &                  my_rank, NSET)     

! \beginSUBROUTINE
!     GPBiCG solves the linear system Ax = b using the
!     GPBiCG iterative method with preconditioning.
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!    \end{flushright}     

      implicit REAL*8(A-H,O-Z)

      integer(kind=kint ),                   intent(in   )::  N
      integer(kind=kint ),                   intent(in   )::  NP
      integer(kind=kint ),                   intent(in   )::  NPL
      integer(kind=kint ),                   intent(in   )::  NPU
!
      real   (kind=kreal),                   intent(inout)::  RESID
      real   (kind=kreal),                   intent(in   )::  SIGMA_DIAG
      real   (kind=kreal),                   intent(in   )::  SIGMA
      integer(kind=kint ),                   intent(inout)::  ITER
      integer(kind=kint ),                   intent(inout)::  ERROR
      integer(kind=kint ),                   intent(in   )::  my_rank

      integer(kind=kint )                  , intent(in)   :: NSET

      real   (kind=kreal), dimension(NP )  , intent(inout)::  D
      real   (kind=kreal), dimension(NP )  , intent(inout)::  B
      real   (kind=kreal), dimension(NP )  , intent(inout)::  X

      real   (kind=kreal), dimension(NPU)  , intent(inout)::  AU
      real   (kind=kreal), dimension(NPU)  , intent(inout)::  AL

      integer(kind=kint ), dimension(0:NP)   , intent(in) ::  INU
      integer(kind=kint ), dimension(0:NP)   , intent(in) ::  INL
      integer(kind=kint ), dimension(  NPU)  , intent(in) ::  IAU
      integer(kind=kint ), dimension(  NPL)  , intent(in) ::  IAL
      character(len=kchara)                  , intent(in) :: PRECOND

      real   (kind=kreal), dimension(:),   allocatable, save :: DD
      real   (kind=kreal), dimension(:,:), allocatable :: WW
      real   (kind=kreal), dimension(5)                :: C0, CG
      real   (kind=kreal), dimension(2)                :: EQ

      integer(kind=kint ) :: IFLAG, MONITORFLAG, R, RT, T, T0, TT, P, PT
      integer(kind=kint ) ::  U, W1, Y, Z, WK, W2, MAXIT
      integer(kind=kint ) :: isL, isU, ieL, ieU, i, inod, id, kk, k, j
      real   (kind=kreal) :: TOL, W, SS, BETA, ALPHA, RHO, RHO1
      data IFLAG/0/

!      open (12,file='test.lis',status='unknown')

!C
!C-- INIT.
      allocate (WW(NP,13))

      MAXIT = ITER
      TOL   = RESID

      MONITORFLAG = ERROR
      ERROR= 0
      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 101
        return
      endif

      if (IFLAG.eq.0) then
        allocate (DD   (NP))
        IFLAG= 1
      endif

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
        DD(i)= 0.d0
      enddo

      if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
        do i= 1, NP
          isL= INL(i-1) + 1
          ieL= INL(i)
          W= D(i) * SIGMA_DIAG
          do k= isL, ieL
            SS=  AL(k)
            id= IAL(k)

            isU= INU(id-1) + 1
            ieU= INU(id)
            do kk= isU, ieU
              SS= SS + AU(kk) * SIGMA
            enddo
            W= W - AL(k)*SS*DD(id)
          enddo
          DD(i)= 1.d0 / W
        enddo
      endif

      if (PRECOND(1:4).eq.'SSOR' .or. PRECOND(1:4).eq.'DIAG') then
        do i= 1, NP
          DD(i)= 1.d0/(SIGMA_DIAG*D(i))
        enddo
      endif
!C===
      endif

!C
!C +----------------------+
!C | {r}= {b} - [A]{xini} |
!C +----------------------+
!C===
      do j= 1, N
        WVAL= B(j) - D(j) * X(j)
         isU= INU(j-1) + 1
         ieU= INU(j  ) 
        do i= isU, ieU
          inod= IAU(i)
          WVAL= WVAL - AU(i) * X(inod)
        enddo

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          inod= IAL(i)
          WVAL= WVAL - AL(i) * X(inod)
        enddo
        WW(j,R)= WVAL
      enddo

      BNRM20 = 0.0d0
      RHO0   = 0.0d0
      do i= 1, N
        WW(i,RT)= WW(i,R)
        WW(i, T)= 0.d0
        WW(i,T0)= 0.d0
        WW(i,W1)= 0.d0
          BNRM20= BNRM20 + B(i)**2
          RHO0  = RHO0   + WW(i,RT)*WW(i,R) 
      enddo

      BNRM2 = BNRM20
      RHO = RHO0
!
      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===

!C
!C*************************************************************** ITERATIVE PROC.
!C
      do iter= 1, MAXIT
!C
!C-- INIT.
      do j= 1, N
        WW(j,WK)=  WW(j,R)
      enddo

!C
!C +----------------+
!C | {r}= [Minv]{r} |
!C +----------------+
!C===

!C
!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      do i= 1, NP
        isL= INL(i-1) + 1
        ieL= INL(i  ) 
        WVAL= WW(i,R)
        do j= isL, ieL
          inod= IAL(j)
          WVAL=  WVAL -  AL(j) * WW(inod,R)
        enddo
        WW(i,R)= WVAL * DD(i)
      enddo

      do i= NP, 1, -1
        SW = 0.0d0
        isU= INU(i-1) + 1
        ieU= INU(i  ) 
        do j= isU, ieU
          inod = IAU(j)
          SW= SW + AU(j) * WW(inod,R)
        enddo
        WW(i,R)= WW(i,R) - DD(i) * SW
      enddo
      endif

!C
!C-- diagonal
      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        WW(i,R)=  WW(i,R) * DD(i)
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
          WW(j,P)= WW(j,R) + BETA*( WW(j,P)-WW(j,U))
        enddo
       else
        do j= 1, N
          WW(j,P)= WW(j,R)
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

      do j= 1, N
        WVAL= D(j) * WW(j,P)
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          inod= IAU(i)
          WVAL= WVAL + AU(i) * WW(inod,P)
        enddo

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          inod= IAL(i)
          WVAL= WVAL + AL(i) * WW(inod,P)
        enddo
        WW(j,PT)= WVAL
      enddo
!C
!C-- calc. ALPHA

      RHO10= 0.d0
      do j= 1, N
        RHO10= RHO10 + WW(j,RT)*WW(j,PT)
      enddo

      RHO1 = RHO10

      ALPHA= RHO / RHO1
!C===

!C
!C +------------------------------------------+
!C | {y}= {t} - {r} - ALPHA{w} + ALPHA{p_tld} |
!C | {t}= {r}                  - ALPHA{p_tld} |
!C +------------------------------------------+
!C===
      do j= 1, N
        WW(j,Y)= WW(j, T) - WW(j,WK) + ALPHA * (-WW(j,W1)+WW(j,PT))
        WW(j,T)= WW(j,WK)            - ALPHA *            WW(j,PT)
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

      do i= 1, NP
        WW(i,TT)= WW(i,T)
        WW(i,W2)= WW(i,PT)
      enddo

!C
!C-- incomplete CHOLESKY

      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      do i= 1, NP
        isL= INL(i-1) + 1
        ieL= INL(i  ) 
        WVAL1= WW(i,TT)
        WVAL2= WW(i,W2)
        WVAL3= WW(i,T0)
        do j= isL, ieL
          inod = IAL(j)
          WVAL1=  WVAL1 -  AL(j) * WW(inod,TT)
          WVAL2=  WVAL2 -  AL(j) * WW(inod,W2)
          WVAL3=  WVAL3 -  AL(j) * WW(inod,T0)
        enddo
        WW(i,TT)= WVAL1 * DD(i)
        WW(i,W2)= WVAL2 * DD(i)
        WW(i,T0)= WVAL3 * DD(i)
      enddo

      do i= NP, 1, -1
        SW1= 0.0d0
        SW2= 0.0d0
        SW3= 0.0d0
        isU= INU(i-1) + 1
        ieU= INU(i  ) 
        do j= isU, ieU
          inod = IAU(j)
          SW1= SW1 + AU(j) * WW(inod,TT)
          SW2= SW2 + AU(j) * WW(inod,W2)
          SW3= SW3 + AU(j) * WW(inod,T0)
        enddo
        WW(i,TT)= WW(i,TT) - DD(i) * SW1
        WW(i,W2)= WW(i,W2) - DD(i) * SW2
        WW(i,T0)= WW(i,T0) - DD(i) * SW3
      enddo
      endif

!C
!C-- diagonal
      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        WW(i,TT)=  WW(i,TT) * DD(i)
        WW(i,W2)=  WW(i,W2) * DD(i)
        WW(i,T0)=  WW(i,T0) * DD(i)
      enddo
      endif      
!C===

!C
!C-- calc. [A]{t_tld}
!
      do j= 1, N
        WVAL= D(j) * WW(j,TT)
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          inod= IAU(i)
          WVAL= WVAL + AU(i) * WW(inod,TT)
        enddo

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          inod= IAL(i)
          WVAL= WVAL + AL(i) * WW(inod,TT)
        enddo
        WW(j,WK)= WVAL
      enddo

      do j= 1, N
        WW(j,TT)= WW(j,WK)
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
        C0(1)= C0(1) + WW(j, Y) * WW(j, Y)
        C0(2)= C0(2) + WW(j,TT) * WW(j, T)
        C0(3)= C0(3) + WW(j, Y) * WW(j, T)
        C0(4)= C0(4) + WW(j,TT) * WW(j, Y)
        C0(5)= C0(5) + WW(j,TT) * WW(j,TT)
      enddo

      CG(1:5) = C0(1:5)

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
        WW(j,U)= QSI*WW(j,W2) +                                         &
     &           ETA*(WW(j,T0)-WW(j,R)+BETA*WW(j,U))
        WW(j,Z)= QSI* WW(j, R)+ETA*WW(j,Z) - ALPHA*WW(j,U)
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
        X (j)   =  X(j)   + ALPHA*WW(j,P) +     WW(j,Z)
        WW(j,R) = WW(j,T) - ETA  *WW(j,Y) - QSI*WW(j,TT)
        WW(j,T0)= WW(j,T)
          DNRM20= DNRM20  + WW(j,R)**2
          COEF10= COEF10  + WW(j,R)*WW(j,RT)
      enddo

      DNRM2 = DNRM20
      COEF1 = COEF10

      BETA = ALPHA*COEF1 / (QSI*RHO)
      do j= 1, N
        WW(j,W1)= WW(j,TT) + BETA*WW(j,PT)
      enddo

      RESID= dsqrt(DNRM2/BNRM2)
      RHO  = COEF1

        if (my_rank.eq.0 .and. MONITORFLAG.eq.1)                        &
     &    write (*, 1000) ITER, RESID
 1000   format ('GPBICG_11: ', i5, 1pe16.6)

      if (RESID.le.TOL   ) exit
      if ( ITER.eq.MAXIT ) ERROR= -100
!C===
      enddo

!C
!C-- INTERFACE data EXCHANGE

      deallocate (WW)

      end subroutine        GPBiCG
      end module     solver_GPBiCG_single
