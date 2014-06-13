!
!  module solver_BiCGSTAB_single
!
!C*** 
!C*** module solver_BiCGSTAB_single
!C***
!
      module solver_BiCGSTAB_single
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
!C*** BiCGSTAB
!C
      subroutine BiCGSTAB                                               &
     &                 (N, NP,  NPL, NPU,                               &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  RESID,  ITER, ERROR,                            &
     &                  my_rank, NSET)     

! \beginSUBROUTINE
!     BiCGSTAB solves the linear system Ax = b using the
!     Bi-Conjugate Gradient Stabilized iterative method with preconditioning.
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

      real   (kind=kreal), dimension(:), allocatable, save :: DD

      real   (kind=kreal), dimension(:,:), allocatable :: WW
      real   (kind=kreal), dimension(2)                :: C0, CG

      integer(kind=kint ) :: IFLAG, MONITORFLAG
      integer(kind=kint ) :: R, RT, P, PT, S, ST, T, V, MAXIT
      integer(kind=kint ) :: isL, isU, ieL, ieU, i, j, k, inod, id, kk
      real   (kind=kreal) :: TOL, W, SS, BETA, RHO, RHO1, ALPHA, OMEGA
      data IFLAG/0/

!C
!C-- INIT.
      allocate (WW(NP,8))

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
      do i= 1, N
        WW(i,RT)= WW(i,R)
          BNRM20= BNRM20 + B(i)**2
      enddo

      BNRM2 = BNRM20
      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===

!C
!C*************************************************************** ITERATIVE PROC.
!C
      do iter= 1, MAXIT
!C
!C +-----------------+
!C | RHO= {r}{r_tld} |
!C +-----------------+
!C===
      RHO0= 0.d0
      do j= 1, N
        RHO0= RHO0 + WW(j,RT)*WW(j,R)
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
          WW(j,P)= WW(j,R) + BETA*( WW(j,P)-OMEGA*WW(j,V))
        enddo
      else
        do j= 1, N
          WW(j,P)= WW(j,R)
        enddo
      endif
!C===

!C
!C +--------------------+
!C | {p}= [Minv]{p_tld} |
!C +--------------------+
!C===

      do i= 1, NP
        WW(i,PT)= WW(i,P)
      enddo

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      do i= 1, NP
        isL= INL(i-1) + 1
        ieL= INL(i  ) 
        WVAL= WW(i,PT)
        do j= isL, ieL
          inod = IAL(j)
          WVAL=  WVAL -  AL(j) * WW(inod,PT)
        enddo
        WW(i,PT)= WVAL * DD(i)
      enddo

      do i= NP, 1, -1
        SW  = 0.0d0

        isU= INU(i-1) + 1
        ieU= INU(i  ) 

        do j= isU, ieU
          inod = IAU(j)
          SW= SW + AU(j) * WW(inod,PT)
        enddo
        WW(i,PT)= WW(i,PT) - DD(i) * SW
      enddo
      endif

!C
!C-- diagonal
      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        WW(i,PT)=  WW(i,PT) * DD(i)
      enddo
      endif      
!C===

!C
!C +-------------------+
!C | {v} = [A] {p_tld} |
!C +-------------------+
!C===

      do j= 1, N
        WVAL= D(j) * WW(j,PT)
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          inod= IAU(i)
          WVAL= WVAL + AU(i) * WW(inod,PT)
        enddo

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          inod= IAL(i)
          WVAL= WVAL + AL(i) * WW(inod,PT)
        enddo
        WW(j,V)= WVAL
      enddo
!C===

!C
!C-- calc. ALPHA

      C20= 0.d0
      do j= 1, N
        C20= C20 + WW(j,RT)*WW(j,V)
      enddo

      C2 = C20
      ALPHA= RHO / C2

!C
!C-- {s}= {r} - ALPHA*{V}
      do j= 1, N
        WW(j,S)= WW(j,R) - ALPHA*WW(j,V)
      enddo

!C
!C +--------------------+
!C | {s_tld}= [Minv]{s} |
!C +--------------------+
!C===

      do i= 1, NP
        WW(i,ST)= WW(i,S)
      enddo

!C
!C-- incomplete CHOLESKY

      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      do i= 1, NP
        isL= INL(i-1) + 1
        ieL= INL(i  ) 
        WVAL= WW(i,ST)
        do j= isL, ieL
          inod = IAL(j)
          WVAL=  WVAL -  AL(j) * WW(inod,ST)
        enddo
        WW(i,ST)= WVAL * DD(i)
      enddo

      do i= NP, 1, -1
        SW  = 0.0d0

        isU= INU(i-1) + 1
        ieU= INU(i  ) 

        do j= isU, ieU
          inod = IAU(j)
          SW= SW + AU(j) * WW(inod,ST)
        enddo
        WW(i,ST)= WW(i,ST) - DD(i) * SW
      enddo
      endif

!C
!C-- diagonal
      if (PRECOND(1:4).eq.'DIAG') then
      do i= 1, N
        WW(i,ST)=  WW(i,ST) * DD(i)
      enddo
      endif      
!C===

!C
!C +------------------+
!C | {t} = [A]{s_tld} |
!C +------------------+
!C===

      do j= 1, N
        WVAL= D(j) * WW(j,ST)

        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          inod= IAU(i)
          WVAL= WVAL + AU(i) * WW(inod,ST)
        enddo

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          inod= IAL(i)
          WVAL= WVAL + AL(i) * WW(inod,ST)
        enddo
        WW(j,T)= WVAL
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
        C0(1)= C0(1) + WW(j,T) * WW(j,S)
        C0(2)= C0(2) + WW(j,T) * WW(j,T)
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
        X (j)  =  X(j)   + ALPHA*WW(j,PT) + OMEGA*WW(j,ST)
        WW(j,R)= WW(j,S )                 - OMEGA*WW(j,T)
        DNRM20 = DNRM20  + WW(j,S)**2
      enddo

      RHO1= RHO

      DNRM2 = DNRM20
      RESID= dsqrt(DNRM2/BNRM2)

        if (my_rank.eq.0 .and. MONITORFLAG.eq.1)                        &
     &    write (*, 1000) ITER, RESID
 1000   format ('BiCGSTAB_11: ', i5, 1pe16.6)

      if (RESID.le.TOL   ) exit
      if ( ITER.eq.MAXIT ) ERROR= -100
!C===

      enddo

      deallocate (WW)

      close (12)

      end subroutine        BiCGSTAB
      end module     solver_BiCGSTAB_single

