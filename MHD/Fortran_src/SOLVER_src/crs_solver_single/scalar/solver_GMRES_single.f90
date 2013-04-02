!
!  module solver_GMRES_single
!
!C*** 
!C*** module solver_GMRES_single
!C***
!
      module solver_GMRES_single
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
!C*** GMRES
!C
      subroutine GMRES                                                  &
     &                 (N, NP,  NPL, NPU,                               &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA, NREST,        &
     &                  RESID,  ITER, ERROR,                            &
     &                  my_rank, NSET)     

! \beginSUBROUTINE
!     GMRES solves the linear system Ax = b using the
!     Generalized Minimal RESidual iterative method with preconditioning.
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!    \end{flushright}     

      implicit REAL*8(A-H,O-Z)

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
      real   (kind=kreal), dimension(:),   allocatable :: SS
      real   (kind=kreal), dimension(:,:), allocatable :: H

      integer(kind=kint ) :: IFLAG, MONITORFLAG
      integer(kind=kint ) :: AV, CS, SN, R, S, V, W, Y, MAXIT
      real   (kind=kreal) :: TOL, WVAL

      real   (kind=kreal)   ZERO, ONE
      parameter ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      data IFLAG/0/

!C
!C-- INIT.
      MONITORFLAG = ERROR
      ERROR= 0
      NRK  = NREST + 4

      allocate (H (NRK,NRK))
      allocate (WW(NP,NRK))
      allocate (SS(NRK))

      LDH= NREST + 2
      LDW= N

      MAXIT= ITER
      TOL  = RESID

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 101
        return
      endif

      if (IFLAG.eq.0) then
        allocate (DD   (NP))
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
        DD(ik)= 0.d0
      enddo

      if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
        do ik= 1, NP
          isL= INL(ik-1) + 1
          ieL= INL(ik)
          WVAL= D(ik) * SIGMA_DIAG
          do k= isL, ieL
            SVAL=  AL(k)
            id= IAL(k)

            isU= INU(id-1) + 1
            ieU= INU(id)
            do kk= isU, ieU
              SVAL= SVAL + AU(kk) * SIGMA
            enddo
            WVAL= WVAL - AL(k)*SVAL*DD(id)
          enddo
          DD(ik)= 1.d0 / WVAL
        enddo
      endif

      if (PRECOND(1:4).eq.'SSOR' .or. PRECOND(1:4).eq.'DIAG') then
        do ik= 1, NP
          DD(ik)= 1.d0/(SIGMA_DIAG*D(ik))
        enddo
      endif
!C===
      endif

!C
!C +-----------------------+
!C | [M]{r}= {b} - [A]{x0} |
!C +-----------------------+
!C===
      do j= 1, N
        WVAL= B(j) - D(j) * X(j)

        isU= INU(j-1)+1
        ieU= INU(j)
        do ik= isU, ieU
          inod= IAU(ik)
          WVAL= WVAL - AU(ik) * X(inod)
        enddo

        isL= INL(j-1)+1
        ieL= INL(j)
        do ik= isL, ieL
          inod= IAL(ik)
          WVAL= WVAL - AL(ik) * X(inod)
        enddo
        WW(j,AV)= WVAL
      enddo

      do ik= 1, NP
        WW(ik,R)= WW(ik,AV)
      enddo

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      do ik= 1, NP
        isL= INL(ik-1) + 1
        ieL= INL(ik  ) 
        WVAL= WW(ik,R)
        do j= isL, ieL
          inod = IAL(j)
          WVAL=  WVAL -  AL(j) * WW(inod,R)
        enddo
        WW(ik,R)= WVAL * DD(ik)
      enddo

      do ik= NP, 1, -1
        SW  = 0.0d0
        isU= INU(ik-1) + 1
        ieU= INU(ik  ) 
        do j= isU, ieU
          inod = IAU(j)
          SW= SW + AU(j) * WW(inod,R)
        enddo
        WW(ik,R)= WW(ik,R) - DD(ik) * SW
      enddo
      endif

!C
!C-- diagonal
      if (PRECOND(1:4).eq.'DIAG') then
      do ik= 1, N
        WW(ik,R)=  WW(ik,R) * DD(ik)
      enddo
      endif      
!C===
      BNRM20= ZERO
      do ik= 1, N
        BNRM20= BNRM20 + B(ik)**2
      enddo

      BNRM2 = BNRM20
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
          DNRM20= DNRM20 + WW(ik,R)**2
        enddo
        DNRM2 = DNRM20

        RNORM= dsqrt(DNRM2)
        coef= ONE/RNORM
        do ik= 1, N
          WW(ik,V)= WW(ik,R) * coef
        enddo
!C===

!C
!C +--------------+
!C | {s}= |r|{e1} |
!C +--------------+
!C===
        WW(1,S) = RNORM         
        do k = 2, N
          WW(k,S) = ZERO
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
      do j= 1, N
        WVAL= D(j) * WW(j,V+I-1)

        isU= INU(j-1)+1
        ieU= INU(j)
        do ik= isU, ieU
          inod= IAU(ik)
          WVAL= WVAL + AU(ik) * WW(inod,V+I-1)
        enddo

        isL= INL(j-1)+1
        ieL= INL(j)
        do ik= isL, ieL
          inod= IAL(ik)
          WVAL= WVAL + AL(ik) * WW(inod,V+I-1)
        enddo
        WW(j,W)= WVAL
      enddo

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      do ik= 1, NP
        isL= INL(ik-1) + 1
        ieL= INL(ik  ) 
        WVAL= WW(ik,W)
        do j= isL, ieL
          inod = IAL(j)
          WVAL=  WVAL -  AL(j) * WW(inod,W)
        enddo
        WW(ik,W)= WVAL * DD(ik)
      enddo

      do ik= NP, 1, -1
        SW  = 0.0d0
        isU= INU(ik-1) + 1
        ieU= INU(ik  ) 
        do j= isU, ieU
          inod =  IAU(j)
          SW= SW + AU(j) * WW(inod,W)
        enddo
        WW(ik,W)= WW(ik,W) - DD(ik) * SW
      enddo
      endif

!C
!C-- diagonal
      if (PRECOND(1:4).eq.'DIAG') then
      do ik= 1, N
        WW(ik,W)=  WW(ik,W) * DD(ik)
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
            VAL0= VAL0 + WW(ik,W)*WW(ik,V+K-1)
          enddo
          VAL = VAL0

          do ik= 1, N
            WW(ik,W)= WW(ik,W) - VAL * WW(ik,V+K-1)
          enddo
          H(K,I)= VAL
        enddo
        
        VAL0= 0.d0
        do ik= 1, N
          VAL0= VAL0 + WW(ik,W)**2
        enddo
        VAL = VAL0

        H(I+1,I)= dsqrt(VAL)
        coef= ONE / H(I+1,I)
        do ik= 1, N
          WW(ik,V+I+1-1)= WW(ik,W) * coef
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

           if (my_rank.eq.0 .and. MONITORFLAG.eq.1)                     &
     &    write (*, 1000) ITER, RESID
 1000   format ('GMRES_11: ', i5, 1pe16.6)

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
                X(kk)= X(kk) + WW(jj,Y)*WW(kk,V+jj-1)
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
                X(kk)= X(kk) + WW(jj,Y)*WW(kk,V+jj-1)
              enddo
              enddo

!C
!C-- Compute residual vector R, find norm, then check for tolerance.        

      do j= 1, N
        WVAL= B(j) - D(j) * X(j)
        isU= INU(j-1)+1
        ieU= INU(j)
        do ik= isU, ieU
          inod= IAU(ik)
          WVAL= WVAL - AU(ik) * X(inod)
        enddo

        isL= INL(j-1)+1
        ieL= INL(j)
        do ik= isL, ieL
          inod= IAL(ik)
          WVAL= WVAL - AL(ik) * X(inod)
        enddo
        WW(j,AV)= WVAL
      enddo

      do ik= 1, N
        WW(ik,R)= WW(ik,AV)
      enddo

        DNRM20= ZERO
        do ik= 1, N
          DNRM20= DNRM20 + WW(ik,R)**2
        enddo

        DNRM2 = DNRM20

        WW(I+1,S)= dsqrt(DNRM2/BNRM2)
        RESID    = WW( I+1,S )

!        if ( RESID.le.TOL )   goto 70
        if ( ITER .gt.MAXIT ) goto 60

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
      do ik= 1, NP
        isL= INL(ik-1) + 1
        ieL= INL(ik  ) 
        WVAL= WW(ik,R)
        do j= isL, ieL
          inod = IAL(j)
          WVAL=  WVAL -  AL(j) * WW(inod,R)
        enddo
        WW(ik,R)= WVAL * DD(ik)
      enddo

      do ik= NP, 1, -1
        SW  = 0.0d0
        isU= INU(ik-1) + 1
        ieU= INU(ik  ) 
        do j= isU, ieU
          inod =  IAU(j)
          SW= SW + AU(j) * WW(inod,R)
        enddo
        WW(ik,R)= WW(ik,R) - DD(ik) * SW
      enddo
      endif

!C
!C-- diagonal
      if (PRECOND(1:4).eq.'DIAG') then
      do ik= 1, N
        WW(ik,R)=  WW(ik,R) * DD(ik)
      enddo
      endif      

!C
!C-- RESTART
        goto 10

!C
!C-- iteration FAILED

   60 continue
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
                X(kk)= X(kk) + WW(jj,Y)*WW(kk,V+jj-1)
              enddo
              enddo

   70 continue
!C
!C-- INTERFACE data EXCHANGE

      deallocate (H)
      deallocate (WW)
      deallocate (SS)

      if ( ITER.ge.MAXIT ) ERROR= -100

      end subroutine        GMRES
      end module     solver_GMRES_single

