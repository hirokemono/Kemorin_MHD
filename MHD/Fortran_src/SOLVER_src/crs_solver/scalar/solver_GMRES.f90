!
!  module solver_GMRES
!
!
!C*** 
!C*** module solver_GMRES
!C***
!
      module solver_GMRES
!
      use m_precision
      use m_constants
!
      implicit REAL*8(A-H,O-Z)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!C
!C*** GMRES
!C
      subroutine GMRES                                                  &
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
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!    \end{flushright}     

      use calypso_mpi
!
      use solver_SR
      use crs_matrix_calcs_11
      use crs_norm_products_11
      use incomplete_cholesky_crs_11
      use precond_crs_incomplete_lu
!

      real   (kind=kreal),                   intent(inout)::  RESID
      real   (kind=kreal),                   intent(in   )::  SIGMA_DIAG
      real   (kind=kreal),                   intent(in   )::  SIGMA
      integer(kind=kint ),                   intent(inout)::  ITER
      integer(kind=kint ),                   intent(inout)::  ERROR
      integer(kind=kint ),                   intent(in   )::  my_rank

      integer                              , intent(in)   :: SOLVER_COMM
      integer(kind=kint )                  , intent(in)   :: NSET

      integer(kind=kint )                  , intent(in)   :: NP, N
      integer(kind=kint )                  , intent(in)   :: NPL, NPU
!
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

      real   (kind=kreal), dimension(:), allocatable, save :: ALUG

      real   (kind=kreal), dimension(:,:), allocatable :: WW
      real   (kind=kreal), dimension(:),   allocatable :: SS
      real   (kind=kreal), dimension(:,:), allocatable :: H

      integer(kind=kint ) :: MAXIT, IFLAG, MONITORFLAG
      integer(kind=kint ) :: AV, CS, SN, R, S, V, W, Y
      real   (kind=kreal) :: TOL, WVAL

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
        allocate (ALUG   (NP))
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

       call SOLVER_SEND_RECV                                            &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, D, SOLVER_COMM,my_rank)
     
!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
      if (NSET.ge.1) then
        if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
          call precond_crs_ilu(N, NP, NPL, NPU, D, AL, INL,IAL,         &
     &        INU, AU, ALUG, SIGMA, SIGMA_DIAG)
        else if (PRECOND(1:4).eq.'SSOR'                                 &
     &    .or. PRECOND(1:4).eq.'DIAG') then
          call precond_crs_ssor(N, NP, D, ALUG, SIGMA_DIAG)
        endif
      endif
!C===

!C
!C +-----------------------+
!C | [M]{r}= {b} - [A]{x0} |
!C +-----------------------+
!C===
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X, SOLVER_COMM, my_rank)

        call subtruct_crs_matvec_11 (NP, N, NPL, NPU, INL, INU,         &
     &      IAL, IAU, D, AL, AU, WW(1,AV), B, X)

        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,AV), SOLVER_COMM, my_rank)

        WW(1:NP,R)= WW(1:NP,AV)

!C
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        call i_cholesky_crs_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      ALUG, AL, AU, WW(1,R) )
!C
!C-- diagonal
      else if (PRECOND(1:4).eq.'DIAG') then
        WW(1:N,R)=  WW(1:N,R) * ALUG(1:N)
      end if
!C===
      call cal_local_norm_1(NP, N, B, BNRM20)
      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      if (BNRM2.eq.ZERO) BNRM2= ONE
!C===
      ITER= 0
!C
!C************************************************ GMRES Iteration
!C
      do
        I= 0
!C
!C +---------------+
!C | {v1}= {r}/|r| |
!C +---------------+
!C===
        call cal_local_norm_1(NP, N, WW(1,R), DNRM20)
        call MPI_allREDUCE (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,     &
     &                      MPI_SUM, SOLVER_COMM, ierr)
!
        RNORM= dsqrt(DNRM2)
        coef= ONE/RNORM
        WW(1:N,V)= WW(1:N,R) * coef
!C===

!C
!C +--------------+
!C | {s}= |r|{e1} |
!C +--------------+
!C===
        WW(1,S) =   RNORM
        WW(2:N,S) = ZERO
!C===

!C************************************************ GMRES(m) restart
        do I = 1, NREST
          ITER= ITER + 1
!C
!C +----------------+
!C | [M]{w}= [A]{v} |
!C +----------------+
!C===
          call SOLVER_SEND_RECV                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,V+I-1), SOLVER_COMM, my_rank)

          call cal_crs_matvec_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,   &
     &        D, AL, AU, WW(1,W), WW(1,V+I-1))
!
          call SOLVER_SEND_RECV                                         &
     &     ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &     STACK_EXPORT, NOD_EXPORT, WW(1,W), SOLVER_COMM, my_rank)

!C
!C-- incomplete CHOLESKY
      
          if (PRECOND(1:2).eq.'IC'  .or.                                &
     &        PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
            call i_cholesky_crs_11(NP, N, NPL, NPU, INL, INU, IAL, IAU, &
     &          ALUG, AL, AU, WW(1,W) )
!C
!C-- diagonal
          else if (PRECOND(1:4).eq.'DIAG') then
            WW(1:N,W)=  WW(1:N,W) * ALUG(1:N)
          end if
!C===

!C
!C +------------------------------+
!C | ORTH. BASIS by GRAMM-SCHMIDT |
!C +------------------------------+
!C   Construct the I-th column of the upper Hessenberg matrix H
!C   using the Gram-Schmidt process on V and W.
!C===
          do K= 1, I
            call cal_local_s_product_1(NP, N, WW(1,W), WW(1,V+K-1),     &
     &          VAL0)
            call MPI_allREDUCE (VAL0, VAL, 1, MPI_DOUBLE_PRECISION,     &
     &          MPI_SUM, SOLVER_COMM, ierr)
!
            WW(1:N,W)= WW(1:N,W) - VAL * WW(1:N,V+K-1)
            H(K,I) = VAL
          end do
!
          call cal_local_norm_1(NP, N, WW(1,W), VAL0)
          call MPI_allREDUCE (VAL0, VAL, 1, MPI_DOUBLE_PRECISION,       &
     &       MPI_SUM, SOLVER_COMM, ierr)

          H(I+1,I)= dsqrt(VAL)
          coef= ONE / H(I+1,I)
          WW(1:N,V+I+1-1)= WW(1:N,W) * coef

!C===

!C
!C +-----------------+
!C | GIVENS ROTARION |
!C +-----------------+
!C===

!C
!C-- Plane Rotation
          do k = 1, I-1 
            DTEMP   = H(k,CS)*H(k  ,I) + H(k,SN)*H(k+1,I)
            H(k+1,I)= H(k,CS)*H(k+1,I) - H(k,SN)*H(k  ,I)
            H(k  ,I)= DTEMP
          end do

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
          end if

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

          RESID = dabs (WW(I+1,S))/dsqrt(BNRM2)

          if (my_rank.eq.0 .and. MONITORFLAG.eq.1)                      &
     &      write (*, 1000) ITER, RESID
 1000     format ('GMRES: ', i5, 1pe16.6)

          if ( RESID .le. TOL) then
!C-- [H]{y}= {s_tld}
            IROW= I
            SS(1:IROW)= WW(1:IROW,S)
            WW(IROW,Y)= SS(IROW) / H(IROW,IROW)

            do kk= IROW-1, 1, -1
              do jj= IROW, kk+1, -1
                SS(kk)= SS(kk) - H(kk,jj)*WW(jj,Y)
              enddo
              WW(kk,Y)= SS(kk) / H(kk,kk)
            enddo

!C-- {x}= {x} + {y}{V}
            do jj = 1, IROW
              X(1:N)= X(1:N) + WW(jj,Y)*WW(1:N,V+jj-1)
            end do

            goto 70
          end if

          if(ITER .gt. MAXIT) exit
        end do
!C===

!C
!C +------------------+
!C | CURRENT SOLUTION |
!C +------------------+
!C===

!C-- [H]{y}= {s_tld}
        SS(1:NREST)= WW(1:NREST,S)
        IROW= NREST
        WW(IROW,Y)= SS(IROW) / H(IROW,IROW)

        do kk= IROW-1, 1, -1
          do jj= IROW, kk+1, -1
            SS(kk)= SS(kk) - H(kk,jj)*WW(jj,Y)
          end do
          WW(kk,Y)= SS(kk) / H(kk,kk)
        end do
!C-- {x}= {x} + {y}{V}
        do jj= 1, IROW
          X(1:N)= X(1:N) + WW(jj,Y)*WW(1:N,V+jj-1)
        end do

!C
!C-- Compute residual vector R, find norm, then check for tolerance.        

        call SOLVER_SEND_RECV                                           &
     &      ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,          &
     &        STACK_EXPORT, NOD_EXPORT, X, SOLVER_COMM, my_rank)

        call subtruct_crs_matvec_11 (NP, N, NPL, NPU, INL, INU,         &
     &      IAL, IAU, D, AL, AU, WW(1,AV), B, X)

        WW(1:N,R)= WW(1:N,AV)

        call cal_local_norm_1(NP, N,  WW(1,R), DNRM20)
        call MPI_allREDUCE  (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,    &
     &                       MPI_SUM, SOLVER_COMM, ierr)                

        WW(I+1,S)= dsqrt(DNRM2/BNRM2)
        RESID    = WW( I+1,S )

        if(ITER .gt. MAXIT) exit

        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &    STACK_EXPORT, NOD_EXPORT, WW(1,R), SOLVER_COMM, my_rank)

!C
!C-- incomplete CHOLESKY
        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
          call i_cholesky_crs_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,   &
     &        ALUG, AL, AU, WW(1,R) )
!C
!C-- diagonal
        else if (PRECOND(1:4).eq.'DIAG') then
          WW(1:N,R)=  WW(1:N,R) * ALUG(1:N)
        end if
!C
!C-- RESTART
      end do
!C
!C-- iteration FAILED
!
      INFO = ITER

!C-- [H]{y}= {s_tld}
      IROW = I
      SS(1:IROW)= WW(1:IROW,S)
      WW(IROW,Y)= SS(IROW) / H(IROW,IROW)

      do kk= IROW-1, 1, -1
        do jj= IROW, kk+1, -1
          SS(kk)= SS(kk) - H(kk,jj)*WW(jj,Y)
        end do
        WW(kk,Y)= SS(kk) / H(kk,kk)
      end do

!C-- {x}= {x} + {y}{V}
      do jj= 1, IROW
        X(1:N)= X(1:N) + WW(jj,Y)*WW(1:N,V+jj-1)
      end do

   70 continue
!C
!C-- INTERFACE data EXCHANGE
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X, SOLVER_COMM,my_rank)

      deallocate (H)
      deallocate (WW)
      deallocate (SS)

      if (ITER .ge. MAXIT) ERROR= -100

      end subroutine        GMRES
!
!-----------------------------------------------------------------------
!
      end module     solver_GMRES
