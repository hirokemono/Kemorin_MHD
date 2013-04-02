!C*** 
!C*** module solver_BiCGSTAB
!C***
!
!      subroutine BiCGSTAB                                              &
!     &                 (N, NP,  NPL, NPU,                              &
!     &                  D,  AL, INL, IAL, AU, INU, IAU,                &
!     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,              &
!     &                  EPS,  ITER, ERROR,                             &
!     &                  my_rank, NEIBPETOT, NEIBPE,                    &
!     &                  STACK_IMPORT, NOD_IMPORT,                      &
!     &                  STACK_EXPORT, NOD_EXPORT,                      &
!     &                  SOLVER_COMM , NSET)
!
!
!     BiCGSTAB solves the linear system Ax = b using the
!     Bi-Conjugate Gradient Stabilized iterative method with preconditioning.
!
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!     Modified by H. Matsui on jul. 2005 (ver 1.0)
!
      module solver_BiCGSTAB
!
      use m_precision
!
      implicit none
!
      real(kind=kreal), dimension(:), allocatable, private :: ALUG
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!C
!C*** BiCGSTAB
!C
      subroutine BiCGSTAB                                               &
     &                 (N, NP,  NPL, NPU,                               &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  EPS,  ITER, ERROR,                              &
     &                  my_rank, NEIBPETOT, NEIBPE,                     &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT,                       &
     &                  SOLVER_COMM , NSET)     
!
      use calypso_mpi
!
      use m_work_4_BiCGSTAB11
      use solver_SR
      use crs_matrix_calcs_11
      use incomplete_cholesky_crs_11
      use precond_crs_incomplete_lu
      use crs_norm_products_11
!

      real   (kind=kreal),                   intent(inout)::  EPS
      real   (kind=kreal),                   intent(in   )::  SIGMA_DIAG
      real   (kind=kreal),                   intent(in   )::  SIGMA
      integer(kind=kint ),                   intent(inout)::  ITER
      integer(kind=kint ),                   intent(inout)::  ERROR
      integer(kind=kint ),                   intent(in   )::  my_rank

      integer                              , intent(in)   :: SOLVER_COMM
      integer(kind=kint )                  , intent(in)   :: NSET

      integer(kind=kint ), intent(in   )::  NP, N, NPL, NPU
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

      integer(kind=kint ), intent(in) :: NEIBPETOT
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

      integer(kind=kint ) :: MAXIT, IFLAG, MONITORFLAG

      data IFLAG/0/

!C
!C-- INIT.
      call allocate_work_BiCGSTAB_11(NP,1)

      MONITORFLAG = ERROR
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 101
        return
      endif

      if (IFLAG.eq.0) then
        allocate (ALUG   (NP))
        IFLAG= 1
      endif

      MAXIT = ITER
      TOL   = EPS

      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, D, SOLVER_COMM,my_rank)

      if (NSET.ge.1) then
!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
        if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
          call precond_crs_ilu(N, NP, NPL, NPU, D, AL, INL,IAL,         &
     &      INU, AU, ALUG, SIGMA, SIGMA_DIAG)
        else if (PRECOND(1:4).eq.'SSOR'                                 &
     &       .or. PRECOND(1:4).eq.'DIAG') then
          call precond_crs_ssor(N, NP, D, ALUG, SIGMA_DIAG)
        end if
!C===
      end if

!C
!C +----------------------+
!C | {r}= {b} - [A]{xini} |
!C +----------------------+
!C===
      call subtruct_crs_matvec_11 (NP, N, NPL, NPU, INL, INU,           &
     &    IAL, IAU, D, AL, AU, W(1,R), B, X)

      call cal_local_norm_1(NP, N, B, BNRM20)
      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)
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
      call cal_local_s_product_1(NP, N, W(1,RT), W(1,R), RHO0)
      call MPI_allREDUCE (RHO0, RHO, 1, MPI_DOUBLE_PRECISION,           &
     &                    MPI_SUM, SOLVER_COMM, ierr)
!C===

!C
!C +----------------------------------------+
!C | BETA= (RHO/RHO1) * (ALPHA/OMEGA)       |
!C | {p} = {r} + BETA * ( {p} - OMEGA*{v} ) |
!C +----------------------------------------+
!C===
      if (iter.gt.1) then
        BETA= (RHO/RHO1) * (ALPHA/OMEGA)
        W(1:N,P)= W(1:N,R) + BETA*(W(1:N,P) - OMEGA*W(1:N,V))
      else
        W(1:N,P)= W(1:N,R)
      end if
!C===

!C
!C +--------------------+
!C | {p}= [Minv]{p_tld} |
!C +--------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,P) , SOLVER_COMM, my_rank)

!C
!C-- incomplete CHOLESKY

      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
          W(1:N,PT) =    W(1:N,P)
          W(N+1:NP,PT) = 0.d0
        call i_cholesky_crs_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      ALUG, AL, AU, W(1,PT) )
!C
!C-- diagonal
      else if (PRECOND(1:4).eq.'DIAG') then
        W(1:N,PT) =  W(1:N,P) * ALUG(1:N)
      endif
!C===

!C
!C +-------------------+
!C | {v} = [A] {p_tld} |
!C +-------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,PT) , SOLVER_COMM, my_rank)

      call cal_crs_matvec_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,       &
     &    D, AL, AU, W(1,V), W(1,PT))
!C===

!C
!C-- calc. ALPHA

      call cal_local_s_product_1(NP, N, W(1,RT), W(1,V), C20)
      call MPI_allREDUCE (C20, C2, 1, MPI_DOUBLE_PRECISION,             &
     &                    MPI_SUM, SOLVER_COMM, ierr) 
        ALPHA= RHO / C2

!C
!C-- {s}= {r} - ALPHA*{V}
        W(1:N,S)= W(1:N,R) - ALPHA*W(1:N,V)
!C
!C +--------------------+
!C | {s_tld}= [Minv]{s} |
!C +--------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,S) , SOLVER_COMM, my_rank)

!C
!C-- incomplete CHOLESKY

      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
          W(1:N,ST) =    W(1:N,S)
          W(N+1:NP,ST) = 0.d0
        call i_cholesky_crs_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      ALUG, AL, AU, W(1,ST)  )
!C
!C-- diagonal
      else if (PRECOND(1:4).eq.'DIAG') then
        W(1:N,ST)=  W(1:N,S) * ALUG(1:N)
      end if
!C===

!C
!C +------------------+
!C | {t} = [A]{s_tld} |
!C +------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE

      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,ST) , SOLVER_COMM, my_rank)

      call cal_crs_matvec_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,       &
     &    D, AL, AU, W(1,T), W(1,ST))
!C===

!C
!C +----------------------------+
!C | OMEGA= ({t}{s}) / ({t}{t}) |
!C +----------------------------+
!C===
        CG(1:2)= 0.d0
        call cal_local_sproduct_norm_1(NP, N,                           &
     &     W(1,T), W(1,S), C0(1), C0(2) )
        call MPI_allREDUCE (C0, CG, 2, MPI_DOUBLE_PRECISION,            &
     &                    MPI_SUM, SOLVER_COMM, ierr)
        OMEGA= CG(1) / CG(2)
!C===

!C
!C +----------------+
!C | update {x},{r} |
!C +----------------+
!C===
        RHO1 = RHO
!
        X (1:N)  =  X(1:N)   + ALPHA*W(1:N,PT) + OMEGA*W(1:N,ST)
        W(1:N,R) = W(1:N,S )                   - OMEGA*W(1:N,T)
!
        call cal_local_norm_1(NP, N,  W(1,S), DNRM20)
        call MPI_allREDUCE  (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,    &
     &                     MPI_SUM, SOLVER_COMM, ierr)
        RESID= dsqrt(DNRM2/BNRM2)

        if (my_rank.eq.0 .and. MONITORFLAG.eq.1)                        &
     &    write (*, 1000) ITER, RESID
 1000   format ('BiCGSTAB: ', i5, 1pe16.6)

        if (RESID.le.TOL   ) exit
        if ( ITER.eq.MAXIT ) ERROR= -100
      end do

!C
!C-- INTERFACE data EXCHANGE
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X, SOLVER_COMM, my_rank)
!
      end subroutine        BiCGSTAB
!
!  ---------------------------------------------------------------------
!
      end module     solver_BiCGSTAB

