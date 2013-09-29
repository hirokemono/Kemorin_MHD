!
!C*** 
!C*** module solver_CG
!C***
!C
!C*** CG
!C
!      subroutine CG    (N, NP,  NPL, NPU,                              &
!     &                  D,  AL, INL, IAL, AU, INU, IAU,                &
!     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,              &
!     &                  EPS,  ITER, ERROR, NEIBPETOT, NEIBPE,          &
!     &                  STACK_IMPORT, NOD_IMPORT,                      &
!     &                  STACK_EXPORT, NOD_EXPORT, NSET)
!
!     CG solves the linear system Ax = b using the
!     Conjugate Gradient iterative method with preconditioning.
!
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!
      module solver_CG
!
      use m_precision
!
      implicit none
!
      real(kind=kreal), allocatable, private :: ALUG(:)
      real(kind=kreal), allocatable, private :: SCALE(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine CG    (N, NP,  NPL, NPU,                               &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  EPS,  ITER, ERROR, NEIBPETOT, NEIBPE,           &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT, NSET)
!
      use calypso_mpi
!
      use m_work_4_CG11
      use solver_SR
      use crs_matrix_calcs_11
      use incomplete_cholesky_crs_11
      use precond_crs_incomplete_lu
      use calcs_4_crs_CG11
      use crs_norm_products_11
!
      real   (kind=kreal),                   intent(inout)::  EPS
      real   (kind=kreal),                   intent(in   )::  SIGMA_DIAG
      real   (kind=kreal),                   intent(in   )::  SIGMA
      integer(kind=kint ),                   intent(inout)::  ITER
      integer(kind=kint ),                   intent(inout)::  ERROR

      integer(kind=kint )                  , intent(in)   :: NSET

      integer(kind=kint ), intent(in   )::  NP, N, NPL, NPU
      real   (kind=kreal), dimension(NP )  , intent(inout)::  D
      real   (kind=kreal), dimension(NP )  , intent(inout)::  B
      real   (kind=kreal), dimension(NP )  , intent(inout)::  X

      real   (kind=kreal), dimension(NPU)  , intent(inout)::  AU
      real   (kind=kreal), dimension(NPL)  , intent(inout)::  AL

      integer(kind=kint ), dimension(0:NP)   , intent(in) ::  INU
      integer(kind=kint ), dimension(0:NP)   , intent(in) ::  INL
      integer(kind=kint ), dimension(  NPU)  , intent(in) ::  IAU
      integer(kind=kint ), dimension(  NPL)  , intent(in) ::  IAL
      character(len=kchara)                      , intent(in) :: PRECOND

      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE      (NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in) :: NOD_IMPORT  (:)
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in) :: NOD_EXPORT  (:)

      integer(kind = kint) :: MAXIT, IFLAG
      data IFLAG/0/
!C
!C-- INIT. 
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 101
        return
      endif


      if (IFLAG.eq.0) then
        allocate (ALUG   (NP))
        allocate (SCALE(NP))
        IFLAG= 1
        SCALE(1:NP)= 1.d0
!
        call allocate_work_CG_11(NP, 1)
      else
        call verify_work_CG_11(NP, 1)
      end if

      MAXIT  = ITER
      TOL   = EPS

      if (NSET.ge.1) then
!C
!C-- SCALING
        if (NSET.eq.2) then
          SCALE(1:N)= 1.d0 / dsqrt(dabs(D(1:N)))
!
          call SOLVER_SEND_RECV                                         &
     &       ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &         STACK_EXPORT, NOD_EXPORT, SCALE)
!
          call mat_scaling_crs_ilu(N, NP, NPL, NPU, D, AL, INL, IAL,    &
     &         INU, IAU, AU, SCALE)
        end if
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
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
      if(NSET.eq.2) B(1:N)= B(1:N) * SCALE(1:N)
!C
!C-- INTERFACE data EXCHANGE
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)
!C
      call subtruct_crs_matvec_11 (NP, N, NPL, NPU, INL, INU, IAL, IAU, &
     &    D, AL, AU, W(1,R), B, X)
!
!
      call cal_local_norm_1(NP, N, B, BNRM20)
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
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
!C-- incomplete CHOLESKY
      
        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
          W(1:N,Z) =    W(1:N,R)
          W(N+1:NP,Z) = 0.d0
          call i_cholesky_crs_11(NP, N, NPL, NPU,                       &
     &        INL, INU, IAL, IAU, ALUG, AL, AU, W(1,Z) )
!C
!C-- diagonal
        else if (PRECOND(1:4).eq.'DIAG') then
          W(1:N,Z)=  W(1:N,R) * ALUG(1:N)
        end if
!C===
      
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
        call cal_local_s_product_1(NP, N, W(1,R), W(1,Z), RHO0)
        call MPI_allREDUCE (RHO0, RHO, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
!C===

!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C +-----------------------------+
!C===
        if ( ITER.eq.1 ) then
          W(1:N,P)= W(1:N,Z)
        else
          call crs_z_plus_beta_p_11(N, NP, W(1,P), W(1,Z), RHO, RHO1)
        end if
!C===

!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===        
!C
!C-- INTERFACE data EXCHANGE

        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,P) )

        call cal_crs_matvec_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      D, AL, AU, W(1,Q), W(1,P) )
!C
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
!
        call cal_local_s_product_1(NP, N, W(1,P), W(1,Q), C10)
        call MPI_allREDUCE (C10, C1, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
        ALPHA= RHO / C1
        RHO1 = RHO
!C===

!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C +----------------------+
!C===
        X(1:N) =     X(1:N) + ALPHA * W(1:N,P)
        W(1:N,R) = W(1:N,R) - ALPHA * W(1:N,Q)

        call cal_local_norm_1(NP, N,  W(1,R), DNRM20)
        call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,             &
     &                    MPI_SUM, CALYPSO_COMM, ierr)

        RESID = dsqrt(DNRM2/BNRM2)

        if (my_rank.eq.0) write (12, 1000) ITER, RESID
 1000   format ('solver_CG', i5, 1pe16.6)

        if ( RESID.le.TOL   ) exit
        if ( ITER .eq.MAXIT ) ERROR= -100
      end do
!C===

!C
!C-- INTERFACE data EXCHANGE
      if (NSET.eq.2) then
        X(1:N)= X(1:N) * SCALE(1:N)
        B(1:N)= B(1:N) / SCALE(1:N)
      end if

      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)
!
      end subroutine        CG
!
!  ---------------------------------------------------------------------
!
      end module     solver_CG
