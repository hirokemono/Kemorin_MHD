!C
!C*** 
!C*** module solver_VCGnn_DJDS_SMP
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!!C
!!C
!!C***
!!C***  VCGnn_DJDS_SMP
!!C***
!!      subroutine VCGnn_DJDS_SMP                                       &
!!     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,        &
!!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,              &
!!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,         &
!!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,            &
!!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                    &
!!     &           STACK_IMPORT, NOD_IMPORT,                            &
!!     &           STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,       &
!!     &           SR_sig, SR_r, INITtime, COMPtime, COMMtime)
!!C
!!      subroutine init_VCGnn_DJDS_SMP                                  &
!!     &         (NP, NB, PEsmpTOT, PRECOND, iterPREmax, INITtime)
!!      subroutine solve_VCGnn_DJDS_SMP                                 &
!!     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,        &
!!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,              &
!!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,         &
!!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,            &
!!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                    &
!!     &           STACK_IMPORT, NOD_IMPORT,                            &
!!     &           STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,       &
!!     &           SR_sig, SR_r, COMPtime, COMMtime)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!C
!!C     VCGnn_DJDS_SMP solves the linear system Ax = b with n*n block
!!C     using the Conjugate Gradient iterative method with preconditioning.
!!C     Elements are ordered in descending Jagged Diagonal Storage
!!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!!C
      module solver_VCGnn_DJDS_SMP
!
      use m_precision
      use t_solver_SR
      use calypso_mpi
!
      implicit none
!
      integer(kind=kint), parameter, private :: nWK_CG =  4
      integer(kind = kint), private :: ntotWK_CG = nWK_CG + 3
      real(kind = kreal), allocatable, private :: W(:,:)
!
      private :: verify_work_4_matvecnn
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvecnn(NP, NB, nwk)
!
       integer(kind = kint), intent(in) :: NP, NB, nwk
!
      if(allocated(W) .eqv. .false.) then
        allocate ( W(NB*NP,nwk))
        W = 0.0d0
      else if(size(W) .lt. (nwk*NB*NP)) then
        deallocate (W)
        allocate ( W(NB*NP,nwk) )
        W = 0.0d0
      end if
!
      end subroutine verify_work_4_matvecnn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine VCGnn_DJDS_SMP                                         &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,         &
     &           SR_sig, SR_r, INITtime, COMPtime, COMMtime)
!
      integer(kind=kint ), intent(in) :: N, NP, NB, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: NEIBPETOT
      character(len=kchara ), intent(in):: PRECOND
      integer(kind=kint ), intent(inout) :: ITR, IER
      real   (kind=kreal), intent(in) :: EPS

      integer(kind=kint), intent(in) :: NtoO(NP)
      integer(kind=kint), intent(in) :: OtoN_L(NP), NtoO_U(NP)
      integer(kind=kint), intent(in) :: LtoU(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)
      integer(kind=kint), intent(in) :: IVECT(0:NVECT)
      integer(kind=kint), intent(in) :: NLhyp(NVECT), NUhyp(NVECT) 

      integer(kind=kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
      integer(kind=kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
      integer(kind=kint), intent(in) :: IAL(NPL)
      integer(kind=kint), intent(in) :: IAU(NPU)

      integer(kind=kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

      real(kind=kreal), intent(in) :: D(NB*NB*NP )
      real(kind=kreal), intent(in) :: AL(NB*NB*NPL)
      real(kind=kreal), intent(in) :: AU(NB*NB*NPU)

      real(kind=kreal), intent(in) :: ALU_L(NB*NB*N), ALU_U(NB*NB*N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT)) 
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      integer(kind=kint ), intent(in)  :: iterPREmax
!
      real(kind=kreal), intent(inout) :: B(NB*NP)
      real(kind=kreal), intent(inout) :: X(NB*NP)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for initialization
      real(kind = kreal), intent(inout) :: INITtime
!>      Elapsed time for solver iteration
      real(kind = kreal), intent(inout) :: COMPtime
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
!
      call init_VCGnn_DJDS_SMP(NP, NB, PEsmpTOT,                        &
     &                         PRECOND, iterPREmax, INITtime)
!
      call solve_VCGnn_DJDS_SMP                                         &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,         &
     &           SR_sig, SR_r, COMPtime, COMMtime)

      end subroutine VCGnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_VCGnn_DJDS_SMP                                    &
     &         (NP, NB, PEsmpTOT, PRECOND, iterPREmax, INITtime)
!
      use djds_matrix_calcs_nn
      use incomplete_cholesky_nn
      use i_cholesky_w_asdd_nn
      use block_ilu_nn
!
      integer(kind=kint ), intent(in) :: NP, NB, PEsmpTOT
      character(len=kchara), intent(in) :: PRECOND
      integer(kind=kint ), intent(in)  :: iterPREmax
!>      Elapsed time for initialization
      real(kind = kreal), intent(inout) :: INITtime
!
      real(kind = kreal) :: START_TIME
!
!
      START_TIME= MPI_WTIME()
      ntotWK_CG = nWK_CG + 3
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if(iterPREmax .ge. 1) ntotWK_CG = ntotWK_CG + 2
      end if
!
      call verify_work_4_matvecnn(NP,NB, ntotWK_CG)
      INITtime = MPI_WTIME() - START_TIME
!
      end subroutine init_VCGnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine solve_VCGnn_DJDS_SMP                                   &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,         &
     &           SR_sig, SR_r, COMPtime, COMMtime)
!
      use solver_SR_N
!
      use m_CG_constants
!
      use cal_norm_products_nn
      use vector_calc_solver_nn
      use djds_matrix_calcs_nn
      use incomplete_cholesky_nn
      use i_cholesky_w_asdd_nn
      use diagonal_scaling_nn
      use block_ilu_nn
      use calcs_4_CG
!
      integer(kind=kint ), intent(in) :: N, NP, NB, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: NEIBPETOT
      character(len=kchara ), intent(in):: PRECOND
      integer(kind=kint ), intent(inout) :: ITR, IER
      real   (kind=kreal), intent(in) :: EPS

      integer(kind=kint), intent(in) :: NtoO(NP)
      integer(kind=kint), intent(in) :: OtoN_L(NP), NtoO_U(NP)
      integer(kind=kint), intent(in) :: LtoU(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)
      integer(kind=kint), intent(in) :: IVECT(0:NVECT)
      integer(kind=kint), intent(in) :: NLhyp(NVECT), NUhyp(NVECT) 

      integer(kind=kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
      integer(kind=kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
      integer(kind=kint), intent(in) :: IAL(NPL)
      integer(kind=kint), intent(in) :: IAU(NPU)

      integer(kind=kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

      real(kind=kreal), intent(in) :: D(NB*NB*NP )
      real(kind=kreal), intent(in) :: AL(NB*NB*NPL)
      real(kind=kreal), intent(in) :: AU(NB*NB*NPU)

      real(kind=kreal), intent(in) :: ALU_L(NB*NB*N), ALU_U(NB*NB*N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT)) 
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))

      integer(kind=kint ), intent(in)  :: iterPREmax
!
      real(kind=kreal), intent(inout) :: B(NB*NP)
      real(kind=kreal), intent(inout) :: X(NB*NP)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for solver iteration
      real(kind = kreal), intent(inout) :: COMPtime
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME, S1_TIME
!
!
!
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: BNRM2,  DNRM2,  C1,  RHO, RHO1, ALPHA
      real(kind=kreal) :: BNRM20, DNRM20, C10, RHO0
!
      integer(kind=kint ) :: npLX1, npUX1
      integer(kind=kint ) :: iter, MAXIT

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
!
      npLX1= NL * PEsmpTOT
      npUX1= NU * PEsmpTOT

      MAXIT= ITR
      TOL  = EPS
      S1_TIME= MPI_WTIME()
      COMMtime = 0.0d0
!
!$omp workshare
      W(1:NB*NP,1:ntotWK_CG) = 0.0d0
!$omp end workshare
!
      call reset_solver_time
!C
!C-- change B,X

      call change_order_2_solve_bxn(NP, NB, PEsmpTOT, STACKmcG,         &
     &           NtoO, B, X, W(1,iWK))
!
!C
!C-- INTERFACE data EXCHANGE
!C===
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===

       call subtruct_matvec_nn                                          &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,R), B, X, W(1,iWK))
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===

      call cal_local_norm_n(NP, NB, PEsmpTOT, STACKmcG, B, BNRM20)

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      S1_TIME= MPI_WTIME()
      do iter= 1, MAXIT
!C
!C************************************************* Conjugate Gradient Iteration

!C
!C +----------------+
!C | {z}= [Minv]{r} |
!C +----------------+
!C===

        call clear_vector_solve_nn(NP, NB, W(1,Z) )

        if(PRECOND(1:2).eq.'IC'  .or.                                   &
     &     PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!
          if (iterPREmax.eq.1) then
            call incomplete_cholesky_1xnn                               &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,Z), W(1,R), W(1,iWK))
          else
!
            do iterPRE= 1, iterPREmax
              call i_cholesky_w_asdd_1xnn                               &
     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,   &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,ZQ), W(1,Z), W(1,R),     &
     &            W(1,iWK))

!C
!C-- INTERFACE data EXCHANGE
              START_TIME= MPI_WTIME()
              call SOLVER_SEND_RECV_N                                   &
     &           (NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,  &
     &            STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,ZQ))
              COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
!   additive SCHWARTZ domain decomposition
!
              call add_vector_nn(NP, NB, W(1,Z), W(1,ZQ) )
            end do
          end if

        else if (PRECOND(1:4).eq.'BLOC'                                 &
     &      .or. PRECOND(1:6).eq.'BL_ILU') then
          call block_ilu_1xnn                                           &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, W(1,Z), W(1,R), W(1,iWK))
!
!C===
!
        else if (PRECOND(1:4).eq.'DIAG') then
          call diag_scaling_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,         &
     &                           W(1,Z), W(1,R), ALU_L)
        end if
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
!
        call cal_local_s_product_n(NP, NB, PEsmpTOT, STACKmcG,          &
     &                             W(1,R), W(1,Z), RHO0)

        START_TIME= MPI_WTIME()
        call MPI_allREDUCE(RHO0, RHO, 1, CALYPSO_REAL,                  &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!C===

!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C | {p} = {z} + BETA*{p}        |
!C +-----------------------------+
!C===
        if ( ITER.eq.1 ) then
          call copy_internal_vect_n_smp(NP, NB, PEsmpTOT, STACKmcG,     &
     &                                  W(1,P), W(1,Z))
        else
          call djds_z_plus_beta_p_nn(NP, NB, PEsmpTOT, STACKmcG,        &
     &                               W(1,P), W(1,Z), RHO, RHO1)
        end if
!C===
!C
!C-- INTERFACE data EXCHANGE
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_N                                         &
     &     (NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
     &      STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,P))
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===

        call cal_matvec_nn                                              &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,Q), W(1,P), W(1,iWK))
!
!C===

!C
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
!
        call cal_local_s_product_n(NP, NB, PEsmpTOT, STACKmcG,          &
     &      W(1,P), W(1,Q), C10)
!

        START_TIME= MPI_WTIME()
        call MPI_allREDUCE(C10, C1, 1, CALYPSO_REAL,                    &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
        ALPHA= RHO / C1
!C===

!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C | norm= {r}^2          |
!C +----------------------+
!C===

        call djds_x_and_residual_CG_nn(NP, NB, PEsmpTOT, STACKmcG,      &
     &      DNRM20, X, W(1,R), W(1,P), W(1,Q), ALPHA)
!
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE(DNRM20, DNRM2, 1, CALYPSO_REAL,              &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
        RESID= dsqrt(DNRM2/BNRM2)

        if(IER.eq.1 .and. my_rank.eq.0) write (12,'(a23,i5,1p10e16.6)') &
     &    'solver_VCGnn_DJDS_SMP: ', ITER, RESID
!        if (my_rank.eq.0)  write (*,'(i5, 2(1pe16.6))') ITER, RESID
        ITR = ITER

        RHO1 = RHO

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if

      enddo

!C===
   30 continue

!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

!      call SOLVER_SEND_RECV_N                                          &
!     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
!     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,1))
!
!      call SOLVER_SEND_RECV_N                                          &
!     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
!     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,2))
!
!      call SOLVER_SEND_RECV_N                                          &
!     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
!     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,3))

!C
!C== change B,X

      call back_2_original_order_bxn(NP, NB, NtoO, B, X, W(1,iWK))
      COMPtime= MPI_WTIME() - S1_TIME
      IER = 0

      return
      end subroutine solve_VCGnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module solver_VCGnn_DJDS_SMP
