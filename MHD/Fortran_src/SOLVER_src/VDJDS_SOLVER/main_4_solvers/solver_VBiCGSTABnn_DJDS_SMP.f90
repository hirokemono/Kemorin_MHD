!C
!C*** 
!C*** module solver_VBiCGSTABnn_DJDS_SMP
!C***
!
!      Written by H. Matsui on Nov. 2005
!
!!C
!!C***
!!C***  VBiCGSTABnn_DJDS_SMP
!!C***
!!      subroutine VBiCGSTABnn_DJDS_SMP                                 &
!!     &          (N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,        &
!!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,              &
!!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,         &
!!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,            &
!!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                    &
!!     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!!     &           PRECOND, iterPREmax, SR_sig, SR_r)
!!C
!!      subroutine init_VBiCGSTABnn_DJDS_SMP                            &
!!     &         (NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!!      subroutine solve_VBiCGSTABnn_DJDS_SMP                           &
!!     &          (N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,        &
!!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,              &
!!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,         &
!!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,            &
!!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                    &
!!     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!!     &           PRECOND, iterPREmax, SR_sig, SR_r)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!C
!!C     VBiCGSTABnn_DJDS_SMP solves the linear system Ax = b
!!C     using the BiCGSTAB iterative method with preconditioning
!!C     for n*n Block Matrix by OVERLAPPING method.
!!C     Elements are ordered in descending Jagged Diagonal Storage
!!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!!C
      module solver_VBiCGSTABnn_DJDS_SMP
!
      use m_precision
      use t_solver_SR
!
      implicit none
!
      real(kind = kreal), allocatable :: W(:,:)
      private :: W
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
      subroutine VBiCGSTABnn_DJDS_SMP                                   &
     &          (N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r)
!
      integer(kind=kint ), intent(in) :: N, NP, NB, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: NEIBPETOT
      character(len=kchara), intent(in)  :: PRECOND
      real   (kind=kreal), intent(in) :: EPS
!
      integer(kind=kint ), intent(inout) :: ITR, IER
!
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
!>      Structure of communication buffer for 8-byte integer
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call init_VBiCGSTABnn_DJDS_SMP                                    &
     &   (NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!
      call solve_VBiCGSTABnn_DJDS_SMP                                   &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r)
!
      end subroutine VBiCGSTABnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_VBiCGSTABnn_DJDS_SMP                              &
     &         (NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!
      use m_work_4_BiCGSTAB
!
      use djds_matrix_calcs_nn
      use incomplete_cholesky_nn
      use i_cholesky_w_asdd_nn
      use block_ilu_nn
!
      integer(kind=kint ), intent(in) :: NP, NB, PEsmpTOT
      character(len=kchara), intent(in) :: PRECOND
      integer(kind=kint ), intent(in)  :: iterPREmax
!
      integer(kind=kint ) :: nwk
!
!   allocate work arrays
!
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if(iterPREmax .ge. 1) ntotWK_BiCGSTAB = ntotWK_BiCGSTAB + 2
      end if
!
      call verify_work_4_matvecnn(NP,NB, ntotWK_BiCGSTAB)
!
      end subroutine init_VBiCGSTABnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine solve_VBiCGSTABnn_DJDS_SMP                             &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r)

      use calypso_mpi
!
      use t_solver_SR
      use solver_SR_N
!
      use m_work_4_BiCGSTAB
      use m_solver_count_time
!
      use cal_norm_products_nn
      use vector_calc_solver_nn
      use djds_matrix_calcs_nn
      use incomplete_cholesky_nn
      use i_cholesky_w_asdd_nn
      use block_ilu_nn
      use diagonal_scaling_nn
      use calcs_4_BiCGSTAB
!
      integer(kind=kint ), intent(in) :: N, NP, NB, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: NEIBPETOT
      character(len=kchara), intent(in)  :: PRECOND
      real   (kind=kreal), intent(in) :: EPS
!
      integer(kind=kint ), intent(inout) :: ITR, IER
!
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
!
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
!>      Structure of communication buffer for 8-byte integer
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind=kint ) :: npLX1, npUX1
      integer(kind=kint ) :: iter, MAXIT

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      npLX1= NL * PEsmpTOT
      npUX1= NU * PEsmpTOT

      MAXIT= ITR
      TOL  = EPS
      S1_TIME= MPI_WTIME()
!
!$omp workshare
      W(1:NB*NP,1:nWK_BiCGSTAB) = 0.0d0
!$omp end workshare
!
      call reset_solver_time
!C
!C-- change B,X
!
      call change_order_2_solve_bxn(NP, NB, PEsmpTOT, STACKmcG,         &
     &           NtoO, B, X, W(1,iWK))

!C
!C-- INTERFACE data EXCHANGE
!
!C===
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C | {rt}= {b} - [A]{xini} |
!C +-----------------------+
!C===
!
       call subtruct_matvec_nn                                          &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,R), B, X, W(1,iWK))
!
       call copy_vector_nn(NP, NB, W(1,RT), W(1,R) )
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
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do iter= 1, MAXIT
!C
!C************************************************* 
!C       Conjugate Gradient Iteration

!C
!C +-------------------+
!C | {RHO}= {r}{r_tld} |
!C +-------------------+
!C===
!
        call cal_local_s_product_n(NP, NB, PEsmpTOT, STACKmcG,          &
     &    W(1,R), W(1,RT), RHO0)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (RHO0, RHO, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C===

!C
!C
!C +----------------------------------------+
!C | BETA= (RHO/RHO1) * (ALPHA/OMEGA)       |
!C | {p} = {r} + BETA * ( {p} - OMEGA*{v} ) |
!C +----------------------------------------+
!C===
        if (iter.eq.1) then
          call copy_internal_vect_n_smp(NP, NB, PEsmpTOT, STACKmcG,     &
     &      W(1,P), W(1,R) )
        else
          call r_plus_beta_p_sub_omega_v_nn(NP, NB, PEsmpTOT, STACKmcG, &
     &       W(1,P), W(1,R), W(1,V), OMEGA, ALPHA, RHO, RHO1)
        endif
!C===

!C
!C +--------------------+
!C | {p_tld}= [Minv]{p} |
!C +--------------------+
!C===
        call clear_vector_solve_nn(NP, NB, W(1,PT) )

        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!
          if (iterPREmax.eq.1) then
            call incomplete_cholesky_1xnn                               &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,PT), W(1,P), W(1,iWK))
          else
!
            do iterPRE= 1, iterPREmax
!
              call i_cholesky_w_asdd_1xnn                               &
     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,   &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,ZQ), W(1,PT), W(1,P),    &
     &            W(1,iWK))

!C
!C-- INTERFACE data EXCHANGE
              START_TIME= MPI_WTIME()
              call SOLVER_SEND_RECV_N                                   &
     &           (NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT,              &
     &            NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,                 &
     &            SR_sig, SR_r, W(1,ZQ))
              END_TIME= MPI_WTIME()
              COMMtime = COMMtime + END_TIME - START_TIME
!
!   additive SCHWARTZ domain decomposition
!
              call add_vector_nn(NP, NB, W(1,PT), W(1,ZQ) )
!
            enddo
          end if
!
        else if (PRECOND(1:4).eq.'BLOC'                                 &
     &      .or. PRECOND(1:6).eq.'BL_ILU') then
          call block_ilu_1xnn                                           &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, W(1,PT), W(1,P), W(1,iWK))
!
        else if (PRECOND(1:4).eq.'DIAG') then
          call diag_scaling_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,         &
     &      W(1,PT), W(1,P), ALU_L)
        endif
!C===

!C
!C-- INTERFACE data EXCHANGE
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_N                                         &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,PT))
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
 
!C
!C +-----------------+
!C | {v}= [A]{p_tld} |
!C +-----------------+
!C===        
!
        call cal_matvec_nn                                              &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,V), W(1,PT), W(1,iWK))
!
!C===

!C
!C +----------------+
!C | ALPHA = RHO/C2 |
!C +----------------+
!C===
!
        call cal_local_s_product_n(NP, NB, PEsmpTOT, STACKmcG,          &
     &      W(1,RT), W(1,V), C20)

        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C20, C2, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
        ALPHA= RHO / C2
!C===

!C
!C +----------------------+
!C | {s}= {r} - ALPHA*{v} |
!C +----------------------+
!C===

        call r_sub_alpha_v_nn(NP, NB, PEsmpTOT, STACKmcG, W(1,S),       &
     &      W(1,R), W(1,V), ALPHA)
!
!        call cal_local_norm_n(NP, NB, PEsmpTOT, STACKmcG, W(1,S), aa)
!
!C===

!C
!C +--------------------+
!C | {s_tld}= [Minv]{s} |
!C +--------------------+
!C===
!
        call clear_vector_solve_nn(NP, NB, W(1,ST) )

        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!
          if (iterPREmax.eq.1) then
            call incomplete_cholesky_1xnn                               &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,ST), W(1,S), W(1,iWK))
          else
!
            do iterPRE= 1, iterPREmax
!
              call i_cholesky_w_asdd_1xnn                               &
     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,   &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,ZQ), W(1,ST), W(1,S),    &
     &            W(1,iWK))

!C
!C-- INTERFACE data EXCHANGE
              START_TIME= MPI_WTIME()
              call SOLVER_SEND_RECV_N                                   &
     &           (NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT,              &
     &            NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,                 &
     &            SR_sig, SR_r, W(1,ZQ))
              END_TIME= MPI_WTIME()
              COMMtime = COMMtime + END_TIME - START_TIME
!
!   additive SCHWARTZ domain decomposition
!
              call add_vector_nn(NP, NB, W(1,ST), W(1,ZQ) )
!
            enddo
          end if
!
        else if (PRECOND(1:4).eq.'BLOC'                                 &
     &      .or. PRECOND(1:6).eq.'BL_ILU') then
          call block_ilu_1xnn                                           &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, W(1,ST), W(1,S), W(1,iWK))
!
        else if (PRECOND(1:4).eq.'DIAG') then
          call diag_scaling_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,         &
     &      W(1,ST), W(1,S), ALU_L)
        endif
!
!C===
!C
!C-- INTERFACE data EXCHANGE
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_N                                         &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,ST))
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
 
!C
!C +-----------------+
!C | {t}= [A]{s_tld} |
!C +-----------------+
!C===

        call cal_matvec_nn                                              &
     &           (NP, Nb, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,T), W(1,ST), W(1,iWK))
!
!        call cal_local_norm_n(NP, NB, PEsmpTOT, STACKmcG, W(1,T), aa)
!
!C
!C +----------------------------+
!C | OMEGA= ({t}{s}) / ({t}{t}) |
!C +----------------------------+
!C===
!
        call cal_local_sproduct_norm_n(NP, NB, PEsmpTOT, STACKmcG,      &
     &           W(1,T), W(1,S), C0(1), C0(2))

        START_TIME= MPI_WTIME()
        CG(1:2) = 0.0d0
        call MPI_allREDUCE (C0, CG, 2, CALYPSO_REAL, MPI_SUM,           &
     &                    CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME

        OMEGA= CG(1) / CG(2)

!C
!C +----------------+
!C | update {x},{r} |
!C +----------------+
!C===
!
        call cal_x_and_residual_BiCGSTAB_nn(NP, NB, PEsmpTOT, STACKmcG, &
     &      DNRM20, X, W(1,R), W(1,PT), W(1,ST), W(1,S), W(1,T),        &
     &      ALPHA, OMEGA)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,             &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
        RESID= dsqrt(DNRM2/BNRM2)

        if (IER.eq.1 .and. my_rank.eq.0) write (12,'(a33,i5,1p2e16.6)') &
     &            'solver_VBiCGSTABnn_DJDS_SMP: ', ITER, RESID
        ITR = ITER

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if

        if ( RHO1 .eq.0.0d0 ) then
          RHO1 = 1.0d-11*RHO
        else
          RHO1 = RHO
        end if

      enddo

!C===
   30 continue

!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_N                                         &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C== change B,X

      call back_2_original_order_bxn(NP, NB, NtoO, B, X, W(1,iWK))

      IER = 0
      COMPtime= END_TIME - S1_TIME
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
      if (my_rank.eq.0) then
        open(43,file='solver_33.dat',position='append')
        write (43,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(43)
      endif
!
      end subroutine solve_VBiCGSTABnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module     solver_VBiCGSTABnn_DJDS_SMP
