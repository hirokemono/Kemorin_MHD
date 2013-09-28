!C
!C*** 
!C*** module solver_VCGnn_DJDS_SMP
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!C
!C
!C***
!C***  VCGnn_DJDS_SMP
!C***
!      subroutine VCGnn_DJDS_SMP                                        &
!     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,         &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITR, IER,                                        &
!     &           my_rank, NEIBPETOT, NEIBPE,                           &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           SOLVER_COMM, PRECOND, iterPREmax)
!C
!      subroutine init_VCGnn_DJDS_SMP(NP, NB, PEsmpTOT, PRECOND)
!      subroutine solve_VCGnn_DJDS_SMP                                  &
!     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,         &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITR, IER,                                        &
!     &           my_rank, NEIBPETOT, NEIBPE,                           &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           SOLVER_COMM, PRECOND, iterPREmax)
!C
!C     VCGnn_DJDS_SMP solves the linear system Ax = b with n*n block
!C     using the Conjugate Gradient iterative method with preconditioning.
!C     Elements are ordered in descending Jagged Diagonal Storage
!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!C
      module solver_VCGnn_DJDS_SMP
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine VCGnn_DJDS_SMP                                         &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER,                                         &
     &           my_rank, NEIBPETOT, NEIBPE,                            &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
!     &           SOLVER_COMM, PRECOND, iterPREmax)
     &           SOLVER_COMM, PRECOND)
!
      integer(kind=kint ), intent(in) :: N, NP, NB, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
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
      real(kind=kreal), intent(inout) :: B(NB*NP), X(NB*NP)

      real(kind=kreal), intent(in) :: ALU_L(NB*NB*N), ALU_U(NB*NB*N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT)) 
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
!
      call init_VCGnn_DJDS_SMP(NP, NB, PEsmpTOT, PRECOND)
!
      call solve_VCGnn_DJDS_SMP                                         &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER,                                         &
     &           my_rank, NEIBPETOT, NEIBPE,                            &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
!     &           SOLVER_COMM, PRECOND, iterPREmax)
     &           SOLVER_COMM, PRECOND)

      end subroutine VCGnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_VCGnn_DJDS_SMP(NP, NB, PEsmpTOT, PRECOND)
!
      use m_work_4_CGnn
!
      use djds_matrix_calcs_nn
      use incomplete_cholesky_nn
      use i_cholesky_w_asdd_nn
      use block_ilu_nn
!
      integer(kind=kint ), intent(in) :: NP, NB, PEsmpTOT
      character(len=kchara), intent(in) :: PRECOND

      integer(kind=kint ), parameter :: iterPREmax = 1
!      integer(kind=kint ), intent(in)  :: iterPREmax
!
!
!   allocate work arrays
!
      call verify_work_CG_nn(NP, NB, PEsmpTOT)
      call verify_work_4_matvecnn(NP,NB)
!
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if (iterPREmax .eq. 1) then
          call verify_work_4_I_Choleskynn(NP, NB)
        else
          call verify_work_4_IC_asddnn(NP, NB)
        end if
      end if
!
      if (PRECOND(1:4).eq.'BLOC' .or. PRECOND(1:6).eq.'BL_ILU') then
        call verify_work_4_bl_ilunn(NP, NB)
      end if
!
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
     &           EPS, ITR, IER,                                         &
     &           my_rank, NEIBPETOT, NEIBPE,                            &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
!     &           SOLVER_COMM, PRECOND, iterPREmax)
     &           SOLVER_COMM, PRECOND)

      use calypso_mpi
!
      use solver_SR_N
!
      use m_work_4_CGnn
      use m_solver_count_time
!
      use cal_norm_products_nn
      use vector_calc_solver_nn
      use djds_matrix_calcs_nn
      use incomplete_cholesky_nn
      use i_cholesky_w_asdd_nn
      use diagonal_scaling_nn
      use block_ilu_nn
      use calcs_4_CGnn
!
      integer(kind=kint ), intent(in) :: N, NP, NB, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
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
      real(kind=kreal), intent(inout) :: B(NB*NP), X(NB*NP)

      real(kind=kreal), intent(in) :: ALU_L(NB*NB*N), ALU_U(NB*NB*N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT)) 
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))

      integer(kind=kint ), parameter :: iterPREmax = 1
!      integer(kind=kint ), intent(in)  :: iterPREmax
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
!C
!C-- change B,X

      call change_order_2_solve_bxn(NP, NB, PEsmpTOT, STACKmcG,         &
     &           NtoO, B, X)
!
!C
!C-- INTERFACE data EXCHANGE
!C===
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===

       call subtruct_matvec_nn                                          &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,R), B, X)
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===

      call cal_local_norm_n(NP, NB, PEsmpTOT, STACKmcG, B,              &
     &    BNRM20, DNRMsmp)

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

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

      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!
        if (iterPREmax.eq.1) then
          call incomplete_cholesky_1xnn                                 &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,Z), W(1,R) )
        else
!
          do iterPRE= 1, iterPREmax
!
            call i_cholesky_w_asdd_1xnn                                 &
     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,   &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,ZQ), W(1,Z), W(1,R) )

!C
!C-- INTERFACE data EXCHANGE
            START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, W(1,ZQ))
            END_TIME= MPI_WTIME()
            COMMtime = COMMtime + END_TIME - START_TIME
!
!   additive SCHWARTZ domain decomposition
!
            call add_vector_nn(NP, NB, W(1,Z), W(1,ZQ) )
!
          enddo
!
        end if

      else if (PRECOND(1:4).eq.'BLOC'                                   &
     &    .or. PRECOND(1:6).eq.'BL_ILU') then
        call block_ilu_1xnn                                             &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, W(1,Z), W(1,R) )
!
!C===
!
      else if (PRECOND(1:4).eq.'DIAG') then
!
        call diag_scaling_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,           &
     &      W(1,Z), W(1,R), ALU_L)
!
      endif
      
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
!
      call cal_local_s_product_n(NP, NB, PEsmpTOT, STACKmcG,            &
     &    W(1,R), W(1,Z), RHO0, SPsmp)

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (RHO0, RHO, 1, MPI_DOUBLE_PRECISION,           &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!C===

!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C | {p} = {z} + BETA*{p}        |
!C +-----------------------------+
!C===
      if ( ITER.eq.1 ) then
        call copy_internal_vect_n_smp(NP, NB, PEsmpTOT, STACKmcG,       &
     &      W(1,P), W(1,Z) )
      else
        call djds_z_plus_beta_p_nn(NP, NB, PEsmpTOT, STACKmcG,          &
     &      W(1,P), W(1,Z), RHO, RHO1)
      end if
!C===
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, W(1,P))
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===

      call cal_matvec_nn                                                &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,Q), W(1,P) )
!
!C===

!C
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
!
      call cal_local_s_product_n(NP, NB, PEsmpTOT, STACKmcG,            &
     &    W(1,P), W(1,Q), C10, SPsmp)
!

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (C10, C1, 1, MPI_DOUBLE_PRECISION,             &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
      ALPHA= RHO / C1
!C===

!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C | norm= {r}^2          |
!C +----------------------+
!C===

      call djds_x_and_residual_CG_nn(NP, NB, PEsmpTOT, STACKmcG,        &
     &    DNRM20, X, W(1,R), W(1,P), W(1,Q), ALPHA, DNRMsmp)
!
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
      RESID= dsqrt(DNRM2/BNRM2)

      if (IER.eq.1 .and. my_rank.eq.0) write (12,'(a23,i5,1p10e16.6)')  &
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
      E1_TIME= MPI_WTIME()
      COMPtime= E1_TIME - S1_TIME

!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!      call SOLVER_SEND_RECV_N                                          &
!     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
!     &     STACK_EXPORT, NOD_EXPORT, W(1,1))
!
!      call SOLVER_SEND_RECV_N                                          &
!     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
!     &     STACK_EXPORT, NOD_EXPORT, W(1,2))
!
!      call SOLVER_SEND_RECV_N                                          &
!     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
!     &     STACK_EXPORT, NOD_EXPORT, W(1,3))

!C
!C== change B,X

      call back_2_original_order_bxn(NP, NB, NtoO, B, X)

      IER = 0
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
      if (my_rank.eq.0) then
        open(43,file='solver_33.dat',position='append')
        write (43,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(43)
      endif

      return
      end subroutine solve_VCGnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module     solver_VCGnn_DJDS_SMP
