!C
!C*** 
!C*** module solver_VGPBiCG11_DJDS_SMP
!C***
!
!      Modified by H. Matsui on Nov. 2005
!
!C
!C***
!C***  VGPBiCG11_DJDS_SMP
!C***
!      subroutine VGPBiCG11_DJDS_SMP                                    &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITR, IER,                                        &
!     &           my_rank, NEIBPETOT, NEIBPE,                           &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           SOLVER_COMM, PRECOND, iterPREmax)
!
!      subroutine init_VGPBiCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND)
!      subroutine solve_VGPBiCG11_DJDS_SMP                              &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITR, IER,                                        &
!     &           my_rank, NEIBPETOT, NEIBPE,                           &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           SOLVER_COMM, PRECOND, iterPREmax)
!C
!C     VGPBiCG11_DJDS_SMP solves the linear system Ax = b with
!     GPBiCG iterative method with preconditioning.
!C     Elements are ordered in descending Jagged Diagonal Storage
!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!C
!
      module solver_VGPBiCG11_DJDS_SMP
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
      subroutine VGPBiCG11_DJDS_SMP                                     &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
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
      integer(kind=kint ), intent(in) :: N, NP, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
      character(len=kchara), intent(in) :: PRECOND
      real   (kind=kreal), intent(in) :: EPS
      integer(kind=kint ), intent(inout) :: ITR, IER

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

      real(kind=kreal), intent(in) :: D(NP )
      real(kind=kreal), intent(in) :: AL(NPL)
      real(kind=kreal), intent(in) :: AU(NPU)
!
      real(kind=kreal), intent(inout) :: B(NP)
      real(kind=kreal), intent(inout) :: X(NP)

      real(kind=kreal), intent(in) :: ALU_L(N), ALU_U(N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
!
      call init_VGPBiCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND)
!
      call solve_VGPBiCG11_DJDS_SMP                                     &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
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
!
      end subroutine VGPBiCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine init_VGPBiCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND)
!
      use m_work_4_GPBiCG11
      use djds_matrix_calcs_11
      use incomplete_cholesky_11
      use i_cholesky_w_asdd_11
!
      character(len=kchara), intent(in) :: PRECOND
      integer(kind=kint ), intent(in) :: NP, PEsmpTOT
      integer(kind=kint ), parameter :: iterPREmax = 1
!      integer(kind=kint ), intent(in)  :: iterPREmax
!
!   allocate work arrays
!
      call verify_work_GPBiCG_11(NP, PEsmpTOT)
      call verify_work_4_matvec3x11(NP)
!
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if (iterPREmax .eq. 1) then
          call verify_work_4_I_Cholesky3x11(NP)
        else
          call verify_work_4_IC_asdd3x11(NP)
        end if
      end if
!
      end subroutine init_VGPBiCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine solve_VGPBiCG11_DJDS_SMP                               &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
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
      use calypso_mpi
!
      use solver_SR
!
      use m_work_4_GPBiCG11
      use m_solver_count_time
!
      use djds_norm_products_11
      use vector_calc_solver_11
      use djds_matrix_calcs_11
      use incomplete_cholesky_11
      use i_cholesky_w_asdd_11
      use diagonal_scaling_11
      use calcs_4_GPBiCG11
!
      integer(kind=kint ), intent(in) :: N, NP, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
      character(len=kchara), intent(in) :: PRECOND
      real   (kind=kreal), intent(in) :: EPS
      integer(kind=kint ), intent(inout) :: ITR, IER

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

      real(kind=kreal), intent(in) :: D(NP )
      real(kind=kreal), intent(in) :: AL(NPL)
      real(kind=kreal), intent(in) :: AU(NPU)
!
      real(kind=kreal), intent(inout) :: B(NP)
      real(kind=kreal), intent(inout) :: X(NP)

      real(kind=kreal), intent(in) :: ALU_L(N), ALU_U(N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 

      integer(kind=kint ), parameter :: iterPREmax = 1
!      integer(kind=kint ), intent(in)  :: iterPREmax
!      integer :: j, in
!      real(kind = kreal) :: zz1(NP)

      integer(kind=kint ) :: npLX1, npUX1
      integer(kind=kint ) :: iter, MAXIT
!
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
!
      call reset_solver_time
      call init_work_GPBiCG_11(NP, PEsmpTOT)
!
!C-- change B,X
!
      call change_order_2_solve_bx1(NP, PEsmpTOT, STACKmcG,             &
     &           NtoO, B, X)

!C
!C-- INTERFACE data EXCHANGE
!C===
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X, SOLVER_COMM,my_rank)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
       call copy_vector_11(NP, W(1,RT), W(1,R) )
!C
!C
!C +-----------------------+
!C | {r}= {b} - [A]{xini}  |
!C | {rt}= {b} - [A]{xini} |
!C +-----------------------+
!C===
!C
       call subtruct_matvec_11                                          &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,            &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,R), B, X)
!
!C +-----------------+
!C | BNORM2 = B^2    |
!C | RHO = {r}*{rt}  |
!C +-----------------+
!C===

      call djds_local_sproduct_and_norm_1(NP, PEsmpTOT, STACKmcG,       &
     &           B, W(1,R), W(1,RT), RHO0, BNRM20, SP1smp, DNRMsmp)

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      call MPI_allREDUCE (RHO0, RHO, 1, MPI_DOUBLE_PRECISION,           &
     &                    MPI_SUM, SOLVER_COMM, ierr)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!
!C===
!C************************************************* Conjugate Gradient Iteration
!
      do iter= 1, MAXIT
!C
!C-- INIT.
!C
!C +----------------+
!C | {r}= [Minv]{r} |
!C +----------------+
!C===
!C
!C-- INTERFACE data EXCHANGE
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,R), SOLVER_COMM, my_rank)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C
       call copy_vector_11(NP, W(1,WK), W(1,R) )
!
       call clear_vector_solve_11(NP, W(1,R) )
!
!C-- incomplete CHOLESKY
!
!        call clear_vector_solve_11(NP, W(1,R) )
      
        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!
          if (iterPREmax.eq.1) then
            call incomplete_cholesky_1x11                               &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,R), W(1,WK) )
          else
!
            do iterPRE= 1, iterPREmax
!
              call i_cholesky_w_asdd_1x11                               &
     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,       &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,RZ), W(1,R), W(1,WK) )

!C
!C-- INTERFACE data EXCHANGE
              START_TIME= MPI_WTIME()
              call SOLVER_SEND_RECV                                     &
     &            ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,    &
     &              STACK_EXPORT, NOD_EXPORT, W(1,RZ),                  &
     &              SOLVER_COMM, my_rank)
              END_TIME= MPI_WTIME()
              COMMtime = COMMtime + END_TIME - START_TIME
!
!   additive SCHWARTZ domain decomposition
!
              call add_vector_11(NP, W(1,R), W(1,RZ) )
!
            enddo
!
          end if
!
        else if (PRECOND(1:4).eq.'DIAG') then
!
          call diag_scaling_1x11(NP, N, PEsmpTOT, STACKmcG,             &
     &      W(1,R), W(1,WK), ALU_L)
!
        endif
!
!C
!C +----------------------------------+
!C | {p} = {r} + BETA * ( {p} - {u} ) |
!C +----------------------------------+
!C===
!
        if (iter.gt.1) then
          call r_plus_beta_p_sub_u_11(NP, PEsmpTOT, STACKmcG,           &
     &          W(1,P), W(1,R), W(1,U), BETA)
        else
          call copy_vector_11(NP, W(1,P), W(1,R) )
        end if
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,P), SOLVER_COMM, my_rank)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +--------------------------------+
!C | ALPHA= {r_tld}{r}/{r_tld} A{p} |
!C +--------------------------------+
!C===
!C
!C-- calc. {p_tld}= A{p}
!
        call cal_matvec_11                                              &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,            &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U,  NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU, &
     &            W(1,PT), W(1,P) )
!C
!C-- calc. ALPHA
!
        call djds_local_s_product_1(NP, PEsmpTOT, STACKmcG,             &
     &           W(1,RT), W(1,PT), RHO10, SP1smp)

        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (RHO10, RHO1, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, SOLVER_COMM, ierr)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
        ALPHA= RHO / RHO1
!      if (my_rank.eq.0) write(*,*) 'ALPHA', ALPHA, RHO, RHO1
!C===
!C
!C +------------------------------------------+
!C | {y}= {t} - {r} - ALPHA{w} + ALPHA{p_tld} |
!C | {t}= {r}                  - ALPHA{p_tld} |
!C +------------------------------------------+
!C===
        call t_sub_ro_sub_alpha_w_ptld_11(NP, PEsmpTOT, STACKmcG,       &
     &          W(1,Y), W(1,T), W(1,WK), W(1,W1), W(1,PT), ALPHA)
!
!C
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECVx3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,PT), W(1,T), W(1,T0),          &
     &     SOLVER_COMM, my_rank)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +-----------------------+
!C | {t_tld}= [A][Minv]{t} |
!C +-----------------------+
!C===
!C
!C-- calc. {t_tld} and {t0} by [M] inversion
!C         {W2}   = [Minv]{p_tld} 
!C
        call copy_vector_11(NP, W(1,WT), W(1,T0) )
        call clear_vector_solve_3x11(NP, W(1,TT), W(1,W2), W(1,T0) )
!C
!C-- incomplete CHOLESKY x 3
!
        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!
          if (iterPREmax.eq.1) then
            call incomplete_cholesky_3x11                               &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,TT), W(1,W2), W(1,T0),              &
     &            W(1,T ), W(1,PT), W(1,WT) )
          else
!
            do iterPRE= 1, iterPREmax
!
              call i_cholesky_w_asdd_3x11                               &
     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,       &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,RX), W(1,RY), W(1,RZ),   &
     &            W(1,TT), W(1,W2), W(1,T0),                            &
     &            W(1,T ), W(1,PT), W(1,WT) )

!C
!C-- INTERFACE data EXCHANGE
              START_TIME= MPI_WTIME()
              call SOLVER_SEND_RECVx3                                   &
     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
     &             STACK_EXPORT, NOD_EXPORT,                            &
     &             W(1,RX), W(1,RY), W(1,RZ), SOLVER_COMM,  my_rank)
              END_TIME= MPI_WTIME()
              COMMtime = COMMtime + END_TIME - START_TIME
!
!   additive SCHWARTZ domain decomposition
!
              call add_vector_3x11(NP, W(1,TT), W(1,W2), W(1,T0),       &
     &            W(1,RX), W(1,RY), W(1,RZ) )
!
            enddo
!
          end if
!
        else if (PRECOND(1:4).eq.'DIAG') then
!
          call diag_scaling_3x11(NP, N, PEsmpTOT, STACKmcG,             &
     &        W(1,TT), W(1,W2), W(1,T0), W(1,T ), W(1,PT), W(1,WT),     &
     &        ALU_L)
!
        end if
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,TT), SOLVER_COMM, my_rank)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
!C
!C-- calc. [A]{t_tld}
!
        call cal_matvec_11                                              &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,            &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,WK), W(1,TT) )
!
        call copy_vector_11(NP, W(1,TT), W(1,WK) )
!
!C
!C +-------------------+
!C | calc. QSI and ETA |
!C +-------------------+
!C===
!
        call djds_5_products_norm_1(NP, PEsmpTOT, STACKmcG,             &
     &          W(1,Y), W(1,T), W(1,TT), CG, C0, C0_smp)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C0, CG,  5, MPI_DOUBLE_PRECISION,           &
     &                     MPI_SUM, SOLVER_COMM, ierr)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
        if (iter.eq.1) then
          EQ(1)= CG(2)/CG(5)
          EQ(2)= 0.d0
        else
          EQ(1)= (CG(1)*CG(2)-CG(3)*CG(4)) / (CG(5)*CG(1)-CG(4)*CG(4))
          EQ(2)= (CG(5)*CG(3)-CG(4)*CG(2)) / (CG(5)*CG(1)-CG(4)*CG(4))
        endif

        QSI= EQ(1)
        ETA= EQ(2)
!      if (my_rank.eq.0) write(*,*) 'QSI, ETA', QSI, ETA
!C
!C +----------------------------------------------------------+
!C | {u} = QSI [Minv]{pt} + ETA([Minv]{t0}-[Minv]{r}+BETA*{u} |
!C | {z} = QSI [Minv]{r}  + ETA*{z} - ALPHA*{u}               |
!C +----------------------------------------------------------+
!C===
!
      call cal_u_and_z_11(NP, PEsmpTOT, STACKmcG,                       &
     &          W(1,U), W(1,Z), W(1,W2), W(1,T0), W(1,R),               &
     &          QSI, ETA, ALPHA, BETA)
!C
!C +--------------------+
!C | update {x},{r},{w} |
!C +--------------------+
!C===
!
        call cal_x_and_residual_GPBiCG_11(NP, PEsmpTOT,                 &
     &          STACKmcG, DNRM20, COEF10, X, W(1,R), W(1,T0), W(1,P),   &
     &          W(1,Z), W(1,T), W(1,Y), W(1,TT), W(1,RT), ALPHA,        &
     &          ETA, QSI, DNRMsmp, COEFsmp)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE  (DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,    &
     &                     MPI_SUM, SOLVER_COMM, ierr)
        call MPI_allREDUCE  (COEF10, COEF1, 1, MPI_DOUBLE_PRECISION,    &
     &                     MPI_SUM, SOLVER_COMM, ierr)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!      if (my_rank.eq.0) write(*,*) 'DNRM2, COEF1', DNRM2, COEF1
!
!C +---------------------------------+
!C | BETA = ALPHA*COEF1 / (QSI*RHO)  |
!C | {w} = {tt} + BETA*{pt}          |
!C +---------------------------------+
!
        call tt_add_beta_pt_11(NP, PEsmpTOT, STACKmcG,                  &
     &          W(1,W1), W(1,TT), W(1,PT), BETA, RHO,                   &
     &          ALPHA, COEF1, QSI)
!
        RESID= dsqrt(DNRM2/BNRM2)
        RHO  = COEF1
!
        if (IER.eq.1 .and. my_rank.eq.0) write (12,'(a30,i5,1p2e16.6)') &
     &            'solver_VGPBiCG11_DJDS_SMP: ', ITER, RESID
        ITR = ITER
!
        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if
!
      end do
!
!C===
   30 continue

!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X, SOLVER_COMM,my_rank)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C== change B,X
!
      call back_2_original_order_bx1(NP, NtoO, B, X)

      IER = 0
      COMPtime= END_TIME - S1_TIME
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
    
      if (my_rank.eq.0) then
        open(41,file='solver_11.dat',position='append')
        write (41,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(41)
      endif
!
      end subroutine solve_VGPBiCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module solver_VGPBiCG11_DJDS_SMP
