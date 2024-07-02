!C
!C*** 
!C*** module solver_VBiCGSTAB33_DJDS_SMP
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!!C
!!C***
!!C***  VBiCGSTAB33_DJDS_SMP
!!C***
!!      subroutine VBiCGSTAB33_DJDS_SMP                                 &
!!     &         (N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!!     &          STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!!     &          NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!!     &          INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!!     &          EPS, ITR, IER, NEIBPETOT, NEIBPE,                     &
!!     &          STACK_IMPORT, NOD_IMPORT,                             &
!!     &          STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,        &
!!     &          SR_sig, SR_r, INITtime, COMPtime, COMMtime)
!!
!!      subroutine init_VBiCGSTAB33_DJDS_SMP(NP, PEsmpTOT, PRECOND,     &
!!     &                                     iterPREmax, INITtime)
!!      subroutine solve_VBiCGSTAB33_DJDS_SMP                           &
!!     &         (N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!!     &          STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!!     &          NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!!     &          INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!!     &          EPS, ITR, IER, NEIBPETOT, NEIBPE,                     &
!!     &          STACK_IMPORT, NOD_IMPORT,                             &
!!     &          STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,        &
!!     &          SR_sig, SR_r, COMPtime, COMMtime)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!C
!!C     VBiCGSTAB33_DJDS_SMP solves the linear system Ax = b with 3*3 block
!!C     using the BiCGSTAB iterative method with preconditioning.
!!C     Elements are ordered in descending Jagged Diagonal Storage
!!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!!C
      module solver_VBiCGSTAB33_DJDS_SMP
!
      use m_precision
      use t_solver_SR
      use calypso_mpi
!
      implicit none
!
       real(kind = kreal), allocatable :: W(:,:)
       private :: W
       private :: verify_work_4_matvec33
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec33(NP, nwk)
!
      integer(kind = kint), intent(in) :: NP, nwk
!
      if(allocated(W) .eqv. .false.) then
        allocate ( W(3*NP,nwk) )
        W = 0.0d0
      else if(size(W) .lt. (nwk*3*NP)) then
        deallocate (W)
        allocate ( W(3*NP,nwk) )
        W = 0.0d0
      end if
!
      end subroutine verify_work_4_matvec33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine VBiCGSTAB33_DJDS_SMP                                   &
     &         (N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,               &
     &          STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                 &
     &          NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,            &
     &          INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,               &
     &          EPS, ITR, IER, NEIBPETOT, NEIBPE,                       &
     &          STACK_IMPORT, NOD_IMPORT,                               &
     &          STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,          &
     &          SR_sig, SR_r, INITtime, COMPtime, COMMtime)
!
      integer(kind=kint ), intent(in) :: N, NP, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: NEIBPETOT
      character(len=kchara), intent(in) :: PRECOND
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

      real(kind=kreal), intent(in) :: D(9*NP )
      real(kind=kreal), intent(in) :: AL(9*NPL)
      real(kind=kreal), intent(in) :: AU(9*NPU)
!
      real(kind=kreal), intent(inout) :: B(3*NP)
      real(kind=kreal), intent(inout) :: X(3*NP)

      real(kind=kreal), intent(in) :: ALU_L(9*N), ALU_U(9*N)

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
      call init_VBiCGSTAB33_DJDS_SMP(NP, PEsmpTOT, PRECOND,             &
     &                               iterPREmax, INITtime)
!
      call solve_VBiCGSTAB33_DJDS_SMP                                   &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r, COMPtime, COMMtime)
!
      end subroutine VBiCGSTAB33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine init_VBiCGSTAB33_DJDS_SMP(NP, PEsmpTOT, PRECOND,       &
     &                                     iterPREmax, INITtime)
!
      use m_BiCGSTAB_constants
      use djds_matrix_calcs_33
      use incomplete_cholesky_33
      use i_cholesky_w_asdd_33
      use block_ilu_33
!
      character(len=kchara), intent(in) :: PRECOND
      integer(kind=kint ), intent(in) :: NP, PEsmpTOT
      integer(kind=kint ), intent(in)  :: iterPREmax
!>      Elapsed time for initialization
      real(kind = kreal), intent(inout) :: INITtime
!
      real(kind = kreal) :: START_TIME
!
!
      START_TIME= MPI_WTIME()
!
!   allocate work arrays
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if(iterPREmax .ge. 1) ntotWK_BiCGSTAB = ntotWK_BiCGSTAB + 2
      end if
!
      call verify_work_4_matvec33(NP, ntotWK_BiCGSTAB)
      INITtime = MPI_WTIME() - START_TIME
!
      end subroutine init_VBiCGSTAB33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine solve_VBiCGSTAB33_DJDS_SMP                             &
     &         (N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,               &
     &          STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                 &
     &          NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,            &
     &          INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,               &
     &          EPS, ITR, IER, NEIBPETOT, NEIBPE,                       &
     &          STACK_IMPORT, NOD_IMPORT,                               &
     &          STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax,          &
     &          SR_sig, SR_r, COMPtime, COMMtime)
!
      use solver_SR_3
!
      use m_BiCGSTAB_constants
!
      use cal_norm_products_33
      use vector_calc_solver_33
      use djds_matrix_calcs_33
      use incomplete_cholesky_33
      use i_cholesky_w_asdd_33
      use block_ilu_33
      use diagonal_scaling_33
      use calcs_4_BiCGSTAB
!
      integer(kind=kint ), intent(in) :: N, NP, NL, NU, NPL, NPU
      integer(kind=kint ), intent(in) :: PEsmpTOT, NVECT
      integer(kind=kint ), intent(in) :: NEIBPETOT
      character(len=kchara), intent(in) :: PRECOND
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

      real(kind=kreal), intent(in) :: D(9*NP )
      real(kind=kreal), intent(in) :: AL(9*NPL)
      real(kind=kreal), intent(in) :: AU(9*NPU)

      real(kind=kreal), intent(in) :: ALU_L(9*N), ALU_U(9*N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 

      integer(kind=kint ), intent(in)  :: iterPREmax
!
      real(kind=kreal), intent(inout) :: B(3*NP)
      real(kind=kreal), intent(inout) :: X(3*NP)
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
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: ALPHA, OMEGA
      real(kind=kreal) :: RHO1
      real(kind=kreal) :: BNRM2(1),  DNRM2(1),  C2(1),  RHO(1)
      real(kind=kreal) :: BNRM20(1), DNRM20(1), C20(1), RHO0(1)
      real(kind=kreal) :: C0(2), CG(2)
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
      W(1:3*NP,1:nWK_BiCGSTAB) = 0.0d0
!$omp end workshare
!
      call reset_solver_time
!C
!C-- change B,X
!
      call change_order_2_solve_bx3(NP, PEsmpTOT, STACKmcG,             &
     &           NtoO, B, X, W(1,iWK))

!C
!C-- INTERFACE data EXCHANGE
!
!C===
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C | {rt}= {b} - [A]{xini} |
!C +-----------------------+
!C===
!
       call subtruct_matvec_33                                          &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            W(1,R), B, X, W(1,iWK))
!
       call copy_vector_33(NP, W(1,RT), W(1,R) )
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===

      call cal_local_norm_3(NP, PEsmpTOT, STACKmcG, B, BNRM20(1))

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

      if (BNRM2(1) .eq. 0.d0) BNRM2(1) = 1.d0
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
        call cal_local_s_product_3(NP, PEsmpTOT, STACKmcG,              &
     &    W(1,R), W(1,RT), RHO0(1))
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE(RHO0, RHO, 1, CALYPSO_REAL,                  &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!C===

!C
!C
!C +----------------------------------------+
!C | BETA= (RHO/RHO1) * (ALPHA/OMEGA)       |
!C | {p} = {r} + BETA * ( {p} - OMEGA*{v} ) |
!C +----------------------------------------+
!C===
        if (iter.eq.1) then
          call copy_internal_vect_3_smp(NP, PEsmpTOT, STACKmcG,         &
     &        W(1,P), W(1,R) )
        else
          call r_plus_beta_p_sub_omega_v_33(NP, PEsmpTOT, STACKmcG,     &
     &       W(1,P), W(1,R), W(1,V), OMEGA, ALPHA, RHO(1), RHO1)
        end if
!C===

!C
!C +--------------------+
!C | {p_tld}= [Minv]{p} |
!C +--------------------+
!C===
        call clear_vector_solve_33(NP, W(1,PT) )

        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!
          if (iterPREmax.eq.1) then
            call incomplete_cholesky_1x33                               &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,PT), W(1,P), W(1,iWK))
          else
!
            do iterPRE= 1, iterPREmax
              call i_cholesky_w_asdd_1x33                               &
     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,       &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,ZQ), W(1,PT), W(1,P),    &
     &            W(1,iWK))

!C
!C-- INTERFACE data EXCHANGE
              START_TIME= MPI_WTIME()
              call SOLVER_SEND_RECV_3                                   &
     &           (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
     &            STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,ZQ))
              COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
!   additive SCHWARTZ domain decomposition
!
              call add_vector_33(NP, W(1,PT), W(1,ZQ) )
            end do
          end if
!
        else if (PRECOND(1:4).eq.'BLOC'                                 &
     &    .or. PRECOND(1:6).eq.'BL_ILU') then
!
          call block_ilu_1x33                                           &
     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,       &
     &           ALU_L, W(1,PT), W(1,P), W(1,iWK))
!
        else if (PRECOND(1:4).eq.'DIAG') then
          call diag_scaling_1x33(NP, N, PEsmpTOT, STACKmcG,             &
     &      W(1,PT), W(1,P), ALU_L)
        end if
!C===

!C
!C-- INTERFACE data EXCHANGE
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,PT) )
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!C
!C +-----------------+
!C | {v}= [A]{p_tld} |
!C +-----------------+
!C===
!
        call cal_matvec_33                                              &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            W(1,V), W(1,PT), W(1,iWK))
!
!C===
!C
!C +----------------+
!C | ALPHA = RHO/C2 |
!C +----------------+
!C===
!
        call cal_local_s_product_3(NP, PEsmpTOT, STACKmcG,              &
     &    W(1,RT), W(1,V), C20(1))

        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C20, C2, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
        ALPHA = RHO(1) / C2(1)
!C===

!C
!C +----------------------+
!C | {s}= {r} - ALPHA*{v} |
!C +----------------------+
!C===
        call r_sub_alpha_v_33(NP, PEsmpTOT, STACKmcG, W(1,S), W(1,R),   &
     &    W(1,V), ALPHA)
!
!        call cal_local_norm_3(NP, PEsmpTOT, STACKmcG, W(1,S), aa)
!
!C===

!C
!C +--------------------+
!C | {s_tld}= [Minv]{s} |
!C +--------------------+
!C===
!
        call clear_vector_solve_33(NP, W(1,ST) )

        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!
          if (iterPREmax.eq.1) then
            call incomplete_cholesky_1x33                               &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,ST), W(1,S), W(1,iWK))
          else
            do iterPRE= 1, iterPREmax
              call i_cholesky_w_asdd_1x33                               &
     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,       &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,ZQ), W(1,ST), W(1,S),    &
     &            W(1,iWK))

!C
!C-- INTERFACE data EXCHANGE
              START_TIME= MPI_WTIME()
              call SOLVER_SEND_RECV_3                                   &
     &           (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
     &            STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,ZQ))
              COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
!   additive SCHWARTZ domain decomposition
!
              call add_vector_33(NP, W(1,ST), W(1,ZQ) )
!
            end do
          end if
!
        else if (PRECOND(1:4).eq.'BLOC'                                 &
     &        .or. PRECOND(1:6).eq.'BL_ILU') then
          call block_ilu_1x33                                           &
     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,       &
     &           ALU_L, W(1,ST), W(1,S), W(1,iWK))
!
        else if (PRECOND(1:4).eq.'DIAG') then
          call diag_scaling_1x33(NP, N, PEsmpTOT, STACKmcG,             &
     &      W(1,ST), W(1,S), ALU_L)
        end if
!
!C===
!C
!C-- INTERFACE data EXCHANGE
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,ST) )
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!C
!C +-----------------+
!C | {t}= [A]{s_tld} |
!C +-----------------+
!C===
        call cal_matvec_33                                              &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            W(1,T), W(1,ST), W(1,iWK))
!
!        call cal_local_norm_3(NP, PEsmpTOT, STACKmcG, W(1,T), aa)
!
!C
!C +----------------------------+
!C | OMEGA= ({t}{s}) / ({t}{t}) |
!C +----------------------------+
!C===
!
        call cal_local_sproduct_norm_3(NP, PEsmpTOT, STACKmcG,          &
     &           W(1,T), W(1,S), C0(1), C0(2))

        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C0, CG, 2, CALYPSO_REAL, MPI_SUM,           &
     &                    CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

        OMEGA= CG(1) / CG(2)

!C
!C +----------------+
!C | update {x},{r} |
!C +----------------+
!C===
!
        call cal_x_and_residual_BiCGSTAB_33(NP, PEsmpTOT, STACKmcG,     &
     &      DNRM20(1), X, W(1,R), W(1,PT), W(1,ST), W(1,S), W(1,T),     &
     &      ALPHA, OMEGA)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,             &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
        RESID= dsqrt(DNRM2(1) / BNRM2(1))

        if (IER.eq.1 .and. my_rank.eq.0) write (12,'(a33,i5,1p2e16.6)') &
     &            'solver_VBiCGSTAB33_DJDS_SMP: ', ITER, RESID
        ITR = ITER

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if

        if ( RHO1 .eq.0.0d0 ) then
          RHO1 = 1.0d-11 * RHO(1)
        else
          RHO1 = RHO(1)
        end if
      end do

!C===
   30 continue

!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!C
!C== change B,X

      call back_2_original_order_bx3(NP, NtoO, B, X, W(1,iWK))
      COMPtime= MPI_WTIME() - S1_TIME

      IER = 0

      end subroutine solve_VBiCGSTAB33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module solver_VBiCGSTAB33_DJDS_SMP
