!C
!C*** 
!C*** module solver_VCG11_DJDS_SMP
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!C
!C***
!C***  VCG11_DJDS_SMP
!C***
!      subroutine VCG11_DJDS_SMP                                        &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                     &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax)
!
!      subroutine init_VCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND)
!      subroutine solve_VCG11_DJDS_SMP                                  &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                     &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT, PRECOND, iterPREmax)
!C
!C     VCG_DJDS_SMP solves the linear system Ax = b 
!C     using the Conjugate Gradient iterative method with preconditioning.
!C     Elements are ordered in descending Jagged Diagonal Storage
!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!C
      module solver_VCG11_DJDS_SMP
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
!C
      subroutine VCG11_DJDS_SMP                                         &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!     &           PRECOND, iterPREmax)
!
!
      integer(kind=kint ), intent(in) :: N, NP, NL, NU, NPL, NPU, NVECT
      integer(kind=kint ), intent(in) :: PEsmpTOT
      integer(kind=kint ), intent(in) :: NEIBPETOT
      character (len=kchara), intent(in) :: PRECOND
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
      call init_VCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND)

      call solve_VCG11_DJDS_SMP                                         &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!     &           PRECOND, iterPREmax)
!
      end subroutine VCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine init_VCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND)
!
      use m_work_4_CG11
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
      call verify_work_CG_11(NP, PEsmpTOT)
      call verify_work_4_matvec11(NP)
!
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if (iterPREmax .eq. 1) then
          call verify_work_4_I_Cholesky11(NP)
        else
          call verify_work_4_IC_asdd11(NP)
        end if
      end if
!
      end subroutine init_VCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine solve_VCG11_DJDS_SMP                                   &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!     &           PRECOND, iterPREmax)

      use calypso_mpi

      use solver_SR
!
      use m_work_4_CG11
      use m_solver_count_time
!
      use djds_norm_products_11
      use vector_calc_solver_11
      use djds_matrix_calcs_11
      use i_cholesky_w_asdd_11
      use incomplete_cholesky_11
      use diagonal_scaling_11
      use calcs_4_CG11
!
      integer(kind=kint ), intent(in) :: N, NP, NL, NU, NPL, NPU, NVECT
      integer(kind=kint ), intent(in) :: PEsmpTOT
      integer(kind=kint ), intent(in) :: NEIBPETOT
      character (len=kchara), intent(in) :: PRECOND
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
!
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
!
      call reset_solver_time
      call init_work_CG_11(NP)
!
!C
!C-- change B,X

       call change_order_2_solve_bx1(NP, PEsmpTOT, STACKmcG,            &
     &           NtoO, B, X)

       call clear_vector_solve_11(NP, W(1,3) )

!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
!
       call subtruct_matvec_11                                          &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,            &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,R), B, X)
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===
      call djds_local_norm_1(NP, PEsmpTOT, STACKmcG, B,                 &
     &    BNRM20, DNRMsmp)
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do iter= 1, MAXIT
!C
!C************************************************* Conjugate Gradient Iteration

!C
!C +----------------+
!C | {z}= [Minv]{r} |
!C +----------------+
!C===
!
       call clear_vector_solve_11(NP, W(1,Z) )
!
!C
!C-- incomplete CHOLESKY
      
        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
!C
          if (iterPREmax.eq.1) then
            call incomplete_cholesky_1x11                               &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, W(1,Z), W(1,R) )
          else
!
            do iterPRE= 1, iterPREmax
!
              call i_cholesky_w_asdd_1x11                               &
     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,       &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, W(1,ZQ), W(1,Z), W(1,R) )
!
!C
!C-- INTERFACE data EXCHANGE
              START_TIME= MPI_WTIME()
              call SOLVER_SEND_RECV                                     &
     &            ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,    &
     &              STACK_EXPORT, NOD_EXPORT, W(1,ZQ) )
              END_TIME= MPI_WTIME()
              COMMtime = COMMtime + END_TIME - START_TIME
!
!   additive SCHWARTZ domain decomposition
!
              call add_vector_11(NP, W(1,Z), W(1,ZQ) )
!
            end do
          end if
!C===
        else if (PRECOND .eq. 'DIAG' ) then
          call diag_scaling_1x11(NP, N, PEsmpTOT, STACKmcG,             &
     &           W(1,Z), W(1,R), ALU_L)
        end if
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
!
      call djds_local_s_product_1(NP, PEsmpTOT, STACKmcG,               &
     &           W(1,R), W(1,Z), RHO0, SPsmp)
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (RHO0, RHO, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
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
          call copy_internal_vect_1_smp(NP, PEsmpTOT, STACKmcG,         &
     &          W(1,P), W(1,Z) )
        else
          call djds_z_plus_beta_p_11(NP, PEsmpTOT, STACKmcG,            &
     &          W(1,P), W(1,Z), RHO, RHO1)
        endif
!C===
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, W(1,P) )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===        
!
      call cal_matvec_11                                                &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,            &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,Q), W(1,P) )
!
!C===
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
!
        call djds_local_s_product_1(NP, PEsmpTOT, STACKmcG,             &
     &           W(1,P), W(1,Q), C10, SPsmp)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C10, C1, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
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
!
        call djds_x_and_residual_CG_11(NP, PEsmpTOT, STACKmcG,          &
     &      DNRM20, X, W(1,R), W(1,P), W(1,Q), ALPHA, DNRMsmp)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,             &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
        RESID= dsqrt(DNRM2/BNRM2)

        if (IER.eq.1 .and. my_rank.eq.0) write (12,'(a23,i5,1p2e16.6)') &
     &            'solver_VCG11_DJDS_SMP: ', ITER, RESID
        ITR = ITER

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if

        RHO1 = RHO
      end do

!C===
   30 continue

!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C== change B,X

       call back_2_original_order_bx1(NP, NtoO, B, X)

      IER = 0
      E1_TIME= MPI_WTIME()
      COMPtime= E1_TIME - S1_TIME
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
      if (my_rank.eq.0) then
        open(41,file='solver_11.dat',position='append')
        write (41,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(41)
      endif

      return
      end subroutine solve_VCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module     solver_VCG11_DJDS_SMP
