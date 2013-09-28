!Csolver_JACOBI33_DJDS.f90
!C*** 
!C*** module solver_JACOBI33_DJDS
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!C
!C***
!C***  VJACOBI33_DJDS_SMP
!C***
!      subroutine VJACOBI33_DJDS_SMP                                    &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITR, IER,                                        &
!     &           my_rank, NEIBPETOT, NEIBPE,                           &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!C
!      subroutine init_VJACOBI33_DJDS_SMP(NP, PEsmpTOT)
!      subroutine solve_VJACOBI33_DJDS_SMP                              &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITR, IER,                                        &
!     &           my_rank, NEIBPETOT, NEIBPE,                           &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!C
!C     VCG_DJDS_SMP solves the linear system Ax = b 
!C     using the Conjugate Gradient iterative method with preconditioning.
!C     Elements are ordered in descending Jagged Diagonal Storage
!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!C
      module solver_JACOBI33_DJDS
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
      subroutine VJACOBI33_DJDS_SMP                                     &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER,                                         &
     &           my_rank, NEIBPETOT, NEIBPE,                            &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!
      integer(kind=kint ), intent(in) :: N, NP, NL, NU, NPL, NPU, NVECT
      integer(kind=kint ), intent(in) :: PEsmpTOT
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
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

      real(kind=kreal), intent(in) :: D(9*NP )
      real(kind=kreal), intent(in) :: AL(9*NPL)
      real(kind=kreal), intent(in) :: AU(9*NPU)

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
!
      call init_VJACOBI33_DJDS_SMP(NP, PEsmpTOT)

      call solve_VJACOBI33_DJDS_SMP                                     &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER,                                         &
     &           my_rank, NEIBPETOT, NEIBPE,                            &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!
      end subroutine VJACOBI33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine init_VJACOBI33_DJDS_SMP(NP, PEsmpTOT)
!
      use m_work_4_CG33
      use djds_matrix_calcs_33
      use jacobi_precondition_33
!
      integer(kind=kint ), intent(in) :: NP, PEsmpTOT
!
!   allocate work arrays
!
      call verify_work_CG_33(NP, PEsmpTOT)
      call verify_work_4_matvec33(NP)
!
      call verify_work_4_jacobi_prec33(NP)
!
      end subroutine init_VJACOBI33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine solve_VJACOBI33_DJDS_SMP                               &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER,                                         &
     &           my_rank, NEIBPETOT, NEIBPE,                            &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!
      use calypso_mpi
!
      use solver_SR_3
!
      use m_work_4_CG33
      use m_solver_count_time
!
      use cal_norm_products_33
      use vector_calc_solver_33
      use djds_matrix_calcs_33
      use jacobi_precondition_33
!
      integer(kind=kint ), intent(in) :: N, NP, NL, NU, NPL, NPU, NVECT
      integer(kind=kint ), intent(in) :: PEsmpTOT
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
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

      real(kind=kreal), intent(in) :: D(9*NP )
      real(kind=kreal), intent(in) :: AL(9*NPL)
      real(kind=kreal), intent(in) :: AU(9*NPU)

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

      integer(kind=kint ) :: npLX1, npUX1, i
      integer(kind=kint ) :: iter, MAXIT
!
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
      call init_work_CG_33(NP)
!
!C-- change B,X
!
       call change_order_2_solve_bx3(NP, PEsmpTOT, STACKmcG,            &
     &           NtoO, B, X)
!C
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
!
       call subtruct_matvec_33                                          &
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
      call cal_local_norm_3(NP, PEsmpTOT, STACKmcG, B, BNRM20, DNRMsmp)
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, MPI_DOUBLE_PRECISION,       &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do iter= 1, MAXIT
!C
!C******************************************** Gauss-Zeidel iteration
!
      call jacobi_forward_33                                            &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1,                &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            AL, AU, ALU_L, X, B)
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, X)
!
         call subtruct_matvec_33                                        &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,            &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,R), B, X)
!
!C
!C +---------------+
!C | DNRM2 = B^2   |
!C +---------------+
!C===
         call cal_local_norm_3(NP, PEsmpTOT, STACKmcG, W(1,R),          &
     &       DNRM20, DNRMsmp)
!
         START_TIME= MPI_WTIME()
         call MPI_allREDUCE(DNRM20, DNRM2, 1, MPI_DOUBLE_PRECISION,     &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
         END_TIME= MPI_WTIME()
         COMMtime = COMMtime + END_TIME - START_TIME
!
         RESID= dsqrt(DNRM2/BNRM2)

         if (IER.eq.1 .and. my_rank.eq.0) write(12,'(a,i5,1p2e16.6)') &
     &               'solver_JACOBI_DJDS33_SMP: ', ITER, RESID
         ITR = ITER

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
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
     &     STACK_EXPORT, NOD_EXPORT, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C== change B,X

       call back_2_original_order_bx3(NP, NtoO, B, X)

      IER = 0
      E1_TIME= MPI_WTIME()
      COMPtime= E1_TIME - S1_TIME
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
      if (my_rank.eq.0) then
        open(41,file='solver_11.dat',position='append')
        write (41,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(41)
      end if
!
      end subroutine solve_VJACOBI33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module solver_JACOBI33_DJDS
