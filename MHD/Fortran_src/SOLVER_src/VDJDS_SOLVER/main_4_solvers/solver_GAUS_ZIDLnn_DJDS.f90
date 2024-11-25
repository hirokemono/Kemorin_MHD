!Csolver_GAUS_ZIDLnn_DJDS.f90
!C*** 
!C*** module solver_GAUS_ZIDLnn_DJDS
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!C
!C***
!C***  VGAUSS_ZEIDELnn_DJDS_SMP
!C***
!!      subroutine VGAUSS_ZEIDELnn_DJDS_SMP                             &
!!     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,        &
!!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,              &
!!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,         &
!!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,            &
!!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                    &
!!     &           STACK_IMPORT, NOD_IMPORT,                            &
!!     &           STACK_EXPORT, NOD_EXPORT, PRECOND,                   &
!!     &           SR_sig, SR_r, INITtime, COMPtime, COMMtime)
!!C
!!      subroutine init_VGAUSS_ZEIDELnn_DJDS_SMP(NP, NB,                &
!!     &                                         PEsmpTOT, INITtime)
!!      subroutine solve_VGAUSS_ZEIDELnn_DJDS_SMP                       &
!!     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,        &
!!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,              &
!!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,         &
!!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,            &
!!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                    &
!!     &           STACK_IMPORT, NOD_IMPORT,                            &
!!     &           STACK_EXPORT, NOD_EXPORT, PRECOND,                   &
!!     &           SR_sig, SR_r, COMPtime, COMMtime)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!C
!!C     VCG_DJDS_SMP solves the linear system Ax = b 
!!C     using the Conjugate Gradient iterative method with preconditioning.
!!C     Elements are ordered in descending Jagged Diagonal Storage
!!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!!C
      module solver_GAUS_ZIDLnn_DJDS
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
!C
      subroutine VGAUSS_ZEIDELnn_DJDS_SMP                               &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND,                     &
     &           SR_sig, SR_r, INITtime, COMPtime, COMMtime)
!
      integer(kind=kint ), intent(in) :: N, NP, NB
      integer(kind=kint ), intent(in) :: NL, NU, NPL, NPU, NVECT
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

      real(kind=kreal), intent(in) :: ALU_L(N), ALU_U(N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
      real(kind=kreal), intent(inout) :: B(NP)
      real(kind=kreal), intent(inout) :: X(NP)
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
      call init_VGAUSS_ZEIDELnn_DJDS_SMP(NP, NB, PEsmpTOT, INITtime)
!
      call solve_VGAUSS_ZEIDELnn_DJDS_SMP                               &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND,                     &
     &           SR_sig, SR_r, COMPtime, COMMtime)
!
      end subroutine VGAUSS_ZEIDELnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_VGAUSS_ZEIDELnn_DJDS_SMP(NP, NB,                  &
     &                                         PEsmpTOT, INITtime)
!
      use djds_matrix_calcs_nn
      use symmetric_gauss_zeidel_nn
!
      integer(kind=kint ), intent(in) :: NP, NB, PEsmpTOT
!>      Elapsed time for initialization
      real(kind = kreal), intent(inout) :: INITtime
!
      real(kind = kreal) :: START_TIME
!
!
      START_TIME= MPI_WTIME()
      ntotWK_CG = nWK_CG + 3
      call verify_work_4_matvecnn(NP, NB, ntotWK_CG)
      INITtime = MPI_WTIME() - START_TIME
!
      end subroutine init_VGAUSS_ZEIDELnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine solve_VGAUSS_ZEIDELnn_DJDS_SMP                         &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND,                     &
     &           SR_sig, SR_r, COMPtime, COMMtime)
!
      use solver_SR_N
!
      use m_CG_constants
      use symmetric_gauss_zeidel_nn
!
      use cal_norm_products_nn
      use vector_calc_solver_nn
      use djds_matrix_calcs_nn
!
      integer(kind=kint ), intent(in) :: N, NP, NB
      integer(kind=kint ), intent(in) :: NL, NU, NPL, NPU, NVECT
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

      real(kind=kreal), intent(in) :: ALU_L(N), ALU_U(N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
      real(kind=kreal), intent(inout) :: B(NP)
      real(kind=kreal), intent(inout) :: X(NP)
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
      integer(kind=kint ), parameter :: iter_gauss = 1
!
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: BNRM2,  DNRM2
      real(kind=kreal) :: BNRM20, DNRM20
!
!
      integer(kind=kint ) :: npLX1, npUX1
      integer(kind=kint ) :: ITER, MAXIT
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
!
      MAXIT= ITR
      TOL  = EPS
      S1_TIME= MPI_WTIME()
      COMMtime = 0.0d0
!
!$omp workshare
      W(1:NB*NP,1:ntotWK_CG) = 0.0d0
!$omp end workshare
!
!
!C
!C-- change B,X
!
       call change_order_2_solve_bxn(NP, NB, PEsmpTOT, STACKmcG,        &
     &     NtoO, B, X, W(1,iWK))
!
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===
       call change_order_2_solve_bxn(NP, NB, PEsmpTOT, STACKmcG,        &
     &           NtoO, B, X, W(1,iWK))
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do ITER= 1, MAXIT
!C
!C******************************************** Gauss-Zeidel iteration
!
         call weak_sym_gauss_zeidel_nn                                  &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, X, W(1,iWK), NEIBPETOT, NEIBPE,             &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            iter_gauss, SR_sig, SR_r, COMMtime)
!
!
         call subtruct_matvec_nn                                        &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,R), B, X, W(1,iWK))
!
!C
!C +---------------+
!C | DNRM2 = B^2   |
!C +---------------+
!C===
         call cal_local_norm_n(NP, NB, PEsmpTOT, STACKmcG, W(1,R),      &
     &       DNRM20)
!
         START_TIME= MPI_WTIME()
         call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,            &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
         COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
         RESID= dsqrt(DNRM2/BNRM2)

         if (IER.eq.1 .and. my_rank.eq.0) write(12,'(a,i5,1p2e16.6)')   &
     &               'solver_GAUSS_ZEIDELnn_DJDS_SMP: ', ITER, RESID
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
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

!C
!C== change B,X

      call back_2_original_order_bxn(NP, NB, NtoO, B, X, W(1,iWK))

      IER = 0
      COMPtime = MPI_WTIME() - S1_TIME
!
      end subroutine solve_VGAUSS_ZEIDELnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module solver_GAUS_ZIDLnn_DJDS
