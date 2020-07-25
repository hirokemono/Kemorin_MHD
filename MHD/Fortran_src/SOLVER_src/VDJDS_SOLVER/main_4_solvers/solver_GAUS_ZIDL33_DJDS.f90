!Csolver_GAUS_ZIDL33_DJDS.f90
!C*** 
!C*** module solver_GAUS_ZIDL33_DJDS
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!!C
!!C***
!!C***  VGAUSS_ZEIDEL33_DJDS_SMP
!!C***
!!      subroutine VGAUSS_ZEIDEL33_DJDS_SMP                             &
!!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,            &
!!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,              &
!!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,         &
!!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,            &
!!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                    &
!!     &           STACK_IMPORT, NOD_IMPORT,                            &
!!     &           STACK_EXPORT, NOD_EXPORT, PRECOND, SR_sig, SR_r)
!!C
!!      subroutine init_VGAUSS_ZEIDEL33_DJDS_SMP(NP, PEsmpTOT)
!!      subroutine solve_VGAUSS_ZEIDEL33_DJDS_SMP                       &
!!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,            &
!!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,              &
!!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,         &
!!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,            &
!!     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                    &
!!     &           STACK_IMPORT, NOD_IMPORT,                            &
!!     &           STACK_EXPORT, NOD_EXPORT, PRECOND, SR_sig, SR_r)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!C
!!C     VCG_DJDS_SMP solves the linear system Ax = b 
!!C     using the Conjugate Gradient iterative method with preconditioning.
!!C     Elements are ordered in descending Jagged Diagonal Storage
!!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!!C
      module solver_GAUS_ZIDL33_DJDS
!
      use m_precision
      use t_solver_SR
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
!C
      subroutine VGAUSS_ZEIDEL33_DJDS_SMP                               &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND, SR_sig, SR_r)

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
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call init_VGAUSS_ZEIDEL33_DJDS_SMP(NP, PEsmpTOT)
!
      call solve_VGAUSS_ZEIDEL33_DJDS_SMP                               &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND, SR_sig, SR_r)
!
      end subroutine VGAUSS_ZEIDEL33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine init_VGAUSS_ZEIDEL33_DJDS_SMP(NP, PEsmpTOT)
!
      use m_work_4_CG
      use djds_matrix_calcs_33
      use symmetric_gauss_zeidel_33
!
      integer(kind=kint ), intent(in) :: NP, PEsmpTOT
!
!
      call verify_work_4_matvec33(NP, ntotWK_CG)
!
      end subroutine init_VGAUSS_ZEIDEL33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine solve_VGAUSS_ZEIDEL33_DJDS_SMP                         &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND, SR_sig, SR_r)
!
      use calypso_mpi
!
      use solver_SR_3
!
      use m_work_4_CG
      use m_solver_count_time
      use symmetric_gauss_zeidel_33
!
      use cal_norm_products_33
      use vector_calc_solver_33
      use djds_matrix_calcs_33
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

      real(kind=kreal), intent(in) :: ALU_L(N), ALU_U(N)

      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 

      real(kind=kreal), intent(inout) :: B(NP)
      real(kind=kreal), intent(inout) :: X(NP)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r

      integer(kind=kint ), parameter :: iter_gauss = 1
!
      integer(kind=kint ) :: npLX1, npUX1, i
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
!$omp workshare
      W(1:3*NP,1:ntotWK_CG) = 0.0d0
!$omp end workshare
!
      call reset_solver_time
!
!
!C
!C-- change B,X
!
       call change_order_2_solve_bx3(NP, PEsmpTOT, STACKmcG,            &
     &     NtoO, B, X, W(1,iWK))
!
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===
      call cal_local_norm_3(NP, PEsmpTOT, STACKmcG, B, BNRM20)
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do iter= 1, MAXIT
!C
!C******************************************** Gauss-Zeidel iteration
!
         call weak_sym_gauss_zeidel_33                                  &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, X, W(1,iWK), NEIBPETOT, NEIBPE,             &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            iter_gauss, SR_sig, SR_r)
!
!
         call subtruct_matvec_33                                        &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,            &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W(1,R), B, X, W(1,iWK))
!
!C
!C +---------------+
!C | DNRM2 = B^2   |
!C +---------------+
!C===
         call cal_local_norm_3(NP, PEsmpTOT, STACKmcG, W(1,R), DNRM20)
!
         START_TIME= MPI_WTIME()
         call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,            &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
         END_TIME= MPI_WTIME()
         COMMtime = COMMtime + END_TIME - START_TIME
!
         RESID= dsqrt(DNRM2/BNRM2)

         if (IER.eq.1 .and. my_rank.eq.0) write(12,'(a,i5,1p2e16.6)')   &
     &               'solver_GAUSS_ZEIDEL33_DJDS_SMP: ', ITER, RESID
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
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C== change B,X

       call back_2_original_order_bx3(NP, NtoO, B, X, W(1,iWK))

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
      end subroutine solve_VGAUSS_ZEIDEL33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module solver_GAUS_ZIDL33_DJDS
