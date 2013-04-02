!symmetric_gauss_zeidel_11.f90
!      module symmetric_gauss_zeidel_11
!
!     Written by Kemorin
!
!       subroutine weak_sym_gauss_zeidel_11                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                      &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank, niter)
!
!       subroutine full_sym_gauss_zeidel_11                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                      &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank, niter)
!
!
!       subroutine weak_sym_gauss_zeidel_3x11                           &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,    &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank, niter)
!
!       subroutine full_sym_gauss_zeidel_3x11                           &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,    &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank, niter)
!
      module symmetric_gauss_zeidel_11
!
      use m_precision
!
      use m_solver_count_time
      use gauss_zeidel_11
      use solver_SR
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine weak_sym_gauss_zeidel_11                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                       &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank, niter)
!
      use calypso_mpi
!
      integer(kind=kint ), intent(in) :: niter
!
      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: B(NP)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer(kind = kint) :: icou
!
!
      do icou = 1, niter
        call gauss_zeidel_forward_11                                    &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S, SOLVER_COMM, my_rank)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
!
        call gauss_zeidel_backward_11                                   &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S, SOLVER_COMM, my_rank)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
      end do
!
      end subroutine weak_sym_gauss_zeidel_11
!
!  ---------------------------------------------------------------------
!
       subroutine full_sym_gauss_zeidel_11                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                       &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank, niter)
!
      use calypso_mpi
!
      integer(kind=kint ), intent(in) :: niter
!
      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: B(NP)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer(kind = kint) :: icou, iv
!
!
      do icou = 1, niter
        do iv= 1, NVECT
         call gauss_zeidel_each_plane_11                                &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
!
          START_TIME= MPI_WTIME()
          call SOLVER_SEND_RECV                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S, SOLVER_COMM, my_rank)
           END_TIME= MPI_WTIME()
           COMMtime = COMMtime + END_TIME - START_TIME
        end do
!
        do iv= NVECT, 1, -1
         call gauss_zeidel_each_plane_11                                &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
!
        START_TIME= MPI_WTIME()
          call SOLVER_SEND_RECV                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S, SOLVER_COMM, my_rank)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
        end do
      end do
!
      end subroutine full_sym_gauss_zeidel_11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine weak_sym_gauss_zeidel_3x11                            &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,     &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank, niter)
!
      use calypso_mpi
!
      integer(kind=kint ), intent(in) :: niter
!
      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_U(NP), OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: B1(NP), B2(NP), B3(NP)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer(kind = kint) :: icou
!
!
      do icou = 1, niter
        call gauss_zeidel_forward_3x11                                  &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECVx3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S1, S2, S3,                        &
     &     SOLVER_COMM, my_rank)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
        call gauss_zeidel_backward_3x11                                 &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECVx3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S1, S2, S3,                        &
     &     SOLVER_COMM, my_rank)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
      end do
!
       end subroutine weak_sym_gauss_zeidel_3x11
!
!  ---------------------------------------------------------------------
!
       subroutine full_sym_gauss_zeidel_3x11                            &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,     &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank, niter)
!
      use calypso_mpi
!
      integer(kind=kint ), intent(in) :: niter
!
      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_U(NP), OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: B1(NP), B2(NP), B3(NP)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer(kind = kint) :: icou, iv
!
!
      do icou = 1, niter
        do iv= 1, NVECT
          call gauss_zeidel_each_plane_3x11                             &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
          START_TIME= MPI_WTIME()
          call SOLVER_SEND_RECVx3                                       &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S1, S2, S3,                        &
     &     SOLVER_COMM, my_rank)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
        end do
!
        do iv= NVECT, 1, -1
          call gauss_zeidel_each_plane_3x11                             &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
          START_TIME= MPI_WTIME()
          call SOLVER_SEND_RECVx3                                       &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S1, S2, S3,                        &
     &     SOLVER_COMM, my_rank)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
!
        end do
      end do
!
      end subroutine full_sym_gauss_zeidel_3x11
!
!  ---------------------------------------------------------------------
!
      end module symmetric_gauss_zeidel_11
