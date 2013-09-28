!gauss_zeidel_w_comm_33.f90
!      module gauss_zeidel_w_comm_33
!
!     Written by Kemorin
!
!    complete Gauss-Zeidel (data communication for each hyperplane)
!
!       subroutine weak_gauss_zeidel_forward_33                         &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                      &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank)
!       subroutine weak_gauss_zeidel_backward_33                        &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                      &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank)
!
!       subroutine weak_gauss_zeidel_forward_3x33                       &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,    &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank)
!       subroutine weak_gauss_zeidel_backward_3x33                      &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,    &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank)
!
!    complete Gauss-Zeidel (data communication for each hyperplane)
!
!       subroutine full_gauss_zeidel_forward_33                         &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                      &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank)
!       subroutine full_gauss_zeidel_backward_33                        &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                      &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank)
!
!       subroutine full_gauss_zeidel_forward_3x33                       &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,    &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank)
!       subroutine full_gauss_zeidel_backward_3x33                      &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,    &
!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!     &            SOLVER_COMM, my_rank)
!
!        at (9*i-8, 9*i-4, 9*i) 
!       ALU_U = 1 / Diag (Ordered by DJDS ordering for upper component)
!        else
!       ALU_U = Diag (Ordered by DJDS ordering for upper component)
!
      module gauss_zeidel_w_comm_33
!
      use calypso_mpi
      use m_precision
!
      use m_solver_count_time
      use gauss_zeidel_33
      use solver_SR_3
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine weak_gauss_zeidel_forward_33                          &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                       &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B(3*NP)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer(kind = kint) :: iv
!
!
       call gauss_zeidel_forward_33                                     &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
!
       end subroutine weak_gauss_zeidel_forward_33
!
!  ---------------------------------------------------------------------
!
       subroutine weak_gauss_zeidel_backward_33                         &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                       &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B(3*NP)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
       call gauss_zeidel_backward_33                                    &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
!
       end subroutine weak_gauss_zeidel_backward_33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine weak_gauss_zeidel_forward_3x33                        &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,     &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), B2(3*NP), B3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
        call gauss_zeidel_forward_3x33                                  &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
        START_TIME= MPI_WTIME()
        call solver_send_recv_3x3                                       &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S1, S2, S3)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
       end subroutine weak_gauss_zeidel_forward_3x33
!
!  ---------------------------------------------------------------------
!
       subroutine weak_gauss_zeidel_backward_3x33                       &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,     &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), B2(3*NP), B3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
        call gauss_zeidel_backward_3x33                                 &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
        START_TIME= MPI_WTIME()
        call solver_send_recv_3x3                                       &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S1, S2, S3)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
       end subroutine weak_gauss_zeidel_backward_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine full_gauss_zeidel_forward_33                          &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                       &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B(3*NP)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer(kind = kint) :: iv
!
!
      do iv= 1, NVECT
       call gauss_zeidel_each_plane_33                                  &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S)
         END_TIME= MPI_WTIME()
         COMMtime = COMMtime + END_TIME - START_TIME
      end do
!
       end subroutine full_gauss_zeidel_forward_33
!
!  ---------------------------------------------------------------------
!
       subroutine full_gauss_zeidel_backward_33                         &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, NEIBPETOT, NEIBPE,                       &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B(3*NP)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer(kind = kint) :: iv
!
!
      do iv= NVECT, 1, -1
       call gauss_zeidel_each_plane_33                                  &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
!

        call SOLVER_SEND_RECV_3                                         &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S)
          END_TIME= MPI_WTIME()
          COMMtime = COMMtime + END_TIME - START_TIME
         END_TIME= MPI_WTIME()
         COMMtime = COMMtime + END_TIME - START_TIME
      end do
!
       end subroutine full_gauss_zeidel_backward_33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine full_gauss_zeidel_forward_3x33                        &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,     &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), B2(3*NP), B3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer(kind = kint) :: iv
!
!
      do iv= 1, NVECT
        call gauss_zeidel_each_plane_3x33                               &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
        START_TIME= MPI_WTIME()
        call solver_send_recv_3x3                                       &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S1, S2, S3)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
      end do
!
       end subroutine full_gauss_zeidel_forward_3x33
!
!  ---------------------------------------------------------------------
!
       subroutine full_gauss_zeidel_backward_3x33                       &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, NEIBPETOT, NEIBPE,     &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SOLVER_COMM, my_rank)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), B2(3*NP), B3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer(kind = kint) :: iv
!
!
      do iv= NVECT, 1, -1
        call gauss_zeidel_each_plane_3x33                               &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
        START_TIME= MPI_WTIME()
        call solver_send_recv_3x3                                       &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, S1, S2, S3)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
      end do
!
       end subroutine full_gauss_zeidel_backward_3x33
!
!  ---------------------------------------------------------------------
!
      end module gauss_zeidel_w_comm_33
