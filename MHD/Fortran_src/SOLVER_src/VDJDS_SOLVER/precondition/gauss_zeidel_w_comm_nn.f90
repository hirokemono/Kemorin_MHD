!>@file   gauss_zeidel_w_comm_nn.f90
!!@brief  module gauss_zeidel_w_comm_nn
!!
!!@author H. Matsui
!!@date Programmed in 20??
!
!>@brief  complete Gauss-Zeidel (data communication for each hyperplane)
!!
!!@verbatim
!!       subroutine weak_gauss_zeidel_forward_nn                        &
!!     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,   &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B, S, W3, NEIBPETOT, NEIBPE,                 &
!!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT, &
!!     &            SR_sig, SR_r, COMMtime)
!!       subroutine weak_gauss_zeidel_backward_nn                       &
!!     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,   &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B, S, W3, NEIBPETOT, NEIBPE,                 &
!!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT, &
!!     &            SR_sig, SR_r, COMMtime)
!!
!!       subroutine weak_gauss_zeidel_forward_3xnn                      &
!!     &          (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,    &
!!     &           PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!!     &           OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!!     &           ALU_U, B1, B2, B3, S1, S2, S3, W9, NEIBPETOT, NEIBPE,&
!!     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!!     &           SR_sig, SR_r, COMMtime)
!!       subroutine weak_gauss_zeidel_backward_3xnn                     &
!!     &          (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,    &
!!     &           PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!!     &           OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!!     &           ALU_U, B1, B2, B3, S1, S2, S3, W9, NEIBPETOT, NEIBPE,&
!!     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!!     &           SR_sig, SR_r, COMMtime)
!!
!!    complete Gauss-Zeidel (data communication for each hyperplane)
!!
!!       subroutine full_gauss_zeidel_forward_nn                        &
!!     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,   &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B, S, W3, NEIBPETOT, NEIBPE,                 &
!!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT, &
!!     &            SR_sig, SR_r, COMMtime)
!!       subroutine full_gauss_zeidel_backward_nn                       &
!!     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,   &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B, S, W3, NEIBPETOT, NEIBPE,                 &
!!     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT, &
!!     &            SR_sig, SR_r, COMMtime)
!!
!!       subroutine full_gauss_zeidel_forward_3xnn                      &
!!     &          (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,    &
!!     &           PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!!     &           OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!!     &           ALU_U, B1, B2, B3, S1, S2, S3, W9, NEIBPETOT, NEIBPE,&
!!     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!!     &            SR_sig, SR_r, COMMtime)
!!       subroutine full_gauss_zeidel_backward_3xnn                     &
!!     &          (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,    &
!!     &           PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!!     &           OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!!     &           ALU_U, B1, B2, B3, S1, S2, S3, W9, NEIBPETOT, NEIBPE,&
!!     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,  &
!!     &            SR_sig, SR_r, COMMtime)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!        at (NB*NB*i-8, NB*NB*i-4, NB*NB*i) 
!!       ALU_U = 1 / Diag (Ordered by DJDS ordering for upper component)
!!        else
!!       ALU_U = Diag (Ordered by DJDS ordering for upper component)
!!@endverbatim
!
      module gauss_zeidel_w_comm_nn
!
      use calypso_mpi
      use m_precision
!
      use t_solver_SR
      use gauss_zeidel_nn
      use solver_SR_N
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine weak_gauss_zeidel_forward_nn                          &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3, NEIBPETOT, NEIBPE,                   &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SR_sig, SR_r, COMMtime)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
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
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: B(NB*NP)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
       real(kind = kreal), intent(inout) :: W3(NB*NP,3)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME
!
       call gauss_zeidel_forward_nn                                     &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
!
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_N                                         &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, S)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
       end subroutine weak_gauss_zeidel_forward_nn
!
!  ---------------------------------------------------------------------
!
       subroutine weak_gauss_zeidel_backward_nn                         &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3, NEIBPETOT, NEIBPE,                   &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SR_sig, SR_r, COMMtime)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
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
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: B(NB*NP)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
       real(kind = kreal), intent(inout) :: W3(NB*NP,3)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME
!
       call gauss_zeidel_backward_nn                                    &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
!
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_N                                         &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, S)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
       end subroutine weak_gauss_zeidel_backward_nn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine weak_gauss_zeidel_forward_3xnn                        &
     &          (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,      &
     &           PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,     &
     &           OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,      &
     &           ALU_U, B1, B2, B3, S1, S2, S3, W9, NEIBPETOT, NEIBPE,  &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           SR_sig, SR_r, COMMtime)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
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
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: B1(NB*NP), B2(NB*NP)
       real(kind = kreal), intent(in) :: B3(NB*NP)
!
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
       real(kind = kreal), intent(inout) :: W9(NB*NP,9)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME
!
        call gauss_zeidel_forward_3xnn                                  &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_nx3                                       &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, S1, S2, S3)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
       end subroutine weak_gauss_zeidel_forward_3xnn
!
!  ---------------------------------------------------------------------
!
       subroutine weak_gauss_zeidel_backward_3xnn                       &
     &          (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,      &
     &           PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,     &
     &           OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,      &
     &           ALU_U, B1, B2, B3, S1, S2, S3, W9, NEIBPETOT, NEIBPE,  &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           SR_sig, SR_r, COMMtime)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
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
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: B1(NB*NP), B2(NB*NP)
       real(kind = kreal), intent(in) :: B3(NB*NP)
!
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
       real(kind = kreal), intent(inout) :: W9(NB*NP,9)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME
!
!
        call gauss_zeidel_backward_3xnn                                 &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_nx3                                       &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, S1, S2, S3)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
       end subroutine weak_gauss_zeidel_backward_3xnn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine full_gauss_zeidel_forward_nn                          &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3, NEIBPETOT, NEIBPE,                   &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SR_sig, SR_r, COMMtime)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
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
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: B(NB*NP)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
       real(kind = kreal), intent(inout) :: W3(NB*NP,3)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME
!
       integer(kind = kint) :: iv
!
!
      do iv= 1, NVECT
       call gauss_zeidel_each_plane_nn                                  &
     &           (iv, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
!
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_N                                         &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, S)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
      end do
!
       end subroutine full_gauss_zeidel_forward_nn
!
!  ---------------------------------------------------------------------
!
       subroutine full_gauss_zeidel_backward_nn                         &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3, NEIBPETOT, NEIBPE,                   &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            SR_sig, SR_r, COMMtime)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
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
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: B(NB*NP)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
       real(kind = kreal), intent(inout) :: W3(NB*NP,3)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME
!
       integer(kind = kint) :: iv
!
!
      do iv= NVECT, 1, -1
       call gauss_zeidel_each_plane_nn                                  &
     &           (iv, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
!
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_N                                         &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, S)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
      end do
!
       end subroutine full_gauss_zeidel_backward_nn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine full_gauss_zeidel_forward_3xnn                        &
     &          (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,      &
     &           PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,     &
     &           OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,      &
     &           ALU_U, B1, B2, B3, S1, S2, S3, W9, NEIBPETOT, NEIBPE,  &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           SR_sig, SR_r, COMMtime)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
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
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: B1(NB*NP), B2(NB*NP)
       real(kind = kreal), intent(in) :: B3(NB*NP)
!
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
       real(kind = kreal), intent(inout) :: W9(NB*NP,9)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME
!
       integer(kind = kint) :: iv
!
!
      do iv= 1, NVECT
        call gauss_zeidel_each_plane_3xnn                               &
     &           (iv, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_nx3                                       &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, S1, S2, S3)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
      end do
!
       end subroutine full_gauss_zeidel_forward_3xnn
!
!  ---------------------------------------------------------------------
!
       subroutine full_gauss_zeidel_backward_3xnn                       &
     &          (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,      &
     &           PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,     &
     &           OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,      &
     &           ALU_U, B1, B2, B3, S1, S2, S3, W9, NEIBPETOT, NEIBPE,  &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           SR_sig, SR_r, COMMtime)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT)) 
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
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
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: B1(NB*NP), B2(NB*NP)
       real(kind = kreal), intent(in) :: B3(NB*NP)
!
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
       real(kind = kreal), intent(inout) :: W9(NB*NP,9)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME
!
       integer(kind = kint) :: iv
!
!
      do iv= NVECT, 1, -1
        call gauss_zeidel_each_plane_3xnn                               &
     &           (iv, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
!
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_nx3                                       &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, S1, S2, S3)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
      end do
!
       end subroutine full_gauss_zeidel_backward_3xnn
!
!  ---------------------------------------------------------------------
!
      end module gauss_zeidel_w_comm_nn
