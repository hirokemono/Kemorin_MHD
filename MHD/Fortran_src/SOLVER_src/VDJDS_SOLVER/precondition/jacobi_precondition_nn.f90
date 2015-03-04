!jacobi_precondition_nn.f90
!      module jacobi_precondition_nn
!
!     Written by Kemorin
!
!C
!C***  Gauss method
!C***
!C
!      subroutine jacobi_forward_NN                                     &
!     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,           &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            AL, AU, ALU_U, S, V, W3)
!      subroutine jacobi_forward_3xNN                                   &
!     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,           &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            AL, AU, ALU_U, S1, S2, S3, V1, V2, V3, W9)
!
      module jacobi_precondition_nn
!
      use m_precision
!
      use ordering_by_o2nl_nn
      use ordering_by_l2u_o2nu_nn
      use ordering_by_new2old_U_nn
      use cal_4_lower_nn
      use cal_4_upper_nn
      use diagonal_scaling_nn
!
      implicit none
!
       integer(kind = kint), parameter :: IZ1 = 1, IZ2 = 2, IZ3 = 3
       integer(kind = kint), parameter :: IZ4 = 4, IZ5 = 5, IZ6 = 6
       integer(kind = kint), parameter :: IZ7 = 7, IZ8 = 8, IZ9 = 9
       private :: IZ1, IZ2, IZ3, IZ4, IZ5, IZ6, IZ7, IZ8, IZ9
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine jacobi_forward_NN                                      &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,            &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            AL, AU, ALU_U, S, V, W3)
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU, NB
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
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: V(NB*NP)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
       real(kind=kreal), intent(inout) :: W3(NB*NP,3)
!
!
       call ordering_nx2_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,       &
     &      OtoN_L, W3(1,IZ2), W3(1,IZ1), S, V)
!
       call subtract_lower_nn(NP, NB, NL, NPL, PEsmpTOT, NVECT, npLX1,  &
     &     STACKmc, NLhyp, INL, IAL, W3(1,IZ2), AL, W3(1,IZ1) )
!
       call ordering_nx1_l2u_o2n_u(NP, NB, OtoN_U, LtoU,                &
     &     W3(1,IZ1), W3(1,IZ3), S, W3(1,IZ2) )
!
       call subtract_upper_nn(NP, NB, NU, NPU, PEsmpTOT, NVECT, npUX1,  &
     &     STACKmc, NUhyp, INU, IAU, W3(1,IZ3), AU, W3(1,IZ1) )
!
       call ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,       &
     &     NtoO_U, W3(1,IZ1), W3(1,IZ3) )
!
       call diag_scaling_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,            &
     &     S, W3(1,IZ1), ALU_U)
!
      end subroutine jacobi_forward_NN
!
!  ---------------------------------------------------------------------
!
      subroutine jacobi_forward_3xNN                                    &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,            &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            AL, AU, ALU_U, S1, S2, S3, V1, V2, V3, W9)
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU, NB
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
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
!
       real(kind=kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind=kreal), intent(inout) :: S3(NB*NP)
       real(kind=kreal), intent(inout) :: W9(NB*NP,9)
!
!
       call ordering_nx6_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,       &
     &     OtoN_L, W9(1,IZ4), W9(1,IZ5), W9(1,IZ6), W9(1,IZ1),          &
     &     W9(1,IZ2), W9(1,IZ3), S1, S2, S3, V1, V2, V3)
!
       call subtract_lower_3xnn(NP, NB, NL, NPL, PEsmpTOT, NVECT,       &
     &     npLX1, STACKmc, NLhyp, INL, IAL, W9(1,IZ4), W9(1,IZ5),       &
     &     W9(1,IZ6), AL, W9(1,IZ1), W9(1,IZ2), W9(1,IZ3) )
!
       call ordering_nx3_l2u_o2n_u(NP, NB, OtoN_U, LtoU,                &
     &     W9(1,IZ1), W9(1,IZ2), W9(1,IZ3),                             &
     &     W9(1,IZ7), W9(1,IZ8), W9(1,IZ9), S1, S2, S3,                 &
     &     W9(1,IZ4), W9(1,IZ5), W9(1,IZ6) )
!
       call subtract_upper_3xnn(NP, NB, NU, NPU, PEsmpTOT, NVECT,       &
     &     npUX1, STACKmc, NUhyp, INU, IAU, W9(1,IZ7), W9(1,IZ8),       &
     &      W9(1,IZ9), AU, W9(1,IZ1), W9(1,IZ2), W9(1,IZ3) )
!
       call ordering_nx3_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,       &
     &     NtoO_U, W9(1,IZ1), W9(1,IZ2), W9(1,IZ3),                     &
     &     W9(1,IZ7), W9(1,IZ8), W9(1,IZ9) )
!
       call diag_scaling_3xnn(NP, N, NB, PEsmpTOT, STACKmcG,            &
     &     S1, S2, S3, W9(1,IZ1), W9(1,IZ2), W9(1,IZ3), ALU_U)
!
      end subroutine jacobi_forward_3xNN
!
!  ---------------------------------------------------------------------
!
      end module jacobi_precondition_nn
