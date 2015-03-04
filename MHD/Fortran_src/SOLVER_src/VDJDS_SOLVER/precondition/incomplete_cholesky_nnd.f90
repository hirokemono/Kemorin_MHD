!
!      module incomplete_cholesky_nnd
!
!     Written by Kemorin
!C
!C***  Incomplete Cholesky
!C***
!C
!       subroutine incomplete_cholesky_1xnnd                            &
!     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,    &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_L, ALU_U, S, V, W2)
!       subroutine incomplete_cholesky_3xnnd                            &
!     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,    &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_L, ALU_U, S1, S2, S3, V1, V2, V3, W6)
!
      module incomplete_cholesky_nnd
!
      use m_precision
!
      use ordering_by_o2nl_nn
      use ordering_by_l2u_nn
      use ordering_by_new2old_U_nn
      use vector_calc_solver_nn
      use forward_substitute_nnd
      use backward_substitute_nnd
!
      implicit none
!
       integer(kind = kint), parameter :: IZ1 = 1, IZ2 = 2, IZ3 = 3
       integer(kind = kint), parameter :: IR1 = 4, IR2 = 5, IR3 = 6
       private :: IZ1, IZ2, IZ3, IR1, IR2, IR3
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine incomplete_cholesky_1xnnd                             &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, S, V, W2)
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
       real(kind=kreal), intent(inout) :: W2(NB*NP,2)
!
!
      call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,        &
     &    OtoN_L, S, V )
      call clear_external_solve_nn(N, NP, NB, S )
      call clear_vector_solve_nn(NP, NB, W2(1,IZ2) )
!C
!C== forward substitution

      call forward_substitute_1xnnd(N, NP, NB, NL, NPL, PEsmpTOT,       &
     &    NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)

      call ordering_nx1_by_l2u(NP, NB, LtoU, W2(1,IZ1), S )

!C
!C== backward substitution

      call backward_substitute_1xnnd(N, NP, NB, NU, NPU, PEsmpTOT,      &
     &    NVECT,npUX1, STACKmc, NUhyp, INU, IAU, W2(1,IZ1), W2(1,IZ2),  &
     &    AU, ALU_U)

      call ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,        &
     &    NtoO_U, S, W2(1,IZ1) )
!
      end subroutine incomplete_cholesky_1xnnd
!
!  ---------------------------------------------------------------------
!
       subroutine incomplete_cholesky_3xnnd                             &
     &           (N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, S1, S2, S3, V1, V2, V3, W6)
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
!
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
       real(kind=kreal), intent(inout) :: W6(NB*NP,6)
!
!
      call ordering_nx3_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,        &
     &    OtoN_L, S1, S2, S3, V1, V2, V3)
      call clear_external_solve_3xnn(N, NP, NB, S1, S2, S3)
      call clear_vector_solve_3xnn                                      &
     &    (NP, NB, W6(1,IR1), W6(1,IR2), W6(1,IR3) )
!C
!C== forward substitution

      call forward_substitute_3xnnd(N, NP, NB, NL, NPL, PEsmpTOT,       &
     &    NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3,           &
     &    AL, ALU_L)

      call ordering_nx3_by_l2u                                          &
     &    (NP, NB, LtoU, W6(1,IZ1), W6(1,IZ2), W6(1,IZ3), S1, S2, S3)

!C
!C== backward substitution

      call backward_substitute_3xnnd(N, NP, NB, NU, NPU, PEsmpTOT,      &
     &    NVECT, npUX1, STACKmc, NUhyp, INU, IAU, W6(1,IZ1), W6(1,IZ2), &
     &    W6(1,IZ3), W6(1,IR1), W6(1,IR2), W6(1,IR3), AU, ALU_U)

      call ordering_nx3_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,        &
     &    NtoO_U, S1, S2, S3, W6(1,IZ1), W6(1,IZ2), W6(1,IZ3) )
!
      end subroutine incomplete_cholesky_3xnnd
!
!  ---------------------------------------------------------------------
!
      end module incomplete_cholesky_nnd
