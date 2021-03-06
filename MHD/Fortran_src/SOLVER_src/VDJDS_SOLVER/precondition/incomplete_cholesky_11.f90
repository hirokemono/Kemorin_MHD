!
!      module incomplete_cholesky_11
!
!     Written by Kemorin
!
!C
!C***  Incomplete Cholesky
!C***
!C
!       subroutine incomplete_cholesky_1x11                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_L, ALU_U, S, V, W2)
!       subroutine incomplete_cholesky_3x11                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_L, ALU_U, S1, S2, S3, V1, V2, V3, W6)
!
      module incomplete_cholesky_11
!
      use m_precision
!
      use ordering_by_o2nl_11
      use ordering_by_l2u_11
      use ordering_by_new2old_U_11
      use vector_calc_solver_11
      use forward_substitute_11
      use backward_substitute_11
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
       subroutine incomplete_cholesky_1x11                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, S, V, W2)
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
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
       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind=kreal), intent(inout) :: W2(NP,2)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
!
        call ordering_1x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, S, V )
        call clear_external_solve_11(N, NP, S )
        call clear_vector_solve_11(NP, W2(1,IZ2) )
!C
!C== forward substitution

      call forward_substitute_1x11(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &    npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)

      call ordering_1x1_by_l2u(NP, LtoU, W2(1,IZ1), S )

!C
!C== backward substitution

      call backward_substitute_1x11(N, NP, NU, NPU, PEsmpTOT, NVECT,    &
     &   npUX1, STACKmc, NUhyp, INU, IAU, W2(1,IZ1), W2(1,IZ2),         &
     &   AU, ALU_U)

      call ordering_1x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &           NtoO_U, S, W2(1,IZ1) )
!
      end subroutine incomplete_cholesky_1x11
!
!  ---------------------------------------------------------------------
!
       subroutine incomplete_cholesky_3x11                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, S1, S2, S3, V1, V2, V3, W6)
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
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
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
       real(kind=kreal), intent(inout) :: W6(NP,6)
!
!
      call ordering_1x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,            &
     &    OtoN_L, S1, S2, S3, V1, V2, V3 )
      call clear_external_solve_3x11(N, NP, S1, S2, S3 )
      call clear_vector_solve_3x11                                      &
     &    (NP, W6(1,IR1), W6(1,IR2), W6(1,IR3) )
!C
!C== forward substitution

      call forward_substitute_3x11(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &    npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL, ALU_L)

      call ordering_1x3_by_l2u                                          &
     &    (NP, LtoU, W6(1,IZ1), W6(1,IZ2), W6(1,IZ3), S1, S2, S3)

!C
!C== backward substitution

      call backward_substitute_3x11(N, NP, NU, NPU, PEsmpTOT, NVECT,    &
     &   npUX1, STACKmc, NUhyp, INU, IAU, W6(1,IZ1), W6(1,IZ2),         &
     &   W6(1,IZ3), W6(1,IR1), W6(1,IR2), W6(1,IR3), AU, ALU_U)

      call ordering_1x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &           NtoO_U, S1, S2, S3, W6(1,IZ1), W6(1,IZ2), W6(1,IZ3) )
!
      end subroutine incomplete_cholesky_3x11
!
!  ---------------------------------------------------------------------
!
      end module incomplete_cholesky_11
