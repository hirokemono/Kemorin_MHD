!
!      module i_cholesky_w_asdd_nn
!
!     Written by Kemorin
!
!C
!C***  Incomplete Cholesky
!C***     with additive SCHWARTZ Domain composition
!C***
!C
!      subroutine i_cholesky_w_asdd_1xnn                                &
!     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,  &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            D, AL, AU, ALU_L, ALU_U, STmp, SPre, VIni, W2)
!
!      subroutine i_cholesky_w_asdd_3xnn                                &
!     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,  &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            D, AL, AU, ALU_L, ALU_U, STmp1, STmp2, STmp3,        &
!     &            SPre1, SPre2, SPre3, VIni1, VIni2, VIni3, W6)
!
      module i_cholesky_w_asdd_nn
!
      use m_precision
!
      use ordering_by_o2nl_nn
      use ordering_by_l2u_nn
      use ordering_by_new2old_U_nn
      use vector_calc_solver_nn
      use djds_matrix_calcs_nn
      use forward_substitute_nn
      use backward_substitute_nn
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
      subroutine i_cholesky_w_asdd_1xnn                                 &
     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,   &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, STmp, SPre, VIni, W2)
!
       integer(kind = kint), intent(in) :: iterPRE
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
       real(kind = kreal), intent(in) :: D(NB*NB*NP)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: SPre(NB*NP), VIni(NB*NP)
!
       real(kind = kreal), intent(inout) :: STmp(NB*NP)
       real(kind = kreal), intent(inout) :: W2(NB*NP,2)
!
!
      call verify_work_4_matvecnn(NP,NB)
!
      if (iterPRE .eq. 1) then
        call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &          OtoN_L, STmp, VIni )
      else
!
!   additive SCHWARTZ domain decomposition
!
        call subtruct_matvec_nn                                         &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W2(1,IZ2), VIni, SPre)
!
        call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &          OtoN_L, STmp, W2(1,IZ2) )
      end if
!
      call clear_external_solve_nn(N, NP, NB, STmp )
      call clear_vector_solve_nn(NP, NB, W2(1,IZ2) )
!
!C
!C== forward substitution
!
      call forward_substitute_1xnn(N, NP, NB, NL, NPL, PEsmpTOT, NVECT, &
     &     npLX1, STACKmc, NLhyp, INL, IAL, STmp, AL, ALU_L)

      call ordering_nx1_by_l2u(NP, NB, LtoU, W2(1,IZ1), STmp )
!C
!C== backward substitution
!
       call backward_substitute_1xnn(N, NP, NB, NU, NPU, PEsmpTOT,      &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           W2(1,IZ1), W2(1,IZ2), AU, ALU_U)
!
       call ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,       &
     &           NtoO_U, STmp, W2(1,IZ1) )
!
      end subroutine i_cholesky_w_asdd_1xnn
!
!  ---------------------------------------------------------------------
!
      subroutine i_cholesky_w_asdd_3xnn                                 &
     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,   &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, STmp1, STmp2, STmp3,         &
     &            SPre1, SPre2, SPre3, VIni1, VIni2, VIni3, W6)
!
       integer(kind = kint), intent(in) :: iterPRE
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
       real(kind = kreal), intent(in) :: D(NB*NB*NP)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: SPre1(NB*NP), VIni1(NB*NP)
       real(kind = kreal), intent(in) :: SPre2(NB*NP), VIni2(NB*NP)
       real(kind = kreal), intent(in) :: SPre3(NB*NP), VIni3(NB*NP)
!
       real(kind = kreal), intent(inout) :: STmp1(NB*NP)
       real(kind = kreal), intent(inout) :: STmp2(NB*NP)
       real(kind = kreal), intent(inout) :: STmp3(NB*NP)
       real(kind = kreal), intent(inout) :: W6(NB*NP,6)
!
!
      call verify_work_4_matvecnn(NP,NB)
!
      if (iterPRE .eq. 1) then
        call ordering_nx3_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &          OtoN_L, STmp1, STmp2, STmp3, VIni1, VIni2, VIni3)
      else
!
!   additive SCHWARTZ domain decomposition
!
        call subtruct_matvec_3xnn                                       &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W6(1,IR1), W6(1,IR2), W6(1,IR3),                      &
     &            VIni1, VIni2, VIni3, SPre1, SPre2, SPre3)
!
        call ordering_nx3_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &            OtoN_L, STmp1, STmp2, STmp3, W6(1,IR1),               &
     &            W6(1,IR2), W6(1,IR3) )
      end if
!
      call clear_external_solve_3xnn(N, NP, NB, STmp1, STmp2, STmp3)
      call clear_vector_solve_3xnn(NP, NB, W6(1,IR1),                   &
     &            W6(1,IR2), W6(1,IR3) )
!
!C
!C== forward substitution
!
      call forward_substitute_3xnn(N, NP, NB, NL, NPL, PEsmpTOT, NVECT, &
     &            npLX1, STACKmc, NLhyp, INL, IAL, STmp1, STmp2, STmp3, &
     &            AL, ALU_L)

      call ordering_nx3_by_l2u(NP, NB, LtoU, W6(1,IZ1), W6(1,IZ2),      &
     &            W6(1,IZ3), STmp1, STmp2, STmp3 )
!C
!C== backward substitution
!
       call backward_substitute_3xnn(N, NP, NB, NU, NPU, PEsmpTOT,      &
     &            NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
     &            W6(1,IZ1), W6(1,IZ2), W6(1,IZ3), W6(1,IR1),           &
     &            W6(1,IR2), W6(1,IR3), AU, ALU_U)
!
       call ordering_nx3_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,       &
     &            NtoO_U, STmp1, STmp2, STmp3, W6(1,IZ1),               &
     &            W6(1,IZ2), W6(1,IZ3) )
!
      end subroutine i_cholesky_w_asdd_3xnn
!
!  ---------------------------------------------------------------------
!
      end module i_cholesky_w_asdd_nn
