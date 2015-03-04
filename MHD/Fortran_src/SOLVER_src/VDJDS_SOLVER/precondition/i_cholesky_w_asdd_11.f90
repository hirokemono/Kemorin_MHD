!
!      module i_cholesky_w_asdd_11
!
!     Written by Kemorin
!
!C
!C
!C***  Incomplete Cholesky
!C***     with additive SCHWARTZ Domain composition
!C***
!C
!      subroutine i_cholesky_w_asdd_1x11                                &
!     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,      &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            D, AL, AU, ALU_L, ALU_U, STmp, SPre, VIni, W2)
!      subroutine i_cholesky_w_asdd_3x11                                &
!     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,      &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            D, AL, AU, ALU_L, ALU_U, STmp1, STmp2, STmp3,        &
!     &            SPre1, SPre2, SPre3, VIni1, VIni2, VIni3, W6)
!
!
      module i_cholesky_w_asdd_11
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
      subroutine i_cholesky_w_asdd_1x11                                 &
     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,       &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, STmp, SPre, VIni, W2)
!
       use djds_matrix_calcs_11
!
       integer(kind = kint), intent(in) :: iterPRE
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
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: SPre(NP), VIni(NP)
!
       real(kind = kreal), intent(inout) :: STmp(NP)
       real(kind = kreal), intent(inout) :: W2(NP,2)
!
!
      call verify_work_4_matvec11(NP)
!
      if (iterPRE .eq. 1) then
        call ordering_1x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &          OtoN_L, STmp, VIni )
      else
!
!   additive SCHWARTZ domain decomposition
!
        call subtruct_matvec_11                                         &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            W2(1,IZ2), VIni, SPre)
!
        call ordering_1x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &          OtoN_L, STmp, W2(1,IZ2) )
      end if
!
      call clear_external_solve_11(N, NP, STmp )
      call clear_vector_solve_11(NP, W2(1,IZ2) )
!
!C
!C== forward substitution
!
      call forward_substitute_1x11(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &     npLX1, STACKmc, NLhyp, INL, IAL, STmp, AL, ALU_L)

      call ordering_1x1_by_l2u(NP, LtoU, W2(1,IZ1), STmp )
!C
!C== backward substitution
!
       call backward_substitute_1x11(N, NP, NU, NPU, PEsmpTOT,          &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           W2(1,IZ1), W2(1,IZ2), AU, ALU_U)
!
       call ordering_1x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &           NtoO_U, STmp, W2(1,IZ1) )
!
      end subroutine i_cholesky_w_asdd_1x11
!
!  ---------------------------------------------------------------------
!
      subroutine i_cholesky_w_asdd_3x11                                 &
     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,       &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, STmp1, STmp2, STmp3,         &
     &            SPre1, SPre2, SPre3, VIni1, VIni2, VIni3, W6)
!
       use djds_matrix_calcs_11
!
       integer(kind = kint), intent(in) :: iterPRE
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
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: SPre1(NP), VIni1(NP)
       real(kind = kreal), intent(in) :: SPre2(NP), VIni2(NP)
       real(kind = kreal), intent(in) :: SPre3(NP), VIni3(NP)
!
       real(kind = kreal), intent(inout) :: STmp1(NP)
       real(kind = kreal), intent(inout) :: STmp2(NP)
       real(kind = kreal), intent(inout) :: STmp3(NP)
       real(kind = kreal), intent(inout) :: W6(NP,6)
!
!
      if (iflag_work_matvec11 .lt. 9) then
        call deallocate_work_4_matvec11
      end if
      call verify_work_4_matvec3x11(NP)
!
      if (iterPRE .eq. 1) then
        call ordering_1x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &          OtoN_L, STmp1, STmp2, STmp3, VIni1, VIni2, VIni3 )
      else
!
!   additive SCHWARTZ domain decomposition
!
        call subtruct_matvec_3x11                                       &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            W6(1,IR1), W6(1,IR2), W6(1,IR3),                      &
     &            VIni1, VIni2, VIni3, SPre1, SPre2, SPre3)
!
        call ordering_1x3_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,  &
     &            STmp1, STmp2, STmp3, W6(1,IR1), W6(1,IR2), W6(1,IR3))
      end if
!
      call clear_external_solve_3x11(N, NP, STmp1, STmp2, STmp3)
      call clear_vector_solve_3x11                                      &
     &            (NP, W6(1,IR1), W6(1,IR2), W6(1,IR3) )
!
!C
!C== forward substitution
!
      call forward_substitute_3x11(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &            npLX1, STACKmc, NLhyp, INL, IAL, STmp1, STmp2, STmp3, &
     &            AL, ALU_L)

      call ordering_1x3_by_l2u(NP, LtoU, W6(1,IZ1), W6(1,IZ2),          &
     &            W6(1,IZ3), STmp1, STmp2, STmp3)
!C
!C== backward substitution
!
       call backward_substitute_3x11(N, NP, NU, NPU, PEsmpTOT,          &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           W6(1,IZ1), W6(1,IZ2), W6(1,IZ3),                       &
     &           W6(1,IR1), W6(1,IR2), W6(1,IR3), AU, ALU_U)
!
      call ordering_1x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &           NtoO_U, STmp1, STmp2, STmp3, W6(1,IZ1),                &
     &           W6(1,IZ2), W6(1,IZ3) )
!
      end subroutine i_cholesky_w_asdd_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module i_cholesky_w_asdd_11
