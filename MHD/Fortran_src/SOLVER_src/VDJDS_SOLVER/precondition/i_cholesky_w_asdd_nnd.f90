!
!      module i_cholesky_w_asdd_nnd
!
!     Written by Kemorin
!
!      subroutine allocate_work_4_IC_asddnnd(NP,NB)
!      subroutine allocate_work_4_IC_asdd3xnnd(NP,NB)
!      subroutine deallocate_work_4_IC_asddnnd
!C
!C***  Incomplete Cholesky
!C***     with additive SCHWARTZ Domain composition
!C***
!C
!      subroutine i_cholesky_w_asdd_1xnnd                               &
!     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,  &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            D, AL, AU, ALU_L, ALU_U, STmp, SPre, VIni)
!
!      subroutine i_cholesky_w_asdd_3xnnd                               &
!     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,  &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            D, AL, AU, ALU_L, ALU_U, STmp1, STmp2, STmp3,        &
!     &            SPre1, SPre2, SPre3, VIni1, VIni2, VIni3)
!
      module i_cholesky_w_asdd_nnd
!
      use m_precision
!
      use ordering_by_o2nl_nn
      use ordering_by_l2u_nn
      use ordering_by_new2old_U_nn
      use vector_calc_solver_nn
      use djds_matrix_calcs_nnd
      use forward_substitute_nnd
      use backward_substitute_nnd
!
      implicit none
!
       integer(kind = kint) :: iflag_work_IC_asddnnd = 0
       integer(kind = kint), parameter :: IZR = 2
       integer(kind = kint), parameter :: IZ1 = 1, IZ2 = 2, IZ3 = 3
       integer(kind = kint), parameter :: IR1 = 4, IR2 = 5, IR3 = 6
       real(kind = kreal), allocatable :: W6(:,:)
       private :: W6, iflag_work_IC_asddnnd
       private :: IZR
       private :: IZ1, IZ2, IZ3, IR1, IR2, IR3
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_IC_asddnnd(NP,NB)
!
       integer(kind = kint), intent(in) :: NP, NB
!
       allocate ( W6(NB*NP,2) )
       W6 = 0.0d0
       iflag_work_IC_asddnnd = NB*2
!
      end subroutine allocate_work_4_IC_asddnnd
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_IC_asdd3xnnd(NP,NB)
!
       integer(kind = kint), intent(in) :: NP, NB
!
       allocate ( W6(NB*NP,6) )
       W6 = 0.0d0
       iflag_work_IC_asddnnd = NB*6
!
      end subroutine allocate_work_4_IC_asdd3xnnd
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_IC_asddnnd
!
       deallocate ( W6 )
       iflag_work_IC_asddnnd = 0
!
      end subroutine deallocate_work_4_IC_asddnnd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine i_cholesky_w_asdd_1xnnd                                &
     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,   &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, STmp, SPre, VIni)
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
!
!
      if (iflag_work_matvecnnd .lt. 3*NB ) then
        call deallocate_work_4_matvecnnd
      end if
      if (iflag_work_matvecnnd .eq. 0 ) then
        call allocate_work_4_matvecnnd(NP, NB)
      end if
!
      if (iterPRE .eq. 1) then
        call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &          OtoN_L, STmp, VIni )
      else
!
!   additive SCHWARTZ domain decomposition
!
        call subtruct_matvec_nnd                                        &
     &           (NP, NB, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,  &
     &            W6(1,IZR), VIni, SPre)
!
        call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &          OtoN_L, STmp, W6(1,IZR) )
      end if
!
      call clear_external_solve_nn(N, NP, NB, STmp )
      call clear_vector_solve_nn(NP, NB, W6(1,IZR) )
!
!C
!C== forward substitution
!
      call forward_substitute_1xnnd(N, NP, NB, NL, NPL, PEsmpTOT,       &
     &    NVECT, npLX1, STACKmc, NLhyp, INL, IAL, STmp, AL, ALU_L)

      call ordering_nx1_by_l2u(NP, NB, LtoU, W6(1,IZ1), STmp )
!C
!C== backward substitution
!
       call backward_substitute_1xnnd(N, NP, NB, NU, NPU, PEsmpTOT,     &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           W6(1,IZ1), W6(1,IZR), AU, ALU_U)
!
       call ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,       &
     &           NtoO_U, STmp, W6(1,IZ1) )
!
      end subroutine i_cholesky_w_asdd_1xnnd
!
!  ---------------------------------------------------------------------
!
      subroutine i_cholesky_w_asdd_3xnnd                                &
     &           (iterPRE, N, NP, NB, NL, NU, NPL, NPU, npLX1, npUX1,   &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, STmp1, STmp2, STmp3,         &
     &            SPre1, SPre2, SPre3, VIni1, VIni2, VIni3)
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
!
!
      call verify_work_4_matvec3xnnd
!
      if (iterPRE .eq. 1) then
        call ordering_nx3_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &          OtoN_L, STmp1, STmp2, STmp3, VIni1, VIni2, VIni3)
      else
!
!   additive SCHWARTZ domain decomposition
!
        call subtruct_matvec_3xnnd                                      &
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
      call forward_substitute_3xnnd(N, NP, NB, NL, NPL, PEsmpTOT,       &
     &            NVECT, npLX1, STACKmc, NLhyp, INL, IAL,               &
     &            STmp1, STmp2, STmp3, AL, ALU_L)

      call ordering_nx3_by_l2u(NP, NB, LtoU, W6(1,IZ1), W6(1,IZ2),      &
     &            W6(1,IZ3), STmp1, STmp2, STmp3 )
!C
!C== backward substitution
!
       call backward_substitute_3xnnd(N, NP, NB, NU, NPU, PEsmpTOT,     &
     &            NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
     &            W6(1,IZ1), W6(1,IZ2), W6(1,IZ3), W6(1,IR1),           &
     &            W6(1,IR2), W6(1,IR3), AU, ALU_U)
!
       call ordering_nx3_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,       &
     &            NtoO_U, STmp1, STmp2, STmp3, W6(1,IZ1),               &
     &            W6(1,IZ2), W6(1,IZ3) )
!
      end subroutine i_cholesky_w_asdd_3xnnd
!
!  ---------------------------------------------------------------------
!
      end module i_cholesky_w_asdd_nnd
