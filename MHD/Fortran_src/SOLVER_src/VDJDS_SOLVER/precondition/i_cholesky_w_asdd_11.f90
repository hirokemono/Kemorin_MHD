!
!      module i_cholesky_w_asdd_11
!
!     Written by Kemorin
!
!      subroutine verify_work_4_IC_asdd11(NP)
!      subroutine verify_work_4_IC_asdd3x11(NP)
!
!      subroutine allocate_work_4_IC_asdd11(NP)
!      subroutine allocate_work_4_IC_asdd3x11(NP)
!      subroutine deallocate_work_4_IC_asdd11
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
!     &            D, AL, AU, ALU_L, ALU_U, STmp, SPre, VIni)
!      subroutine i_cholesky_w_asdd_3x11                                &
!     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,      &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            D, AL, AU, ALU_L, ALU_U, STmp1, STmp2, STmp3,        &
!     &            SPre1, SPre2, SPre3, VIni1, VIni2, VIni3)
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
       integer(kind = kint) :: iflag_work_IC_asdd11 = 0
       integer(kind = kint), parameter :: IZR = 2
       integer(kind = kint), parameter :: IZ1 = 1, IZ2 = 2, IZ3 = 3
       integer(kind = kint), parameter :: IR1 = 4, IR2 = 5, IR3 = 6
       real(kind = kreal), allocatable :: W6(:,:)
       private :: W6, iflag_work_IC_asdd11
       private :: IZR
       private :: IZ1, IZ2, IZ3, IR1, IR2, IR3
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_IC_asdd11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_IC_asdd11.eq.0) then
        call allocate_work_4_IC_asdd11(NP)
      else if (iflag_work_IC_asdd11 .lt. (2*NP)) then
        call deallocate_work_4_IC_asdd11
        call allocate_work_4_IC_asdd11(NP)
      end if
!
      end subroutine verify_work_4_IC_asdd11
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_IC_asdd3x11(NP)
!
      integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_IC_asdd11.eq.0) then
        call allocate_work_4_IC_asdd3x11(NP)
      else if (iflag_work_IC_asdd11 .lt. (6*NP)) then
        call deallocate_work_4_IC_asdd11
        call allocate_work_4_IC_asdd3x11(NP)
      end if
!
      end subroutine verify_work_4_IC_asdd3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_IC_asdd11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W6(NP,2) )
       W6 = 0.0d0
       iflag_work_IC_asdd11 = 2*NP
!
      end subroutine allocate_work_4_IC_asdd11
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_IC_asdd3x11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W6(NP,6) )
       W6 = 0.0d0
       iflag_work_IC_asdd11 = 6*NP
!
      end subroutine allocate_work_4_IC_asdd3x11
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_IC_asdd11
!
       deallocate ( W6 )
       iflag_work_IC_asdd11 = 0
!
      end subroutine deallocate_work_4_IC_asdd11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine i_cholesky_w_asdd_1x11                                 &
     &           (iterPRE, N, NP, NL, NU, NPL, NPU, npLX1, npUX1,       &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            D, AL, AU, ALU_L, ALU_U, STmp, SPre, VIni)
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
     &            W6(1,IZR), VIni, SPre)
!
        call ordering_1x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &          OtoN_L, STmp, W6(1,IZR) )
      end if
!
      call clear_external_solve_11(N, NP, STmp )
      call clear_vector_solve_11(NP, W6(1,IZR) )
!
!C
!C== forward substitution
!
      call forward_substitute_1x11(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &     npLX1, STACKmc, NLhyp, INL, IAL, STmp, AL, ALU_L)

      call ordering_1x1_by_l2u(NP, LtoU, W6(1,IZ1), STmp )
!C
!C== backward substitution
!
       call backward_substitute_1x11(N, NP, NU, NPU, PEsmpTOT,          &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           W6(1,IZ1), W6(1,IZR), AU, ALU_U)
!
       call ordering_1x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &           NtoO_U, STmp, W6(1,IZ1) )
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
     &            SPre1, SPre2, SPre3, VIni1, VIni2, VIni3)
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
