!
!      module gauss_zeidel_11
!
!     Written by Kemorin
!
!!       subroutine gauss_zeidel_forward_11                             &
!!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,       &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B, S, W3)
!!       subroutine gauss_zeidel_backward_11                            &
!!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,       &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B, S, W3)
!!
!!       subroutine gauss_zeidel_forward_3x11                           &
!!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,       &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
!!       subroutine gauss_zeidel_backward_3x11                          &
!!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,       &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
!!
!!       subroutine gauss_zeidel_each_plane_11                          &
!!     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,   &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B, S, W3)
!!       subroutine gauss_zeidel_each_plane_3x11                        &
!!     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,   &
!!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,  &
!!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,   &
!!     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
!
      module gauss_zeidel_11
!
      use m_precision
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
       subroutine gauss_zeidel_forward_11                               &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
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
       real(kind = kreal), intent(inout) :: W3(NP,3)
!
       integer(kind = kint) :: iv
!
!
      do iv= 1, NVECT
        call gauss_zeidel_each_plane_11                                 &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
      end do
!
       end subroutine gauss_zeidel_forward_11
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_backward_11                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
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
       real(kind = kreal), intent(inout) :: W3(NP,3)
!
       integer(kind = kint) :: iv
!
!
      do iv= NVECT, 1, -1
        call gauss_zeidel_each_plane_11                                 &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
      end do
!
       end subroutine gauss_zeidel_backward_11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_forward_3x11                             &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
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
       real(kind = kreal), intent(inout) :: W9(NP,9)
!
       integer(kind = kint) :: iv
!
!
      do iv= 1, NVECT
        call gauss_zeidel_each_plane_3x11                               &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
      end do
!
       end subroutine gauss_zeidel_forward_3x11
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_backward_3x11                            &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
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
       real(kind = kreal), intent(inout) :: W9(NP,9)
!
       integer(kind = kint) :: iv
!
!
      do iv= NVECT, 1, -1
        call gauss_zeidel_each_plane_3x11                               &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
      end do
!
       end subroutine gauss_zeidel_backward_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_each_plane_11                            &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S, W3)
!
      use ordering_by_o2nl_11
      use ordering_by_l2u_o2nu_11
      use ordering_by_new2old_U_11
      use gauss_zeidel_hp_11
!
       integer(kind = kint), intent(in) :: iv
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
       real(kind = kreal), intent(inout) :: W3(NP,3)
!
!
        call ordering_1x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, W3(1,IZ2), W3(1,IZ1), S, B)
        call gauss_zeidel_hp_11_l(iv, NP, NL, NPL, PEsmpTOT,            &
     &      NVECT, npLX1, STACKmc, NLhyp, INL, IAL, W3(1,IZ1),          &
     &      AL, W3(1,IZ2))
!
       call ordering_1x1_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W3(1,IZ3), W3(1,IZ2), S, W3(1,IZ1) )
!
       call gauss_zeidel_hp_11_u(iv, NP, N, NU, NPU, PEsmpTOT,          &
     &      NVECT, npUX1, STACKmc, NUhyp, INU, IAU, W3(1,IZ2),          &
     &      AU, ALU_U, W3(1,IZ3) )
!
        call ordering_1x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,          &
     &      NtoO_U, S, W3(1,IZ2) )
!
       end subroutine gauss_zeidel_each_plane_11
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_each_plane_3x11                          &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3, W9)
!
      use ordering_by_o2nl_11
      use ordering_by_l2u_o2nu_11
      use ordering_by_new2old_U_11
      use gauss_zeidel_hp_11
!
       integer(kind = kint), intent(in) :: iv
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
       real(kind = kreal), intent(in) :: B1(NP), B2(NP), B3(NP)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
       real(kind = kreal), intent(inout) :: W9(NP,9)
!
!
       call ordering_1x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W9(1,IZ4), W9(1,IZ5), W9(1,IZ6), W9(1,IZ1),                  &
     &     W9(1,IZ2), W9(1,IZ3), S1, S2, S3, B1, B2, B3)
       call gauss_zeidel_hp_3x11_l(iv, NP, NL, NPL, PEsmpTOT,           &
     &      NVECT, npLX1, STACKmc, NLhyp, INL, IAL,                     &
     &      W9(1,IZ1), W9(1,IZ2), W9(1,IZ3), AL,                        &
     &      W9(1,IZ4), W9(1,IZ5), W9(1,IZ6) )
!
       call ordering_1x3_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W9(1,IZ7), W9(1,IZ8), W9(1,IZ9),                             &
     &     W9(1,IZ4), W9(1,IZ5), W9(1,IZ6), S1, S2, S3,                 &
     &     W9(1,IZ1), W9(1,IZ2), W9(1,IZ3) )
       call gauss_zeidel_hp_3x11_u(iv, NP, N, NU, NPU, PEsmpTOT,        &
     &      NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                     &
     &      W9(1,IZ4), W9(1,IZ5), W9(1,IZ6), AU, ALU_U,                 &
     &      W9(1,IZ7), W9(1,IZ8), W9(1,IZ9) )
!
       call ordering_1x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &      NtoO_U, S1, S2, S3, W9(1,IZ4), W9(1,IZ5), W9(1,IZ6) )
!
       end subroutine gauss_zeidel_each_plane_3x11
!
!  ---------------------------------------------------------------------
!
      end module gauss_zeidel_11
