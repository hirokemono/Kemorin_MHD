!jacobi_precondition_11.f90
!      module jacobi_precondition_11
!
!     Written by Kemorin
!
!      subroutine verify_work_4_jacobi_prec11(NP)
!      subroutine allocate_work_4_jacobi_prec11(NP)
!      subroutine deallocate_work_4_jacobi_prec11
!C
!C***  Gauss method
!C***
!C
!      subroutine jacobi_forward_11                                     &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1,               &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            AL, AU, ALU_L, S, V)
!      subroutine jacobi_forward_3x11                                   &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1,               &
!     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            AL, AU, ALU_U, S1, S2, S3, V1, V2, V3)
!
!       ALU_U = 1 / Diag (Ordered by multicolor)
!
      module jacobi_precondition_11
!
      use m_precision
!
      use ordering_by_o2nl_11
      use ordering_by_l2u_o2nu_11
      use ordering_by_new2old_U_11
      use cal_4_lower_11
      use cal_4_upper_11
      use diagonal_scaling_11
!
      implicit none
!
       integer(kind = kint) :: iflag_work_jacobi_prec11 = 0
       integer(kind = kint), parameter :: IZ1 = 1, IZ2 = 2, IZ3 = 3
       integer(kind = kint), parameter :: IZ4 = 4, IZ5 = 5, IZ6 = 6
       integer(kind = kint), parameter :: IZ7 = 7, IZ8 = 8, IZ9 = 9
       real(kind = kreal), allocatable :: W9(:,:)
       private :: W9, iflag_work_jacobi_prec11
       private :: IZ1, IZ2, IZ3, IZ4, IZ5, IZ6, IZ7, IZ8, IZ9
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_jacobi_prec11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_jacobi_prec11.eq.0) then
        call allocate_work_4_jacobi_prec11(NP)
      else if (iflag_work_jacobi_prec11 .eq. (3*NP)) then
        call deallocate_work_4_jacobi_prec11
        call allocate_work_4_jacobi_prec11(NP)
      end if
!
      end subroutine verify_work_4_jacobi_prec11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_jacobi_prec11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W9(NP,3) )
       W9 = 0.0d0
       iflag_work_jacobi_prec11 = 3*NP
!
      end subroutine allocate_work_4_jacobi_prec11
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_jacobi_prec11
!
       deallocate ( W9 )
       iflag_work_jacobi_prec11 = 0
!
      end subroutine deallocate_work_4_jacobi_prec11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine jacobi_forward_11                                      &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1,                &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            AL, AU, ALU_L, S, V)
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
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: V(NP)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
!
       call ordering_1x2_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W9(1,IZ2), W9(1,IZ1), V, S)
!
       call subtract_lower_11(NP, NL, NPL, PEsmpTOT, NVECT, npLX1,      &
     &     STACKmc, NLhyp, INL, IAL, W9(1,IZ2), AL, W9(1,IZ1) )
!
       call ordering_1x1_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W9(1,IZ1), W9(1,IZ3), S, W9(1,IZ2) )
!
       call subtract_upper_11(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,      &
     &     STACKmc, NUhyp, INU, IAU, W9(1,IZ3), AU, W9(1,IZ1) )
!
       call ordering_1x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &     NtoO_U, W9(1,IZ1), W9(1,IZ3) )
!
       call diag_scaling_1x11(NP, N, PEsmpTOT, STACKmcG,                &
     &     S, W9(1,IZ1), ALU_L)
!
      end subroutine jacobi_forward_11
!
!  ---------------------------------------------------------------------
!
      subroutine jacobi_forward_3x11                                    &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1,                &
     &            NVECT, PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            AL, AU, ALU_U, S1, S2, S3, V1, V2, V3)
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
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
!
       call ordering_1x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W9(1,IZ4), W9(1,IZ5), W9(1,IZ6), W9(1,IZ1),                  &
     &     W9(1,IZ2), W9(1,IZ3), S1, S2, S3, V1, V2, V3)
!
       call subtract_lower_3x11(NP, NL, NPL, PEsmpTOT, NVECT, npLX1,    &
     &     STACKmc, NLhyp, INL, IAL, W9(1,IZ4), W9(1,IZ5), W9(1,IZ6),   &
     &     AL, W9(1,IZ1), W9(1,IZ2), W9(1,IZ3) )
!
       call ordering_1x3_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W9(1,IZ1), W9(1,IZ2), W9(1,IZ3),                             &
     &     W9(1,IZ7), W9(1,IZ8), W9(1,IZ9), S1, S2, S3,                 &
     &     W9(1,IZ4), W9(1,IZ5), W9(1,IZ6) )
!
       call subtract_upper_3x11(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,    &
     &     STACKmc, NUhyp, INU, IAU, W9(1,IZ7), W9(1,IZ8), W9(1,IZ9),   &
     &     AU, W9(1,IZ1), W9(1,IZ2), W9(1,IZ3) )
!
       call ordering_1x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &     NtoO_U, W9(1,IZ1), W9(1,IZ2), W9(1,IZ3),                     &
     &     W9(1,IZ7), W9(1,IZ8), W9(1,IZ9) )
!
       call diag_scaling_3x11(NP, N, PEsmpTOT, STACKmcG, S1, S2, S3,    &
     &     W9(1,IZ1), W9(1,IZ2), W9(1,IZ3), ALU_U)
!
      end subroutine jacobi_forward_3x11
!
!  ---------------------------------------------------------------------
!
      end module jacobi_precondition_11
