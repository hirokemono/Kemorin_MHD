!
!      module block_ilu_nn
!
!     Written by Kemorin
!
!!      subroutine block_ilu_1xnn                                       &
!!     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU, &
!!     &           ALU_L, S, V, W2)
!!      subroutine block_ilu_3xnn                                       &
!!     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU, &
!!     &           ALU_L, S1, S2, S3, V1, V2, V3, W6)
!!
!!      subroutine block_ilu_nnd                                        &
!!     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU, &
!!     &           ALU_L, S, V, W2)
!!      subroutine block_ilu_3xnnd                                      &
!!     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU, &
!!     &           ALU_L, S1, S2, S3, V1, V2, V3, W6)
!
      module block_ilu_nn
!
      use m_precision
!
      use ordering_by_o2nl_nn
      use ordering_by_l2u_nn
      use ordering_by_new2old_U_nn
      use cal_block_ilu_nn
!
      implicit none
!
       integer(kind = kint), parameter :: IW1 = 1, IW2 = 2
       integer(kind = kint), parameter :: IW3 = 3, IW4 = 4
       integer(kind = kint), parameter :: IW5 = 5, IW6 = 6
       private :: IW1, IW2, IW3, IW4, IW5, IW6
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_1xnn                                         &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, S, V, W2)
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
       real(kind = kreal), intent(inout) :: W2(NB*NP,2)
!
!
        call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &      OtoN_L, W2(1,IW1), V )

        call cal_bl_ilu_1xnn(NP, N, NB, PEsmpTOT, STACKmcG, W2(1,IW1),  &
     &      ALU_L)

        call ordering_nx1_by_l2u(NP, NB, LtoU, W2(1,IW2), W2(1,IW1) )

        call ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,      &
     &      NtoO_U, S, W2(1,IW2) )
!
      end subroutine block_ilu_1xnn
!
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_3xnn                                         &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, S1, S2, S3, V1, V2, V3, W6)
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
       real(kind = kreal), intent(inout) :: W6(NB*NP,6)
!
!
        call ordering_nx3_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &      OtoN_L, W6(1,IW1), W6(1,IW2), W6(1,IW3), V1, V2, V3 )

        call cal_bl_ilu_3xnn(NP, N, NB, PEsmpTOT, STACKmcG,             &
     &      W6(1,IW1), W6(1,IW2), W6(1,IW3), ALU_L)

        call ordering_nx3_by_l2u(NP, NB, LtoU, W6(1,IW4), W6(1,IW5),    &
     &      W6(1,IW6), W6(1,IW1), W6(1,IW2), W6(1,IW3) )

        call ordering_nx3_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,      &
     &      NtoO_U, S1, S2, S3, W6(1,IW4), W6(1,IW5), W6(1,IW6) )
!
      end subroutine block_ilu_3xnn
!
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_nnd                                          &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, S, V, W2)
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
       real(kind = kreal), intent(inout) :: W2(NB*NP,2)
!
!
        call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &      OtoN_L, W2(1,IW1), V )

        call cal_bl_ilu_1xnnd(NP, N, NB, PEsmpTOT, STACKmcG, W2(1,IW1), &
     &      ALU_L)

        call ordering_nx1_by_l2u(NP, NB, LtoU, W2(1,IW2), W2(1,IW1) )

        call ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,      &
     &    NtoO_U, S, W2(1,IW2) )
!
      end subroutine block_ilu_nnd
!
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_3xnnd                                        &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, S1, S2, S3, V1, V2, V3, W6)
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
       real(kind = kreal), intent(inout) :: W6(NB*NP,6)
!
!
        call ordering_nx3_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &      OtoN_L, W6(1,IW1), W6(1,IW2), W6(1,IW3), V1, V2, V3 )

        call cal_bl_ilu_3xnnd(NP, N, NB, PEsmpTOT, STACKmcG,            &
     &      W6(1,IW1), W6(1,IW2), W6(1,IW3), ALU_L)

        call ordering_nx3_by_l2u(NP, NB, LtoU, W6(1,IW4), W6(1,IW5),    &
     &      W6(1,IW6), W6(1,IW1), W6(1,IW2), W6(1,IW3) )

        call ordering_nx3_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,      &
     &      NtoO_U, S1, S2, S3, W6(1,IW4), W6(1,IW5), W6(1,IW6) )
!
      end subroutine block_ilu_3xnnd
!
!  ---------------------------------------------------------------------
!
      end module block_ilu_nn
