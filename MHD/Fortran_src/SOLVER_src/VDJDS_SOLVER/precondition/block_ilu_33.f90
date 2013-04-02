!
!      module block_ilu_33
!
!     Written by Kemorin
!
!      subroutine verify_work_4_bl_ilu33(NP)
!      subroutine verify_work_4_bl_ilu3x33(NP)
!      subroutine allocate_work_4_bl_ilu33(NP)
!      subroutine allocate_work_4_bl_ilu3x33(NP)
!
!      subroutine deallocate_work_4_bl_ilu33
!
!
!      subroutine block_ilu_1x33                                        &
!     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,      &
!     &           ALU_L, S, V)
!      subroutine block_ilu_3x33                                        &
!     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,      &
!     &           ALU_L, S1, S2, S3, V1, V2, V3)
!
!      subroutine block_ilu_33d                                         &
!     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,      &
!     &           ALU_L, S, V)
!      subroutine block_ilu_3x33d                                       &
!     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,      &
!     &           ALU_L, S1, S2, S3, V1, V2, V3)
!
      module block_ilu_33
!
      use m_precision
!
      use ordering_by_o2nl_33
      use ordering_by_l2u_33
      use ordering_by_new2old_U_33
      use cal_block_ilu_33
!
      implicit none
!
       integer(kind = kint) :: iflag_work_bl_ilu33 = 0
       integer(kind = kint), parameter :: IW1 = 1, IW2 = 2
       integer(kind = kint), parameter :: IW3 = 3, IW4 = 4
       integer(kind = kint), parameter :: IW5 = 5, IW6 = 6
       real(kind = kreal), allocatable :: W6(:,:)
       private :: W6, iflag_work_bl_ilu33
       private :: IW1, IW2, IW3, IW4, IW5, IW6
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_bl_ilu33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_bl_ilu33.eq.0) then
        call allocate_work_4_bl_ilu33(NP)
      else if (iflag_work_bl_ilu33.lt. (2*3*NP)) then
        call deallocate_work_4_bl_ilu33
        call allocate_work_4_bl_ilu33(NP)
      end if
!
      end subroutine verify_work_4_bl_ilu33
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_bl_ilu3x33(NP)
!
      integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_bl_ilu33.eq.0) then
        call allocate_work_4_bl_ilu3x33(NP)
      else if (iflag_work_bl_ilu33.lt. (6*3*NP)) then
        call deallocate_work_4_bl_ilu33
        call allocate_work_4_bl_ilu3x33(NP)
      end if
!
      end subroutine verify_work_4_bl_ilu3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_bl_ilu33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W6(3*NP,2) )
       W6 = 0.0d0
       iflag_work_bl_ilu33 = 2*3*NP
!
      end subroutine allocate_work_4_bl_ilu33
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_bl_ilu3x33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W6(3*NP,6) )
       W6 = 0.0d0
       iflag_work_bl_ilu33 = 6*3*NP
!
      end subroutine allocate_work_4_bl_ilu3x33
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_bl_ilu33
!
       deallocate ( W6 )
       iflag_work_bl_ilu33 = 0
!
      end subroutine deallocate_work_4_bl_ilu33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_1x33                                         &
     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,       &
     &           ALU_L, S, V)
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
        call ordering_3x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, W6(1,IW1), V )

        call cal_bl_ilu_1x33(NP, N, PEsmpTOT, STACKmcG,                 &
     &      W6(1,IW1), ALU_L)

        call ordering_3x1_by_l2u(NP, LtoU, W6(1,IW2), W6(1,IW1) )

        call ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,          &
     &    NtoO_U, S, W6(1,IW2) )
!
      end subroutine block_ilu_1x33
!
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_3x33                                         &
     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,       &
     &           ALU_L, S1, S2, S3, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
        call ordering_3x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, W6(1,IW1), W6(1,IW2), W6(1,IW3), V1, V2, V3)

        call cal_bl_ilu_3x33(NP, N, PEsmpTOT, STACKmcG,                 &
     &      W6(1,IW1), W6(1,IW2), W6(1,IW3), ALU_L)

        call ordering_3x3_by_l2u(NP, LtoU, W6(1,IW4), W6(1,IW5),        &
     &      W6(1,IW6), W6(1,IW1), W6(1,IW2), W6(1,IW3) )

        call ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,          &
     &    NtoO_U, S1, S2, S3, W6(1,IW4), W6(1,IW5), W6(1,IW6) )
!
      end subroutine block_ilu_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_33d                                          &
     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,       &
     &           ALU_L, S, V)
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
        call ordering_3x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, W6(1,IW1), V )

        call cal_bl_ilu_1x33d(NP, N, PEsmpTOT, STACKmcG,                &
     &       W6(1,IW1), ALU_L)

        call ordering_3x1_by_l2u(NP, LtoU, W6(1,IW2), W6(1,IW1) )

        call ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,          &
     &    NtoO_U, S, W6(1,IW2) )
!
      end subroutine block_ilu_33d
!
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_3x33d                                        &
     &          (N, NP, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,       &
     &           ALU_L, S1, S2, S3, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
        call ordering_3x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, W6(1,IW1), W6(1,IW2), W6(1,IW3), V1, V2, V3)

        call cal_bl_ilu_3x33d(NP, N, PEsmpTOT, STACKmcG,                &
     &      W6(1,IW1), W6(1,IW2), W6(1,IW3), ALU_L)

        call ordering_3x3_by_l2u(NP, LtoU, W6(1,IW4), W6(1,IW5),        &
     &      W6(1,IW6), W6(1,IW1), W6(1,IW2), W6(1,IW3) )

        call ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,          &
     &    NtoO_U, S1, S2, S3, W6(1,IW4), W6(1,IW5), W6(1,IW6) )
!
      end subroutine block_ilu_3x33d
!
!  ---------------------------------------------------------------------
!
      end module block_ilu_33
