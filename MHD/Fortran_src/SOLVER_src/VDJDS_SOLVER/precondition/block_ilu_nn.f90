!
!      module block_ilu_nn
!
!     Written by Kemorin
!
!      subroutine verify_work_4_bl_ilunn(NP, NB)
!      subroutine verify_work_4_bl_ilu3xnn(NP, NB)
!      subroutine allocate_work_4_bl_ilunn(NP,NB)
!      subroutine allocate_work_4_bl_ilu3xnn(NP,NB)
!      subroutine deallocate_work_4_bl_ilunn
!
!      subroutine block_ilu_1xnn                                        &
!     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,  &
!     &           ALU_L, S, V)
!      subroutine block_ilu_3xnn                                        &
!     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,  &
!     &           ALU_L, S1, S2, S3, V1, V2, V3)
!
!      subroutine block_ilu_nnd                                         &
!     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,  &
!     &           ALU_L, S, V)
!      subroutine block_ilu_3xnnd                                       &
!     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,  &
!     &           ALU_L, S1, S2, S3, V1, V2, V3)
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
       integer(kind = kint) :: iflag_work_bl_ilunn = 0
       integer(kind = kint), parameter :: IW1 = 1, IW2 = 2
       integer(kind = kint), parameter :: IW3 = 3, IW4 = 4
       integer(kind = kint), parameter :: IW5 = 5, IW6 = 6
       real(kind = kreal), allocatable :: W6(:,:)
       private :: W6, iflag_work_bl_ilunn
       private :: IW1, IW2, IW3, IW4, IW5, IW6
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_bl_ilunn(NP, NB)
!
       integer(kind = kint), intent(in) :: NP, NB
!
      if (iflag_work_bl_ilunn.eq.0) then
        call allocate_work_4_bl_ilunn(NP, NB)
      else if (iflag_work_bl_ilunn .lt. (2*NB*NP)) then
        call deallocate_work_4_bl_ilunn
        call allocate_work_4_bl_ilunn(NP, NB)
      end if
!
      end subroutine verify_work_4_bl_ilunn
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_bl_ilu3xnn(NP, NB)
!
      integer(kind = kint), intent(in) :: NP, NB
!
!
      if (iflag_work_bl_ilunn.eq.0) then
        call allocate_work_4_bl_ilu3xnn(NP, NB)
      else if (iflag_work_bl_ilunn .lt. (6*NB*NP)) then
        call deallocate_work_4_bl_ilunn
        call allocate_work_4_bl_ilu3xnn(NP, NB)
      end if
!
      end subroutine verify_work_4_bl_ilu3xnn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_bl_ilunn(NP, NB)
!
       integer(kind = kint), intent(in) :: NP, NB
!
       allocate ( W6(NB*NP,2) )
       W6 = 0.0d0
       iflag_work_bl_ilunn = 2*NB*NP
!
      end subroutine allocate_work_4_bl_ilunn
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_bl_ilu3xnn(NP, NB)
!
       integer(kind = kint), intent(in) :: NP, NB
!
       allocate ( W6(NB*NP,6) )
       W6 = 0.0d0
       iflag_work_bl_ilunn = 6*NB*NP
!
      end subroutine allocate_work_4_bl_ilu3xnn
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_bl_ilunn
!
       deallocate ( W6 )
       iflag_work_bl_ilunn = 0
!
      end subroutine deallocate_work_4_bl_ilunn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_1xnn                                         &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, S, V)
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
!
        call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &      OtoN_L, W6(1,IW1), V )

        call cal_bl_ilu_1xnn(NP, N, NB, PEsmpTOT, STACKmcG, W6(1,IW1),  &
     &      ALU_L)

        call ordering_nx1_by_l2u(NP, NB, LtoU, W6(1,IW2), W6(1,IW1) )

        call ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,      &
     &      NtoO_U, S, W6(1,IW2) )
!
      end subroutine block_ilu_1xnn
!
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_3xnn                                         &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, S1, S2, S3, V1, V2, V3)
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
     &           ALU_L, S, V)
!
       integer(kind = kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
!
        call ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,      &
     &      OtoN_L, W6(1,IW1), V )

        call cal_bl_ilu_1xnnd(NP, N, NB, PEsmpTOT, STACKmcG, W6(1,IW1), &
     &      ALU_L)

        call ordering_nx1_by_l2u(NP, NB, LtoU, W6(1,IW2), W6(1,IW1) )

        call ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT, STACKmcG,      &
     &    NtoO_U, S, W6(1,IW2) )
!
      end subroutine block_ilu_nnd
!
!  ---------------------------------------------------------------------
!
      subroutine block_ilu_3xnnd                                        &
     &          (N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, NtoO_U, LtoU,   &
     &           ALU_L, S1, S2, S3, V1, V2, V3)
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
