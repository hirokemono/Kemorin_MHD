!
!      module gauss_zeidel_33
!
!     Written by Kemorin
!
!      subroutine verify_work_GaussZeidel_33(NP)
!      subroutine allocate_work_GaussZeidel_33(NP)
!      subroutine deallocate_work_GaussZeidel_33
!
!       subroutine gauss_zeidel_forward_33                              &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B, S)
!       subroutine gauss_zeidel_backward_33                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B, S)
!
!       subroutine gauss_zeidel_forward_3x33                            &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B1, B2, B3, S1, S2, S3)
!       subroutine gauss_zeidel_backward_3x33                           &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
!       subroutine gauss_zeidel_each_plane_33                           &
!     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,    &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B, S)
!       subroutine gauss_zeidel_each_plane_3x33                         &
!     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,    &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,    &
!     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
      module gauss_zeidel_33
!
      use m_precision
!
      implicit none
!
       integer(kind = kint) :: iflag_work_GaussZeidel_33 = 0
       integer(kind = kint), parameter :: IZR = 2
       integer(kind = kint), parameter :: IZ1 = 1, IZ2 = 2, IZ3 = 3
       integer(kind = kint), parameter :: IZ4 = 4, IZ5 = 5, IZ6 = 6
       integer(kind = kint), parameter :: IZ7 = 7, IZ8 = 8, IZ9 = 9
       real(kind = kreal), allocatable :: W9(:,:)
       private :: W9, iflag_work_GaussZeidel_33
       private :: IZR
       private :: IZ1, IZ2, IZ3, IZ4, IZ5, IZ6, IZ7, IZ8, IZ9
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_GaussZeidel_33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       if(iflag_work_GaussZeidel_33 .eq. 0) then
         call allocate_work_GaussZeidel_33(NP)
       else if(iflag_work_GaussZeidel_33 .lt. (3*3*NP)) then
         call deallocate_work_GaussZeidel_33
         call allocate_work_GaussZeidel_33(NP)
       end if
!
      end subroutine verify_work_GaussZeidel_33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_GaussZeidel_33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W9(3*NP,3) )
       W9 = 0.0d0
       iflag_work_GaussZeidel_33 = 3*3*NP
!
      end subroutine allocate_work_GaussZeidel_33
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_GaussZeidel_33
!
       deallocate ( W9 )
       iflag_work_GaussZeidel_33 = 0
!
      end subroutine deallocate_work_GaussZeidel_33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_forward_33                               &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B(3*NP)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer(kind = kint) :: iv
!
!
      do iv= 1, NVECT
        call gauss_zeidel_each_plane_33                                 &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
      end do
!
       end subroutine gauss_zeidel_forward_33
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_backward_33                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B(3*NP)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer(kind = kint) :: iv
!
!
      do iv= NVECT, 1, -1
        call gauss_zeidel_each_plane_33                                 &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
      end do
!
       end subroutine gauss_zeidel_backward_33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_forward_3x33                             &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), B2(3*NP), B3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer(kind = kint) :: iv
!
!
      do iv= 1, NVECT
        call gauss_zeidel_each_plane_3x33                               &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
      end do
!
       end subroutine gauss_zeidel_forward_3x33
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_backward_3x33                            &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), B2(3*NP), B3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer(kind = kint) :: iv
!
!
      do iv= NVECT, 1, -1
        call gauss_zeidel_each_plane_3x33                               &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
      end do
!
       end subroutine gauss_zeidel_backward_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_each_plane_33                            &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B, S)
!
      use ordering_by_o2nl_33
      use ordering_by_l2u_o2nu_33
      use ordering_by_new2old_U_33
      use gauss_zeidel_hp_33
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
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B(3*NP)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
        call ordering_3x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, W9(1,IZ2), W9(1,IZ1), S, B)
        call gauss_zeidel_hp_33_l(iv, NP, NL, NPL, PEsmpTOT,            &
     &      NVECT, npLX1, STACKmc, NLhyp, INL, IAL, W9(1,IZ1),          &
     &      AL, W9(1,IZ2))
!
       call ordering_3x1_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W9(1,IZ3), W9(1,IZ2), S, W9(1,IZ1) )
       call gauss_zeidel_hp_33_u(iv, NP, N, NU, NPU, PEsmpTOT,          &
     &      NVECT, npUX1, STACKmc, NUhyp, INU, IAU, W9(1,IZ2),          &
     &      AU, ALU_U, W9(1,IZ3) )
!
        call ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,          &
     &      NtoO_U, S, W9(1,IZ2) )
!
       end subroutine gauss_zeidel_each_plane_33
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_each_plane_3x33                          &
     &           (iv, N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,     &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,     &
     &            ALU_U, B1, B2, B3, S1, S2, S3)
!
      use ordering_by_o2nl_33
      use ordering_by_l2u_o2nu_33
      use ordering_by_new2old_U_33
      use gauss_zeidel_hp_33
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NU)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), B2(3*NP), B3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
       call ordering_3x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W9(1,IZ4), W9(1,IZ5), W9(1,IZ6), W9(1,IZ1),                  &
     &     W9(1,IZ2), W9(1,IZ3), S1, S2, S3, B1, B2, B3)
       call gauss_zeidel_hp_3x33_l(iv, NP, NL, NPL, PEsmpTOT,           &
     &      NVECT, npLX1, STACKmc, NLhyp, INL, IAL,                     &
     &      W9(1,IZ1), W9(1,IZ2), W9(1,IZ3), AL,                        &
     &      W9(1,IZ4), W9(1,IZ5), W9(1,IZ6) )
!
       call ordering_3x3_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W9(1,IZ7), W9(1,IZ8), W9(1,IZ9),                             &
     &     W9(1,IZ4), W9(1,IZ5), W9(1,IZ6), S1, S2, S3,                 &
     &     W9(1,IZ1), W9(1,IZ2), W9(1,IZ3) )
       call gauss_zeidel_hp_3x33_u(iv, NP, N, NU, NPU, PEsmpTOT,        &
     &      NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                     &
     &      W9(1,IZ4), W9(1,IZ5), W9(1,IZ6), AU, ALU_U,                 &
     &      W9(1,IZ7), W9(1,IZ8), W9(1,IZ9) )
!
       call ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &      NtoO_U, S1, S2, S3, W9(1,IZ4), W9(1,IZ5), W9(1,IZ6) )
!
       end subroutine gauss_zeidel_each_plane_3x33
!
!  ---------------------------------------------------------------------
!
      end module gauss_zeidel_33
