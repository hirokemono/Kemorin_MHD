!
!      module incomplete_cholesky_33
!
!     Written by Kemorin
!
!      subroutine verify_work_4_I_Cholesky33(NP)
!      subroutine verify_wk_I_Cholesky3x33(NP)
!C
!C***  Incomplete Cholesky
!C***
!C
!       subroutine incomplete_cholesky_1x33                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_L, ALU_U, S, V )
!       subroutine incomplete_cholesky_3x33                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_L, ALU_U, S, V )
!
      module incomplete_cholesky_33
!
      use m_precision
!
      use ordering_by_o2nl_33
      use ordering_by_l2u_33
      use ordering_by_new2old_U_33
      use vector_calc_solver_33
      use forward_substitute_33
      use backward_substitute_33
!
      implicit none
!
       integer(kind = kint) :: iflag_work_I_Cholesky33 = 0
       integer(kind = kint), parameter :: IZR = 2
       integer(kind = kint), parameter :: IZ1 = 1, IZ2 = 2, IZ3 = 3
       integer(kind = kint), parameter :: IR1 = 4, IR2 = 5, IR3 = 6
       real(kind = kreal), allocatable :: W6(:,:)
       private :: W6, iflag_work_I_Cholesky33
       private :: IZR
       private :: IZ1, IZ2, IZ3, IR1, IR2, IR3
!
       private :: deallocate_work_4_I_Cholesky33
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_I_Cholesky33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_I_Cholesky33.eq.0) then
        call allocate_work_4_I_Cholesky33(NP)
      else if (iflag_work_I_Cholesky33 .lt. (2*3*NP)) then
        call deallocate_work_4_I_Cholesky33
        call allocate_work_4_I_Cholesky33(NP)
      end if
!
      end subroutine verify_work_4_I_Cholesky33
!
!  ---------------------------------------------------------------------
!
      subroutine verify_wk_I_Cholesky3x33(NP)
!
      integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_I_Cholesky33.eq.0) then
        call allocate_wk_I_Cholesky3x33(NP)
      else if (iflag_work_I_Cholesky33 .lt. (6*3*NP)) then
        call deallocate_work_4_I_Cholesky33
        call allocate_wk_I_Cholesky3x33(NP)
      end if
!
      end subroutine verify_wk_I_Cholesky3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_I_Cholesky33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W6(3*NP,2) )
       W6 = 0.0d0
       iflag_work_I_Cholesky33 = 2*3*NP
!
      end subroutine allocate_work_4_I_Cholesky33
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_wk_I_Cholesky3x33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W6(3*NP,6) )
       W6 = 0.0d0
       iflag_work_I_Cholesky33 = 6*3*NP
!
      end subroutine allocate_wk_I_Cholesky3x33
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_I_Cholesky33
!
       deallocate ( W6 )
       iflag_work_I_Cholesky33 = 0
!
      end subroutine deallocate_work_4_I_Cholesky33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine incomplete_cholesky_1x33                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, S, V )
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
        call ordering_3x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, S, V )
        call clear_external_solve_33(N, NP, S )
        call clear_vector_solve_33(NP, W6(1,IZR) )
!C
!C== forward substitution

      call forward_substitute_1x33(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &    npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)

      call ordering_3x1_by_l2u(NP, LtoU, W6(1,IZ1), S )

!C
!C== backward substitution

      call backward_substitute_1x33(N, NP, NU, NPU, PEsmpTOT, NVECT,    &
     &   npUX1, STACKmc, NUhyp, INU, IAU, W6(1,IZ1), W6(1,IZR),         &
     &   AU, ALU_U)

      call ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &           NtoO_U, S, W6(1,IZ1) )
!
      end subroutine incomplete_cholesky_1x33
!
!  ---------------------------------------------------------------------
!
       subroutine incomplete_cholesky_3x33                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, S1, S2, S3, V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP)
       real(kind = kreal), intent(in) :: V3(3*NP)
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
        call ordering_3x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, S1, S2, S3, V1, V2, V3 )
        call clear_external_solve_3x33(N, NP, S1, S2, S3)
        call clear_vector_solve_3x33                                    &
     &      (NP, W6(1,IR1), W6(1,IR2), W6(1,IR3) )
!C
!C== forward substitution

      call forward_substitute_3x33(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &    npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL, ALU_L)

      call ordering_3x3_by_l2u                                          &
     &    (NP, LtoU, W6(1,IZ1), W6(1,IZ2), W6(1,IZ3), S1, S2, S3 )

!C
!C== backward substitution

      call backward_substitute_3x33(N, NP, NU, NPU, PEsmpTOT, NVECT,    &
     &    npUX1, STACKmc, NUhyp, INU, IAU, W6(1,IZ1), W6(1,IZ2),        &
     &    W6(1,IZ3), W6(1,IR1), W6(1,IR2), W6(1,IR3), AU, ALU_U)

      call ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &    NtoO_U, S1, S2, S3, W6(1,IZ1), W6(1,IZ2), W6(1,IZ3) )
!
      end subroutine incomplete_cholesky_3x33
!
!  ---------------------------------------------------------------------
!
      end module incomplete_cholesky_33
