!
!      module incomplete_cholesky_11
!
!     Written by Kemorin
!
!      subroutine verify_work_4_I_Cholesky11(NP)
!      subroutine verify_work_4_I_Cholesky3x11(NP)
!
!C
!C***  Incomplete Cholesky
!C***
!C
!       subroutine incomplete_cholesky_1x11                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_L, ALU_U, S, V )
!       subroutine incomplete_cholesky_3x11                             &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,   &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,            &
!     &            ALU_L, ALU_U, S1, S2, S3, V1, V2, V3)
!
      module incomplete_cholesky_11
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
       integer(kind = kint) :: iflag_work_I_Cholesky11 = 0
       integer(kind = kint), parameter :: IZR = 2
       integer(kind = kint), parameter :: IZ1 = 1, IZ2 = 2, IZ3 = 3
       integer(kind = kint), parameter :: IR1 = 4, IR2 = 5, IR3 = 6
       real(kind = kreal), allocatable :: W6(:,:)
       private :: W6, iflag_work_I_Cholesky11
       private :: IZR
       private :: IZ1, IZ2, IZ3, IR1, IR2, IR3
!
       private :: allocate_work_4_I_Cholesky11
       private :: allocate_work_4_I_Cholesky3x11
       private :: deallocate_work_4_I_Cholesky11
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_I_Cholesky11(NP)
!
      integer(kind = kint), intent(in) :: NP
!
!
      if (iflag_work_I_Cholesky11.eq.0) then
        call allocate_work_4_I_Cholesky11(NP)
      else if (iflag_work_I_Cholesky11 .lt. (2*NP)) then
        call deallocate_work_4_I_Cholesky11
        call allocate_work_4_I_Cholesky11(NP)
      end if
!
      end subroutine verify_work_4_I_Cholesky11
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_I_Cholesky3x11(NP)
!
      integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_I_Cholesky11.eq.0) then
        call allocate_work_4_I_Cholesky3x11(NP)
      else if (iflag_work_I_Cholesky11.lt.(6*NP)) then
        call deallocate_work_4_I_Cholesky11
        call allocate_work_4_I_Cholesky3x11(NP)
      end if
!
      end subroutine verify_work_4_I_Cholesky3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_I_Cholesky11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W6(NP,2) )
       W6 = 0.0d0
       iflag_work_I_Cholesky11 = 2*NP
!
      end subroutine allocate_work_4_I_Cholesky11
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_I_Cholesky3x11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W6(NP,6) )
       W6 = 0.0d0
       iflag_work_I_Cholesky11 = 6*NP
!
      end subroutine allocate_work_4_I_Cholesky3x11
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_I_Cholesky11
!
       deallocate ( W6 )
       iflag_work_I_Cholesky11 = 0
!
      end subroutine deallocate_work_4_I_Cholesky11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine incomplete_cholesky_1x11                              &
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
       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
!
        call ordering_1x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,          &
     &      OtoN_L, S, V )
        call clear_external_solve_11(N, NP, S )
        call clear_vector_solve_11(NP, W6(1,IZR) )
!C
!C== forward substitution

      call forward_substitute_1x11(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &    npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)

      call ordering_1x1_by_l2u(NP, LtoU, W6(1,IZ1), S )

!C
!C== backward substitution

      call backward_substitute_1x11(N, NP, NU, NPU, PEsmpTOT, NVECT,    &
     &   npUX1, STACKmc, NUhyp, INU, IAU, W6(1,IZ1), W6(1,IZR),         &
     &   AU, ALU_U)

      call ordering_1x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &           NtoO_U, S, W6(1,IZ1) )
!
      end subroutine incomplete_cholesky_1x11
!
!  ---------------------------------------------------------------------
!
       subroutine incomplete_cholesky_3x11                              &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L,    &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, AL, AU,             &
     &            ALU_L, ALU_U, S1, S2, S3, V1, V2, V3)
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
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
!
      call ordering_1x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,            &
     &    OtoN_L, S1, S2, S3, V1, V2, V3 )
      call clear_external_solve_3x11(N, NP, S1, S2, S3 )
      call clear_vector_solve_3x11                                      &
     &    (NP, W6(1,IR1), W6(1,IR2), W6(1,IR3) )
!C
!C== forward substitution

      call forward_substitute_3x11(N, NP, NL, NPL, PEsmpTOT, NVECT,     &
     &    npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL, ALU_L)

      call ordering_1x3_by_l2u                                          &
     &    (NP, LtoU, W6(1,IZ1), W6(1,IZ2), W6(1,IZ3), S1, S2, S3)

!C
!C== backward substitution

      call backward_substitute_3x11(N, NP, NU, NPU, PEsmpTOT, NVECT,    &
     &   npUX1, STACKmc, NUhyp, INU, IAU, W6(1,IZ1), W6(1,IZ2),         &
     &   W6(1,IZ3), W6(1,IR1), W6(1,IR2), W6(1,IR3), AU, ALU_U)

      call ordering_1x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &           NtoO_U, S1, S2, S3, W6(1,IZ1), W6(1,IZ2), W6(1,IZ3) )
!
      end subroutine incomplete_cholesky_3x11
!
!  ---------------------------------------------------------------------
!
      end module incomplete_cholesky_11
