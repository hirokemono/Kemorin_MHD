!
!      module djds_matrix_calcs_33d
!
!     Written by Kemorin
!
!      subroutine verify_work_4_matvec33d(NP, NB)
!      subroutine verify_work_4_matvec3x33d(NP, NB)
!
!   Ordering by Multicolor
!       subroutine change_order_2_solve_bx3d(NP, PEsmpTOT, STACKmcG,    &
!     &           NtoO, B, X)
!   Returning from Multicolor
!       subroutine back_2_original_order_bx3d(NP, NtoO, B, X)
!
!       subroutine cal_matvec_33d                                       &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S, X )
!       subroutine cal_matvec_3x33d                                     &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S1, S2, S3, X1, X2, X3 )
!C +-------------+
!C | {S}= [A]{X} |
!C +-------------+
!
!       subroutine subtruct_matvec_33d                                  &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S, B, X )
!       subroutine subtruct_matvec_3x33d                                &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S1, S2, S3, B1, B2, B3, X1, X2, X3 )
!C +-------------- ----+
!C | {S}= {B} - [A]{X} |
!C +-------------------+
!
      module djds_matrix_calcs_33d
!
      use m_precision
!
      use cal_4_diagonal_33d
      use cal_4_lower_33d
      use cal_4_upper_33d
      use ordering_by_o2nl_33
      use ordering_by_l2u_o2nu_33
      use ordering_by_new2old_U_33
      use order_vect_4_solver_33
!
      implicit none
!
       integer(kind = kint) :: iflag_work_matvec33d = 0
       integer(kind = kint), parameter :: IWK1 = 1, IWK2 = 2, IWK3 = 3
       integer(kind = kint), parameter :: IWK11= 1, IWK12= 4, IWK13= 7
       integer(kind = kint), parameter :: IWK21= 2, IWK22= 5, IWK23= 8
       integer(kind = kint), parameter :: IWK31= 3, IWK32= 6, IWK33= 9
       real(kind = kreal), allocatable :: W3(:,:)
       private :: W3
       private :: IWK1, IWK2, IWK3
       private :: IWK11, IWK12, IWK13
       private :: IWK21, IWK22, IWK23
       private :: IWK31, IWK32, IWK33
!
       private :: allocate_work_4_matvec33d
       private :: allocate_work_4_matvec3x33d
       private :: deallocate_work_4_matvec33d
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec33d(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_matvec33d .eq. 0) then
        call allocate_work_4_matvec33d(NP)
      else if (iflag_work_matvec33d .lt. (3*3*NP) ) then
        call deallocate_work_4_matvec33d
        call allocate_work_4_matvec33d(NP)
      end if
!
      end subroutine verify_work_4_matvec33d
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec3x33d(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_matvec33d .eq. 0) then
        call allocate_work_4_matvec3x33d(NP)
      else if (iflag_work_matvec33d .lt. (9*3*NP) ) then
        call deallocate_work_4_matvec33d
        call allocate_work_4_matvec3x33d(NP)
      end if
!
      end subroutine verify_work_4_matvec3x33d
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_matvec33d(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W3(3*NP,3) )
       W3 = 0.0d0
       iflag_work_matvec33d = 3*3*NP
!
      end subroutine allocate_work_4_matvec33d
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_matvec3x33d(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W3(3*NP,9) )
       W3 = 0.0d0
       iflag_work_matvec33d = 9*3*NP
!
      end subroutine allocate_work_4_matvec3x33d
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_matvec33d
!
       deallocate ( W3 )
       iflag_work_matvec33d = 0
!
      end subroutine deallocate_work_4_matvec33d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine change_order_2_solve_bx3d(NP, PEsmpTOT, STACKmcG,     &
     &           NtoO, B, X)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO(NP)
       real(kind = kreal), intent(inout) :: B(3*NP)
       real(kind = kreal), intent(inout) :: X(3*NP)
!
       call s_change_order_2_solve_bx3(NP, PEsmpTOT, STACKmcG,          &
     &     NtoO, B, X, W3(1,IWK1),  W3(1,IWK2))
!
      end subroutine change_order_2_solve_bx3d
!
!  ---------------------------------------------------------------------
!
      subroutine back_2_original_order_bx3d(NP, NtoO, B, X)
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: NtoO(NP)
      real(kind = kreal), intent(inout) :: B(3*NP)
      real(kind = kreal), intent(inout) :: X(3*NP)
!
!
      call s_back_2_original_order_bx3(NP, NtoO, B, X,                  &
     &    W3(1,IWK1),  W3(1,IWK2))
!
      end subroutine back_2_original_order_bx3d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine cal_matvec_33d                                        &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            S, X)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT), NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL), IAU(NPU)
!
       real(kind = kreal), intent(in) :: X(3*NP), D(9*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL), AU(9*NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
      call set_by_diagonal_33d(NP, PEsmpTOT, STACKmcG,                  &
     &    W3(1,IWK3), X, D)

      call ordering_3x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,            &
     &    OtoN_L, W3(1,IWK1), W3(1,IWK2), X, W3(1,IWK3) )

      call add_lower_33d(NP, NL, NPL, PEsmpTOT, NVECT,                  &
     &    npLX1, STACKmc, NLhyp, INL, IAL, W3(1,IWK2), AL, W3(1,IWK1) )

      call ordering_3x1_l2u_o2n_u(NP, OtoN_U, LtoU,                     &
     &    W3(1,IWK1), W3(1,IWK3), X, W3(1,IWK2) )

      call add_upper_33d(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,           &
     &    STACKmc, NUhyp, INU, IAU, W3(1,IWK3), AU, W3(1,IWK1) )

      call ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &    NtoO_U, S, W3(1,IWK3) )
!
       end subroutine cal_matvec_33d
!
!  ---------------------------------------------------------------------
!
       subroutine cal_matvec_3x33d                                      &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            S1, S2, S3, X1, X2, X3 )
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT), NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL), IAU(NPU)
!
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL), AU(9*NPU)
       real(kind = kreal), intent(in) :: X1(3*NP), X2(3*NP), X3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
       call set_by_diagonal_3x33d(NP, PEsmpTOT, STACKmcG,               &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3, D)

       call ordering_3x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                       &
     &     W3(1,IWK12), W3(1,IWK22), W3(1,IWK32), X1, X2, X3,           &
     &     W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )

      call add_lower_3x33d(NP, NL, NPL, PEsmpTOT, NVECT,                &
     &    npLX1, STACKmc, NLhyp, INL, IAL, W3(1,IWK12), W3(1,IWK22),    &
     &    W3(1,IWK32), AL, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

      call ordering_3x3_l2u_o2n_u(NP, OtoN_U, LtoU,                     &
     &    W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                        &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3,            &
     &    W3(1,IWK12), W3(1,IWK22), W3(1,IWK32) )

      call add_upper_3x33d(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,         &
     &    STACKmc, NUhyp, INU, IAU, W3(1,IWK13), W3(1,IWK23),           &
     &    W3(1,IWK33), AU, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

      call ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &    NtoO_U, S1, S2, S3, W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )
!
       end subroutine cal_matvec_3x33d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtruct_matvec_33d                                   &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            S, B, X )
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT), NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL), IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(3*NP), X(3*NP), D(9*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL), AU(9*NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
       call subtract_diagonal_33d(NP, PEsmpTOT, STACKmcG,               &
     &     W3(1,IWK3), B, X, D)

       call ordering_3x2_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W3(1,IWK1), W3(1,IWK2), X, W3(1,IWK3) )

       call subtract_lower_33d(NP, NL, NPL, PEsmpTOT, NVECT, npLX1,     &
     &     STACKmc, NLhyp, INL, IAL, W3(1,IWK2), AL, W3(1,IWK1) )

       call ordering_3x1_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W3(1,IWK1), W3(1,IWK3), X, W3(1,IWK2) )

       call subtract_upper_33d(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,     &
     &     STACKmc, NUhyp, INU, IAU, W3(1,IWK3), AU, W3(1,IWK1) )

       call ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &     NtoO_U, S, W3(1,IWK3) )
!
       end subroutine subtruct_matvec_33d
!
!  ---------------------------------------------------------------------
!
       subroutine subtruct_matvec_3x33d                                 &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            S1, S2, S3, B1, B2, B3, X1, X2, X3 )
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT), NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL), IAU(NPU)
!
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL), AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), X1(3*NP)
       real(kind = kreal), intent(in) :: B2(3*NP), X2(3*NP)
       real(kind = kreal), intent(in) :: B3(3*NP), X3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
       call subtract_diagonal_3x33d(NP, PEsmpTOT, STACKmcG,             &
     &     W3(1,IWK13), W3(1,IWK23), W3(1,IWK33),                       &
     &     B1, B2, B3, X1, X2, X3, D)

       call ordering_3x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                       &
     &     W3(1,IWK12), W3(1,IWK22), W3(1,IWK32), X1, X2, X3,           &
     &     W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )

       call subtract_lower_3x33d(NP, NL, NPL, PEsmpTOT, NVECT, npLX1,   &
     &     STACKmc, NLhyp, INL, IAL, W3(1,IWK12), W3(1,IWK22),          &
     &     W3(1,IWK32), AL, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

       call ordering_3x3_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &    W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                        &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3,            &
     &    W3(1,IWK12), W3(1,IWK22), W3(1,IWK32) )

       call subtract_upper_3x33d(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,   &
     &     STACKmc, NUhyp, INU, IAU, W3(1,IWK13), W3(1,IWK23),          &
     &     W3(1,IWK33), AU, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

       call ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &     NtoO_U, S1, S2, S3, W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )
!
       end subroutine subtruct_matvec_3x33d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module djds_matrix_calcs_33d
