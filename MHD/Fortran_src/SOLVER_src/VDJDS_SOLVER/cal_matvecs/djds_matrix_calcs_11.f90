!djds_matrix_calcs_11
!      module djds_matrix_calcs_11
!
!     Written by Kemorin
!
!      subroutine verify_work_4_matvec11(NP)
!      subroutine verify_work_4_matvec3x11(NP)
!
!   Ordering by Multicolor
!       subroutine change_order_2_solve_bx1(NP, PEsmpTOT, STACKmcG,     &
!     &           NtoO, B, X)
!   Returning from Multicolor
!       subroutine back_2_original_order_bx1(NP, NtoO, B, X)
!
!C +-------------+
!C | {S} = {X}   |
!C +-------------+
!      subroutine copy_internal_vect_1_smp(NP, PEsmpTOT, STACKmcG, S, X)
!
!C +-------------+
!C | {S}= [A]{X} |
!C +-------------+
!       subroutine cal_matvec_11                                        &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S, X )
!       subroutine cal_matvec_3x11                                      &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S1, S2, S3, X1, X2, X3 )
!
!C +-------------- ----+
!C | {S}= {B} - [A]{X} |
!C +-------------------+
!       subroutine subtruct_matvec_11                                   &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S, B, X )
!       subroutine subtruct_matvec_3x11                                 &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S1, S2, S3, B1, B2, B3, X1, X2, X3 )
!
      module djds_matrix_calcs_11
!
      use m_precision
!
      use cal_4_diagonal_11
      use cal_4_lower_11
      use cal_4_upper_11
      use ordering_by_o2nl_11
      use ordering_by_l2u_o2nu_11
      use ordering_by_new2old_U_11
      use order_vect_4_solver_11
!
      implicit none
!
       integer(kind = kint) :: iflag_work_matvec11 = 0
       integer(kind = kint), parameter :: IWK1 = 1, IWK2 = 2, IWK3 = 3
       integer(kind = kint), parameter :: IWK11= 1, IWK12= 4, IWK13= 7
       integer(kind = kint), parameter :: IWK21= 2, IWK22= 5, IWK23= 8
       integer(kind = kint), parameter :: IWK31= 3, IWK32= 6, IWK33= 9
       real(kind = kreal), allocatable :: W3(:,:)
!
       private :: W3
       private :: IWK1, IWK2, IWK3
       private :: IWK11, IWK12, IWK13
       private :: IWK21, IWK22, IWK23
       private :: IWK31, IWK32, IWK33
!
       private :: allocate_work_4_matvec11, allocate_work_4_matvec3x11
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_matvec11.eq.0) then
        call allocate_work_4_matvec11(NP)
      else if (iflag_work_matvec11.lt. (3*NP)) then
        call deallocate_work_4_matvec11
        call allocate_work_4_matvec11(NP)
      end if
!
      end subroutine verify_work_4_matvec11
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec3x11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_matvec11.eq.0) then
        call allocate_work_4_matvec3x11(NP)
      else if (iflag_work_matvec11.lt. (9*NP)) then
        call deallocate_work_4_matvec11
        call allocate_work_4_matvec3x11(NP)
      end if
!
      end subroutine verify_work_4_matvec3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_matvec11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W3(NP,3) )
       W3 = 0.0d0
       iflag_work_matvec11 = 3*NP
!
      end subroutine allocate_work_4_matvec11
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_matvec3x11(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W3(NP,9) )
       W3 = 0.0d0
       iflag_work_matvec11 = 9*NP
!
      end subroutine allocate_work_4_matvec3x11
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_matvec11
!
       deallocate ( W3 )
       iflag_work_matvec11 = 0
!
      end subroutine deallocate_work_4_matvec11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine change_order_2_solve_bx1(NP, PEsmpTOT, STACKmcG,      &
     &           NtoO, B, X)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO(NP)
       real(kind = kreal), intent(inout) :: B(NP)
       real(kind = kreal), intent(inout) :: X(NP)
!
!
       call s_change_order_2_solve_bx1(NP, PEsmpTOT, STACKmcG,          &
     &     NtoO, B, X, W3(1,IWK1), W3(1,IWK2) )
!
      end subroutine change_order_2_solve_bx1
!
!  ---------------------------------------------------------------------
!
      subroutine back_2_original_order_bx1(NP, NtoO, B, X)
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: NtoO(NP)
      real(kind = kreal), intent(inout) :: B(NP)
      real(kind = kreal), intent(inout) :: X(NP)
!
!
      call s_back_2_original_order_bx1(NP, NtoO, B, X,                  &
     &    W3(1,IWK1), W3(1,IWK2) )
!
      end subroutine back_2_original_order_bx1
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_internal_vect_1_smp(NP, PEsmpTOT, STACKmcG, S, X)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: X(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
       do ip= 1, PEsmpTOT
         iS= STACKmcG(ip-1) + 1
         iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,X)
         do i= iS, iE
           S(i) = X(i)
         enddo
       enddo
!$omp end parallel do
!
      end subroutine copy_internal_vect_1_smp
!
!  ---------------------------------------------------------------------
!
       subroutine cal_matvec_11                                         &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            S, X )
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
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
       real(kind = kreal), intent(in) :: X(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
!
      call set_by_diagonal_11(NP, PEsmpTOT, STACKmcG,                   &
     &    W3(1,IWK3), X, D)

      call ordering_1x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,            &
     &    OtoN_L, W3(1,IWK1), W3(1,IWK2), X, W3(1,IWK3) )

      call add_lower_11(NP, NL, NPL, PEsmpTOT, NVECT,                   &
     &    npLX1, STACKmc, NLhyp, INL, IAL, W3(1,IWK2), AL, W3(1,IWK1) )

      call ordering_1x1_l2u_o2n_u(NP, OtoN_U, LtoU,                     &
     &    W3(1,IWK1), W3(1,IWK3), X, W3(1,IWK2) )

      call add_upper_11(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,            &
     &    STACKmc, NUhyp, INU, IAU, W3(1,IWK3), AU, W3(1,IWK1) )

      call ordering_1x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &    NtoO_U, S, W3(1,IWK3) )
!
       end subroutine cal_matvec_11
!
!  ---------------------------------------------------------------------
!
       subroutine cal_matvec_3x11                                       &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            S1, S2, S3, X1, X2, X3 )
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
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
       real(kind = kreal), intent(in) :: X1(NP), X2(NP), X3(NP)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
!
      call set_by_diagonal_3x11(NP, PEsmpTOT, STACKmcG,                 &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3, D)

      call ordering_1x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,    &
     &    W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                        &
     &    W3(1,IWK12), W3(1,IWK22), W3(1,IWK32), X1, X2, X3,            &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )

      call add_lower_3x11(NP, NL, NPL, PEsmpTOT, NVECT,                 &
     &    npLX1, STACKmc, NLhyp, INL, IAL, W3(1,IWK12), W3(1,IWK22),    &
     &    W3(1,IWK32), AL, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

      call ordering_1x3_l2u_o2n_u(NP, OtoN_U, LtoU,                     &
     &    W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                        &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3,            &
     &    W3(1,IWK12), W3(1,IWK22), W3(1,IWK32) )

      call add_upper_3x11(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,          &
     &    STACKmc, NUhyp, INU, IAU, W3(1,IWK13), W3(1,IWK23),           &
     &    W3(1,IWK33), AU, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

      call ordering_1x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &    NtoO_U, S1, S2, S3, W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )
!
       end subroutine cal_matvec_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtruct_matvec_11                                    &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            S, B, X )
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
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
       real(kind = kreal), intent(in) :: B(NP), X(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
!
       call subtract_diagonal_11(NP, PEsmpTOT, STACKmcG,                &
     &     W3(1,IWK3), B, X, D)

       call ordering_1x2_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W3(1,IWK1), W3(1,IWK2), X, W3(1,IWK3) )

       call subtract_lower_11(NP, NL, NPL, PEsmpTOT, NVECT, npLX1,      &
     &     STACKmc, NLhyp, INL, IAL, W3(1,IWK2), AL, W3(1,IWK1) )

       call ordering_1x1_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W3(1,IWK1), W3(1,IWK3), X, W3(1,IWK2) )

       call subtract_upper_11(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,      &
     &     STACKmc, NUhyp, INU, IAU, W3(1,IWK3), AU, W3(1,IWK1) )

       call ordering_1x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &     NtoO_U, S, W3(1,IWK3) )
!
       end subroutine subtruct_matvec_11
!
!  ---------------------------------------------------------------------
!
       subroutine subtruct_matvec_3x11                                  &
     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT,  &
     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,      &
     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,          &
     &            S1, S2, S3, B1, B2, B3, X1, X2, X3 )
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
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
       real(kind = kreal), intent(in) :: B1(NP), B2(NP), B3(NP)
       real(kind = kreal), intent(in) :: X1(NP), X2(NP), X3(NP)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
!
       call subtract_diagonal_3x11(NP, PEsmpTOT, STACKmcG,              &
     &     W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), B1, B2, B3,           &
     &     X1, X2, X3, D)

       call ordering_1x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &    W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                        &
     &    W3(1,IWK12), W3(1,IWK22), W3(1,IWK32), X1, X2, X3,            &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )

       call subtract_lower_3x11(NP, NL, NPL, PEsmpTOT, NVECT, npLX1,    &
     &     STACKmc, NLhyp, INL, IAL, W3(1,IWK12), W3(1,IWK22),          &
     &     W3(1,IWK32), AL, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

       call ordering_1x3_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &    W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                        &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3,            &
     &    W3(1,IWK12), W3(1,IWK22), W3(1,IWK32) )

       call subtract_upper_3x11(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,    &
     &     STACKmc, NUhyp, INU, IAU, W3(1,IWK13), W3(1,IWK23),          &
     &     W3(1,IWK33), AU, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

       call ordering_1x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &    NtoO_U, S1, S2, S3, W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )
!
       end subroutine subtruct_matvec_3x11
!
!  ---------------------------------------------------------------------
!
      end module djds_matrix_calcs_11
