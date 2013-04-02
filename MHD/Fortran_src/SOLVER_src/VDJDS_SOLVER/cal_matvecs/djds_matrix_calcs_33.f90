!djds_matrix_calcs_33.f90
!      module djds_matrix_calcs_33
!
!     Written by Kemorin
!
!      subroutine verify_work_4_matvec33(NP)
!      subroutine verify_work_4_matvec3x33(NP)
!
!   Ordering by Multicolor
!       subroutine change_order_2_solve_bx3(NP, PEsmpTOT, STACKmcG,     &
!     &           NtoO, B, X)
!   Returning from Multicolor
!       subroutine back_2_original_order_bx3(NP, NtoO, B, X)
!
!C +-------------+
!C | {S} = {X}   |
!C +-------------+
!      subroutine copy_internal_vect_3_smp(NP, PEsmpTOT, STACKmcG, S, X)
!
!C +-------------+
!C | {S}= [A]{X} |
!C +-------------+
!
!       subroutine cal_matvec_33                                        &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S, X )
!       subroutine cal_matvec_3x33                                      &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S1, S2, S3, X1, X2, X3 )
!
!C +-------------- ----+
!C | {S}= {B} - [A]{X} |
!C +-------------------+
!
!       subroutine subtruct_matvec_33                                   &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S, B, X )
!       subroutine subtruct_matvec_3x33                                 &
!     &           (NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT, PEsmpTOT, &
!     &            STACKmcG, STACKmc, NLhyp, NUhyp, OtoN_L, OtoN_U,     &
!     &            NtoO_U, LtoU, INL, INU, IAL, IAU, D, AL, AU,         &
!     &            S1, S2, S3, B1, B2, B3, X1, X2, X3 )
!
      module djds_matrix_calcs_33
!
      use m_precision
!
      use cal_4_diagonal_33
      use cal_4_lower_33
      use cal_4_upper_33
      use ordering_by_o2nl_33
      use ordering_by_l2u_o2nu_33
      use ordering_by_new2old_U_33
      use order_vect_4_solver_33
!
      implicit none
!
      integer(kind = kint) :: iflag_work_matvec33 = 0
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
      private :: allocate_work_4_matvec33, allocate_work_4_matvec3x33
      private :: deallocate_work_4_matvec33
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_matvec33.eq.0) then
        call allocate_work_4_matvec33(NP)
      else if(iflag_work_matvec33 .lt. (3*3*NP)) then
        call deallocate_work_4_matvec33
        call allocate_work_4_matvec33(NP)
      end if
!
      end subroutine verify_work_4_matvec33
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec3x33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
      if (iflag_work_matvec33.eq.0) then
        call allocate_work_4_matvec3x33(NP)
      else if(iflag_work_matvec33 .lt. (9*3*NP)) then
        call deallocate_work_4_matvec33
        call allocate_work_4_matvec3x33(NP)
      end if
!
      end subroutine verify_work_4_matvec3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_matvec33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W3(3*NP,3) )
       W3 = 0.0d0
       iflag_work_matvec33 = 3*3*NP
!
      end subroutine allocate_work_4_matvec33
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_matvec3x33(NP)
!
       integer(kind = kint), intent(in) :: NP
!
       allocate ( W3(3*NP,9) )
       W3 = 0.0d0
       iflag_work_matvec33 = 9*3*NP
!
      end subroutine allocate_work_4_matvec3x33
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_matvec33
!
       deallocate ( W3 )
       iflag_work_matvec33 = 0
!
      end subroutine deallocate_work_4_matvec33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine change_order_2_solve_bx3(NP, PEsmpTOT, STACKmcG,      &
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
      end subroutine change_order_2_solve_bx3
!
!  ---------------------------------------------------------------------
!
      subroutine back_2_original_order_bx3(NP, NtoO, B, X)
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
      end subroutine back_2_original_order_bx3
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_internal_vect_3_smp(NP, PEsmpTOT, STACKmcG, S, X)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: X(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
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
           S(3*i-2) = X(3*i-2)
           S(3*i-1) = X(3*i-1)
           S(3*i  ) = X(3*i  )
         enddo
       enddo
!$omp end parallel do
!
      end subroutine copy_internal_vect_3_smp
!
!  ---------------------------------------------------------------------
!
       subroutine cal_matvec_33                                         &
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
       real(kind = kreal), intent(in) :: X(3*NP)
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
      call set_by_diagonal_33(NP, PEsmpTOT, STACKmcG,                   &
     &    W3(1,IWK3), X, D)

      call ordering_3x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,            &
     &    OtoN_L, W3(1,IWK1), W3(1,IWK2), X, W3(1,IWK3) )

      call add_lower_33(NP, NL, NPL, PEsmpTOT, NVECT,                   &
     &    npLX1, STACKmc, NLhyp, INL, IAL, W3(1,IWK2), AL, W3(1,IWK1) )

      call ordering_3x1_l2u_o2n_u(NP, OtoN_U, LtoU,                     &
     &    W3(1,IWK1), W3(1,IWK3), X, W3(1,IWK2) )

      call add_upper_33(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,            &
     &    STACKmc, NUhyp, INU, IAU, W3(1,IWK3), AU, W3(1,IWK1) )

      call ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &    NtoO_U, S, W3(1,IWK3) )
!
       end subroutine cal_matvec_33
!
!  ---------------------------------------------------------------------
!
       subroutine cal_matvec_3x33                                       &
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
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: X1(3*NP), X2(3*NP), X3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
      call set_by_diagonal_3x33(NP, PEsmpTOT, STACKmcG,                 &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3, D)

      call ordering_3x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,    &
     &     W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                       &
     &     W3(1,IWK12), W3(1,IWK22), W3(1,IWK32), X1, X2, X3,           &
     &     W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )

      call add_lower_3x33(NP, NL, NPL, PEsmpTOT, NVECT,                 &
     &    npLX1, STACKmc, NLhyp, INL, IAL, W3(1,IWK12), W3(1,IWK22),    &
     &    W3(1,IWK32), AL, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

      call ordering_3x3_l2u_o2n_u(NP, OtoN_U, LtoU,                     &
     &    W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                        &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3,            &
     &    W3(1,IWK12), W3(1,IWK22), W3(1,IWK32) )

      call add_upper_3x33(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,          &
     &    STACKmc, NUhyp, INU, IAU, W3(1,IWK13), W3(1,IWK23),           &
     &    W3(1,IWK33), AU, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

      call ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &    NtoO_U, S1, S2, S3, W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )
!
       end subroutine cal_matvec_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtruct_matvec_33                                    &
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
       real(kind = kreal), intent(in) :: B(3*NP), X(3*NP)
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
!
       call subtract_diagonal_33(NP, PEsmpTOT, STACKmcG,                &
     &     W3(1,IWK3), B, X, D)

       call ordering_3x2_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W3(1,IWK1), W3(1,IWK2), X, W3(1,IWK3) )

       call subtract_lower_33(NP, NL, NPL, PEsmpTOT, NVECT, npLX1,      &
     &     STACKmc, NLhyp, INL, IAL, W3(1,IWK2), AL, W3(1,IWK1) )

       call ordering_3x1_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &     W3(1,IWK1), W3(1,IWK3), X, W3(1,IWK2) )

       call subtract_upper_33(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,      &
     &     STACKmc, NUhyp, INU, IAU, W3(1,IWK3), AU, W3(1,IWK1) )

       call ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &     NtoO_U, S, W3(1,IWK3) )
!
       end subroutine subtruct_matvec_33
!
!  ---------------------------------------------------------------------
!
       subroutine subtruct_matvec_3x33                                  &
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
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: B1(3*NP), X1(3*NP)
       real(kind = kreal), intent(in) :: B2(3*NP), X2(3*NP)
       real(kind = kreal), intent(in) :: B3(3*NP), X3(3*NP)
!
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
!
       call subtract_diagonal_3x33(NP, PEsmpTOT, STACKmcG,              &
     &     W3(1,IWK13), W3(1,IWK23), W3(1,IWK33),                       &
     &     B1, B2, B3, X1, X2, X3, D)

       call ordering_3x6_by_old2new_L(NP, PEsmpTOT, STACKmcG, OtoN_L,   &
     &     W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                       &
     &     W3(1,IWK12), W3(1,IWK22), W3(1,IWK32), X1, X2, X3,           &
     &     W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )

       call subtract_lower_3x33(NP, NL, NPL, PEsmpTOT, NVECT, npLX1,    &
     &     STACKmc, NLhyp, INL, IAL, W3(1,IWK12), W3(1,IWK22),          &
     &     W3(1,IWK32), AL, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

       call ordering_3x3_l2u_o2n_u(NP, OtoN_U, LtoU,                    &
     &    W3(1,IWK11), W3(1,IWK21), W3(1,IWK31),                        &
     &    W3(1,IWK13), W3(1,IWK23), W3(1,IWK33), X1, X2, X3,            &
     &    W3(1,IWK12), W3(1,IWK22), W3(1,IWK32) )

       call subtract_upper_3x33(NP, NU, NPU, PEsmpTOT, NVECT, npUX1,    &
     &     STACKmc, NUhyp, INU, IAU, W3(1,IWK13), W3(1,IWK23),          &
     &     W3(1,IWK33), AU, W3(1,IWK11), W3(1,IWK21), W3(1,IWK31) )

       call ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,           &
     &     NtoO_U, S1, S2, S3, W3(1,IWK13), W3(1,IWK23), W3(1,IWK33) )
!
       end subroutine subtruct_matvec_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module djds_matrix_calcs_33
