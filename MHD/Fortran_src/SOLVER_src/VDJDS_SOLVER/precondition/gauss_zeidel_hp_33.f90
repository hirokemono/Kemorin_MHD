!
!      module gauss_zeidel_hp_33
!
!     Written by Kemorin
!
!       subroutine gauss_zeidel_hp_33_l(iv, NP, NL, NPL, PEsmpTOT,      &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V)
!       subroutine gauss_zeidel_hp_3x33_l(iv, NP, NL, NPL,              &
!     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL,     &
!     &            S1, S2, S3, AL, V1, V2, V3)
!
!       subroutine gauss_zeidel_hp_33_u(iv, NP, N, NU, NPU, PEsmpTOT,   &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU, S, AU,        &
!     &           ALU_U, V)
!       subroutine gauss_zeidel_hp_3x33_u(iv, NP, N, NU, NPU,           &
!     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,     &
!     &           S1, S2, S3, AU, ALU_U, V1, V2, V3)
!
!        at (9*i-8, 9*i-4, 9*i) 
!       ALU_U = 1 / Diag (Ordered by DJDS ordering for upper component)
!        else
!       ALU_U = Diag (Ordered by DJDS ordering for upper component)
!
      module gauss_zeidel_hp_33
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_33_l(iv, NP, NL, NPL, PEsmpTOT,       &
     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT) 
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k, kk
       real(kind = kreal) :: Zm0, Zm1, Zm2
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk,Zm0,Zm1,Zm2)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
!cdir nodep
!voption indep (S,AL,IAL)
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAL(k)
              Zm2= V(3*kk-2)
              Zm1= V(3*kk-1)
              Zm0= V(3*kk  )
              S(3*i-2) = S(3*i-2)                                       &
     &                - AL(9*k-8)*Zm2 - AL(9*k-7)*Zm1 - AL(9*k-6)*Zm0
              S(3*i-1) = S(3*i-1)                                       &
     &                - AL(9*k-5)*Zm2 - AL(9*k-4)*Zm1 - AL(9*k-3)*Zm0
              S(3*i  ) = S(3*i  )                                       &
     &                - AL(9*k-2)*Zm2 - AL(9*k-1)*Zm1 - AL(9*k  )*Zm0
            end do
          end do
        end do
!$omp end parallel do

       end subroutine gauss_zeidel_hp_33_l
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_3x33_l(iv, NP, NL, NPL,               &
     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL,      &
     &            S1, S2, S3, AL, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT) 
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP), S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k, kk
       real(kind = kreal) :: Zm10, Zm11, Zm12
       real(kind = kreal) :: Zm20, Zm21, Zm22
       real(kind = kreal) :: Zm30, Zm31, Zm32
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk) &
!$omp& private(Zm10,Zm11,Zm12,Zm20,Zm21,Zm22,Zm30,Zm31,Zm32)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
!cdir nodep
!voption indep (S1,S2,S3,AL,IAL)
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAL(k)
              Zm12= V1(3*kk-2)
              Zm11= V1(3*kk-1)
              Zm10= V1(3*kk  )
              Zm22= V2(3*kk-2)
              Zm21= V2(3*kk-1)
              Zm20= V2(3*kk  )
              Zm32= V3(3*kk-2)
              Zm31= V3(3*kk-1)
              Zm30= V3(3*kk  )
!
              S1(3*i-2) = S1(3*i-2)                                     &
     &               - AL(9*k-8)*Zm12 - AL(9*k-7)*Zm11 - AL(9*k-6)*Zm10
              S2(3*i-2) = S2(3*i-2)                                     &
     &               - AL(9*k-8)*Zm22 - AL(9*k-7)*Zm21 - AL(9*k-6)*Zm20
              S3(3*i-2) = S3(3*i-2)                                     &
     &               - AL(9*k-8)*Zm32 - AL(9*k-7)*Zm31 - AL(9*k-6)*Zm30
              S1(3*i-1) = S1(3*i-1)                                     &
     &               - AL(9*k-5)*Zm12 - AL(9*k-4)*Zm11 - AL(9*k-3)*Zm10
              S2(3*i-1) = S2(3*i-1)                                     &
     &               - AL(9*k-5)*Zm22 - AL(9*k-4)*Zm21 - AL(9*k-3)*Zm20
              S3(3*i-1) = S3(3*i-1)                                     &
     &               - AL(9*k-5)*Zm32 - AL(9*k-4)*Zm31 - AL(9*k-3)*Zm30
              S1(3*i  ) = S1(3*i  )                                     &
     &               - AL(9*k-2)*Zm12 - AL(9*k-1)*Zm11 - AL(9*k  )*Zm10
              S2(3*i  ) = S2(3*i  )                                     &
     &               - AL(9*k-2)*Zm22 - AL(9*k-1)*Zm21 - AL(9*k  )*Zm20
              S3(3*i  ) = S3(3*i  )                                     &
     &               - AL(9*k-2)*Zm32 - AL(9*k-1)*Zm31 - AL(9*k  )*Zm30
            end do
          end do
        end do
!$omp end parallel do
!
       end subroutine gauss_zeidel_hp_3x33_l
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_33_u(iv, NP, N, NU, NPU, PEsmpTOT,    &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU, S, AU,         &
     &           ALU_U, V)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, N, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k, kk, iv2, ivv
       real(kind = kreal) :: X1, X2, X3, Zm0, Zm1, Zm2
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk,iv2,ivv, &
!$omp&   Zm0,Zm1,Zm2,X1,X2,X3)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!cdir nodep
!voption indep (S,AL,IAL)
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAU(k)
              Zm2= V(3*kk-2)
              Zm1= V(3*kk-1)
              Zm0= V(3*kk  )
              S(3*i-2) = S(3*i-2)                                       &
     &                - AU(9*k-8)*Zm2 - AU(9*k-7)*Zm1 - AU(9*k-6)*Zm0
              S(3*i-1) = S(3*i-1)                                       &
     &                - AU(9*k-5)*Zm2 - AU(9*k-4)*Zm1 - AU(9*k-3)*Zm0
              S(3*i  ) = S(3*i  )                                       &
     &                - AU(9*k-2)*Zm2 - AU(9*k-1)*Zm1 - AU(9*k  )*Zm0
            end do
          end do
!
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!cdir nodep noloopchg
!voption indep (S,ALU_U)
          do i= iS, iE
            X1= S(3*i-2)
            X2= S(3*i-1)
            X3= S(3*i  )
!
            X1= X1 - ALU_U(9*i-7)*X2 - ALU_U(9*i-6)*X3
            X2= X2 - ALU_U(9*i-5)*X1 - ALU_U(9*i-3)*X3
            X3= X3 - ALU_U(9*i-2)*X1 - ALU_U(9*i-1)*X2
!
            S(3*i-2)  =  ALU_U(9*i-8) * X1
            S(3*i-1)  =  ALU_U(9*i-8) * X2
            S(3*i  )  =  ALU_U(9*i-8) * X3
          end do
!
          do iv2 = 1, NVECT-1
            ivv = mod(iv2+iv-1,NVECT) + 1
            iS= STACKmc(PEsmpTOT*(ivv-1)+ip-1) + 1
            iE= STACKmc(PEsmpTOT*(ivv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_U)
            do i= iS, iE
              S(3*i-2)  =  V(3*i-2)
              S(3*i-1)  =  V(3*i-1)
              S(3*i  )  =  V(3*i  )
            end do
          end do
!
        end do
!$omp end parallel do

       end subroutine gauss_zeidel_hp_33_u
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_3x33_u(iv, NP, N, NU, NPU,            &
     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,      &
     &           S1, S2, S3, AU, ALU_U, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, N, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(in) :: ALU_U(9*N)
       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP), S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k, kk, iv2, ivv
       real(kind = kreal) :: X11, X12, X13, Zm10, Zm11, Zm12
       real(kind = kreal) :: X21, X22, X23, Zm20, Zm21, Zm22
       real(kind = kreal) :: X31, X32, X33, Zm30, Zm31, Zm32
!
!
!$omp parallel do  &
!$omp& private(iv0,j,iS,iE,i,k,kk,iv2,ivv) &
!$omp& private(Zm10,Zm11,Zm12,Zm20,Zm21,Zm22,Zm30,Zm31,Zm32) &
!$omp& private(X11,X12,X13,X21,X22,X23,X31,X32,X33)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip-1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!cdir nodep
!voption indep (S1,S2,S3,AL,IAL)
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAU(k)
              Zm12= V1(3*kk-2)
              Zm11= V1(3*kk-1)
              Zm10= V1(3*kk  )
              Zm22= V2(3*kk-2)
              Zm21= V2(3*kk-1)
              Zm20= V2(3*kk  )
              Zm32= V3(3*kk-2)
              Zm31= V3(3*kk-1)
              Zm30= V3(3*kk  )
!
              S1(3*i-2) = S1(3*i-2)                                     &
     &               - AU(9*k-8)*Zm12 - AU(9*k-7)*Zm11 - AU(9*k-6)*Zm10
              S2(3*i-2) = S2(3*i-2)                                     &
     &               - AU(9*k-8)*Zm22 - AU(9*k-7)*Zm21 - AU(9*k-6)*Zm20
              S3(3*i-2) = S3(3*i-2)                                     &
     &               - AU(9*k-8)*Zm32 - AU(9*k-7)*Zm31 - AU(9*k-6)*Zm30
              S1(3*i-1) = S1(3*i-1)                                     &
     &               - AU(9*k-5)*Zm12 - AU(9*k-4)*Zm11 - AU(9*k-3)*Zm10
              S2(3*i-1) = S2(3*i-1)                                     &
     &               - AU(9*k-5)*Zm22 - AU(9*k-4)*Zm21 - AU(9*k-3)*Zm20
              S3(3*i-1) = S3(3*i-1)                                     &
     &               - AU(9*k-5)*Zm32 - AU(9*k-4)*Zm31 - AU(9*k-3)*Zm30
              S1(3*i  ) = S1(3*i  )                                     &
     &               - AU(9*k-2)*Zm12 - AU(9*k-1)*Zm11 - AU(9*k  )*Zm10
              S2(3*i  ) = S2(3*i  )                                     &
     &               - AU(9*k-2)*Zm22 - AU(9*k-1)*Zm21 - AU(9*k  )*Zm20
              S3(3*i  ) = S3(3*i  )                                     &
     &               - AU(9*k-2)*Zm32 - AU(9*k-1)*Zm31 - AU(9*k  )*Zm30
            end do
          end do
!
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,ALU_U)
          do i= iS, iE
            X11= S1(3*i-2)
            X12= S1(3*i-1)
            X13= S1(3*i  )
            X21= S2(3*i-2)
            X22= S2(3*i-1)
            X23= S2(3*i  )
            X31= S3(3*i-2)
            X32= S3(3*i-1)
            X33= S3(3*i  )
!
            X11= X11 - ALU_U(9*i-7)*X12 - ALU_U(9*i-6)*X13
            X21= X21 - ALU_U(9*i-7)*X22 - ALU_U(9*i-6)*X23
            X31= X31 - ALU_U(9*i-7)*X32 - ALU_U(9*i-6)*X33
!
            X12= X12 - ALU_U(9*i-5)*X11 - ALU_U(9*i-3)*X13
            X22= X22 - ALU_U(9*i-5)*X21 - ALU_U(9*i-3)*X23
            X32= X32 - ALU_U(9*i-5)*X31 - ALU_U(9*i-3)*X33
!
            X13= X13 - ALU_U(9*i-2)*X11 - ALU_U(9*i-1)*X12
            X23= X23 - ALU_U(9*i-2)*X21 - ALU_U(9*i-1)*X22
            X33= X33 - ALU_U(9*i-2)*X31 - ALU_U(9*i-1)*X32
!
            S1(3*i-2)  =  ALU_U(9*i-8) * X11
            S1(3*i-1)  =  ALU_U(9*i-8) * X12
            S1(3*i  )  =  ALU_U(9*i-8) * X13
            S2(3*i-2)  =  ALU_U(9*i-4) * X21
            S2(3*i-1)  =  ALU_U(9*i-4) * X22
            S2(3*i  )  =  ALU_U(9*i-4) * X23
            S3(3*i-2)  =  ALU_U(9*i  ) * X31
            S3(3*i-1)  =  ALU_U(9*i  ) * X32
            S3(3*i  )  =  ALU_U(9*i  ) * X33
          end do
!
          do iv2 = 1, NVECT-1
            ivv = mod(iv2+iv-1,NVECT) + 1
            iS= STACKmc(PEsmpTOT*(ivv-1)+ip-1) + 1
            iE= STACKmc(PEsmpTOT*(ivv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_U)
            do i= iS, iE
              S1(3*i-2)  =  V1(3*i-2)
              S1(3*i-1)  =  V1(3*i-1)
              S1(3*i  )  =  V1(3*i  )
              S2(3*i-2)  =  V2(3*i-2)
              S2(3*i-1)  =  V2(3*i-1)
              S2(3*i  )  =  V2(3*i  )
              S3(3*i-2)  =  V3(3*i-2)
              S3(3*i-1)  =  V3(3*i-1)
              S3(3*i  )  =  V3(3*i  )
            end do
          end do
!
        end do
!$omp end parallel do

       end subroutine gauss_zeidel_hp_3x33_u
!
!  ---------------------------------------------------------------------
!
      end module gauss_zeidel_hp_33
