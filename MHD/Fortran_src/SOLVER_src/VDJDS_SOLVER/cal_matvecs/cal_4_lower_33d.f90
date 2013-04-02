!
!      module cal_4_lower_33d
!
!     Written by Kemorin
!
!       subroutine subtract_lower_33d(NP, NL, NPL, PEsmpTOT, NVECT,     &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!       subroutine subtract_lower_3x33d(NP, NL, NPL, PEsmpTOT, NVECT,   &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,      &
!     &           V1, V2, V3 )
!
!       subroutine add_lower_33d(NP, NL, NPL, PEsmpTOT, NVECT,          &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!       subroutine add_lower_3x33d(NP, NL, NPL, PEsmpTOT, NVECT,        &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,      &
!     &           V1, V2, V3 )
!
      module cal_4_lower_33d
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
       subroutine subtract_lower_33d(NP, NL, NPL, PEsmpTOT, NVECT,      &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!$omp parallel do private(iv,iv0,j,iS,iE,i,k,kk) 
      do ip= 1, PEsmpTOT
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
        do  j= 1, NLhyp(iv)
          iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
          iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
!voption indep (S,V,AL,IAL)
          do i= iv0+1, iv0+iE-iS
             k= i+iS - iv0
            kk= IAL(k)
            S(3*i-2)= S(3*i-2) - AL(9*k-8) * V(3*kk-2)
            S(3*i-1)= S(3*i-1) - AL(9*k-4) * V(3*kk-1)
            S(3*i  )= S(3*i  ) - AL(9*k  ) * V(3*kk  )
          enddo
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine subtract_lower_33d
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_lower_3x33d(NP, NL, NPL, PEsmpTOT, NVECT,    &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
!$omp parallel do private(iv,iv0,j,iS,iE,i,k,kk) 
      do ip= 1, PEsmpTOT
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
        do  j= 1, NLhyp(iv)
          iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
          iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!voption indep (S1,S2,S3,V1,V2,V3,AL,IAL)
!cdir nodep
          do i= iv0+1, iv0+iE-iS
             k= i+iS - iv0
            kk= IAL(k)
            S1(3*i-2)= S1(3*i-2) - AL(9*k-8) * V1(3*kk-2)
            S2(3*i-2)= S2(3*i-2) - AL(9*k-8) * V2(3*kk-2)
            S3(3*i-2)= S3(3*i-2) - AL(9*k-8) * V3(3*kk-2)
            S1(3*i-1)= S1(3*i-1) - AL(9*k-4) * V1(3*kk-1)
            S2(3*i-1)= S2(3*i-1) - AL(9*k-4) * V2(3*kk-1)
            S3(3*i-1)= S3(3*i-1) - AL(9*k-4) * V3(3*kk-1)
            S1(3*i  )= S1(3*i  ) - AL(9*k  ) * V1(3*kk  )
            S2(3*i  )= S2(3*i  ) - AL(9*k  ) * V2(3*kk  )
            S3(3*i  )= S3(3*i  ) - AL(9*k  ) * V3(3*kk  )
          enddo
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine subtract_lower_3x33d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_lower_33d(NP, NL, NPL, PEsmpTOT, NVECT,           &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!$omp parallel do private(iv,iv0,j,iS,iE,i,k,kk) 
      do ip= 1, PEsmpTOT
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
        do  j= 1, NLhyp(iv)
          iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
          iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
!voption indep (S,V,AL,IAL)
          do i= iv0+1, iv0+iE-iS
             k= i+iS - iv0
            kk= IAL(k)
            S(3*i-2)= S(3*i-2) + AL(9*k-8) * V(3*kk-2)
            S(3*i-1)= S(3*i-1) + AL(9*k-4) * V(3*kk-1)
            S(3*i  )= S(3*i  ) + AL(9*k  ) * V(3*kk  )
          enddo
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine add_lower_33d
!
!  ---------------------------------------------------------------------
!
       subroutine add_lower_3x33d(NP, NL, NPL, PEsmpTOT, NVECT,         &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!$omp parallel do private(iv,iv0,j,iS,iE,i,k,kk) 
      do ip= 1, PEsmpTOT
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
        do  j= 1, NLhyp(iv)
          iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
          iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep
!voption indep (S1,S2,S3,V1,V2,V3,AL,IAL)
          do i= iv0+1, iv0+iE-iS
             k= i+iS - iv0
            kk= IAL(k)
            S1(3*i-2)= S1(3*i-2) + AL(9*k-8) * V1(3*kk-2)
            S2(3*i-2)= S2(3*i-2) + AL(9*k-8) * V2(3*kk-2)
            S3(3*i-2)= S3(3*i-2) + AL(9*k-8) * V3(3*kk-2)
            S1(3*i-1)= S1(3*i-1) + AL(9*k-4) * V1(3*kk-1)
            S2(3*i-1)= S2(3*i-1) + AL(9*k-4) * V2(3*kk-1)
            S3(3*i-1)= S3(3*i-1) + AL(9*k-4) * V3(3*kk-1)
            S1(3*i  )= S1(3*i  ) + AL(9*k  ) * V1(3*kk  )
            S2(3*i  )= S2(3*i  ) + AL(9*k  ) * V2(3*kk  )
            S3(3*i  )= S3(3*i  ) + AL(9*k  ) * V3(3*kk  )
          enddo
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine add_lower_3x33d
!
!  ---------------------------------------------------------------------
!
      end module cal_4_lower_33d
