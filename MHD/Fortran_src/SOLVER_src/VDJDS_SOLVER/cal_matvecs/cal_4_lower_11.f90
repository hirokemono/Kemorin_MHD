!
!      module cal_4_lower_11
!
!     Written by Kemorin
!
!       subroutine subtract_lower_11(NP, NL, NPL, PEsmpTOT, NVECT,      &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!       subroutine subtract_lower_3x11(NP, NL, NPL, PEsmpTOT, NVECT,    &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,      &
!     &           V1, V2, V3 )
!
!       subroutine add_lower_11(NP, NL, NPL, PEsmpTOT, NVECT,           &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!       subroutine add_lower_3x11(NP, NL, NPL, PEsmpTOT, NVECT,         &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,      &
!     &           V1, V2, V3 )
!
      module cal_4_lower_11
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
       subroutine subtract_lower_11(NP, NL, NPL, PEsmpTOT, NVECT,       &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
!
       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(inout) :: S(NP)
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
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
            do i= iv0+1, iv0+iE-iS
              k  = i+iS - iv0
              kk = IAL(k)
              S(i)= S(i) - AL(k)*V(kk)
            enddo
          enddo
        enddo
      enddo
!$omp end parallel do

       end subroutine subtract_lower_11
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_lower_3x11(NP, NL, NPL, PEsmpTOT, NVECT,     &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
!
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
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
!cdir nodep
            do i= iv0+1, iv0+iE-iS
              k  = i+iS - iv0
              kk = IAL(k)
              S1(i)= S1(i) - AL(k)*V1(kk)
              S2(i)= S2(i) - AL(k)*V2(kk)
              S3(i)= S3(i) - AL(k)*V3(kk)
            enddo
          enddo
        enddo
      enddo
!$omp end parallel do

       end subroutine subtract_lower_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_lower_11(NP, NL, NPL, PEsmpTOT, NVECT,            &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(inout) :: S(NP)
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
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
            do i= iv0+1, iv0+iE-iS
              k  = i+iS - iv0
              kk = IAL(k)
              S(i)= S(i) + AL(k)*V(kk)
            enddo
          enddo
        enddo
      enddo
!$omp end parallel do
!
       end subroutine add_lower_11
!
!  ---------------------------------------------------------------------
!
       subroutine add_lower_3x11(NP, NL, NPL, PEsmpTOT, NVECT,          &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
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
!cdir nodep
            do i= iv0+1, iv0+iE-iS
              k  = i+iS - iv0
              kk = IAL(k)
              S1(i)= S1(i) + AL(k)*V1(kk)
              S2(i)= S2(i) + AL(k)*V2(kk)
              S3(i)= S3(i) + AL(k)*V3(kk)
            enddo
          enddo
        enddo
      enddo
!$omp end parallel do
!
       end subroutine add_lower_3x11
!
!  ---------------------------------------------------------------------
!
      end module cal_4_lower_11
