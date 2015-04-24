!
!      module cal_4_lower_nnd
!
!     Written by Kemorin
!
!       subroutine subtract_lower_nnd(NP, NB, NL, NPL, PEsmpTOT, NVECT, &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!       subroutine subtract_lower_3xnnd(NP, NB, NL, NPL, PEsmpTOT,      &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3,   &
!     &           AL, V1, V2, V3 )
!
!       subroutine add_lower_nnd(NP, NB, NL, NPL, PEsmpTOT, NVECT,      &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!       subroutine add_lower_3xnnd(NP, NB, NL, NPL, PEsmpTOT, NVECT,    &
!     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,      &
!     &           V1, V2, V3 )
!
      module cal_4_lower_nnd
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
       subroutine subtract_lower_nnd(NP, NB, NL, NPL, PEsmpTOT, NVECT,  &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!
       integer(kind = kint), intent(in) :: NP, NB, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
!
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k
       integer (kind = kint) :: ii, ix, im, k1
!
!
!cdir parallel do private(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
!$omp parallel do private(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
!poption indep (S,V,AL,INL,IAL,STACKmc) 
!poption tlocal(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
      do ip= 1, PEsmpTOT
!poption noparallel
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
        do  j= 1, NLhyp(iv)
          iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
          iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
          do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,V,AL,IAL)
              do i= iv0+1, iv0+iE-iS
                k  = i+iS - iv0
                ii = NB*(i-1)      + k1
                ix = NB*(IAL(k)-1) + k1
                im = NB*NB*(k-1)   + NB*(k1-1) + k1
                S(ii)= S(ii) - AL(im)*V(ix)
              enddo
          end do
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine subtract_lower_nnd
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_lower_3xnnd(NP, NB, NL, NPL, PEsmpTOT,       &
     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3,    &
     &           AL, V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NB, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
!
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k
       integer (kind = kint) :: ii, ix, im, k1
!
!
!cdir parallel do private(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
!$omp parallel do private(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
!poption indep (S1,S2,S3,V1,V2,V3,AL,INL,IAL,STACKmc) 
!poption tlocal(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
      do ip= 1, PEsmpTOT
!poption noparallel
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
        do  j= 1, NLhyp(iv)
          iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
          iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
          do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,V1,V2,V3,AL,IAL)
              do i= iv0+1, iv0+iE-iS
                k  = i+iS - iv0
                ii = NB*(i-1)      + k1
                ix = NB*(IAL(k)-1) + k1
                im = NB*NB*(k-1)   + NB*(k1-1) + k1
                S1(ii)= S1(ii) - AL(im)*V1(ix)
                S2(ii)= S2(ii) - AL(im)*V2(ix)
                S3(ii)= S3(ii) - AL(im)*V3(ix)
              enddo
          end do
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine subtract_lower_3xnnd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_lower_nnd(NP, NB, NL, NPL, PEsmpTOT, NVECT,       &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V )
!
       integer(kind = kint), intent(in) :: NP, NB, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k
       integer (kind = kint) :: ii, ix, im, k1
!
!
!cdir parallel do private(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
!$omp parallel do private(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
!poption indep (S,V,AL,INL,IAL,STACKmc) 
!poption tlocal(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
      do ip= 1, PEsmpTOT
!poption noparallel
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
        do  j= 1, NLhyp(iv)
          iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
          iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
          do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,V,AL,IAL)
              do i= iv0+1, iv0+iE-iS
                k  = i+iS - iv0
                ii = NB*(i-1) +      k1
                ix = NB*(IAL(k)-1) + k1
                im = NB*NB*(k-1) +   NB*(k1-1) + k1
                S(ii)= S(ii) + AL(im)*V(ix)
              enddo
          end do
        enddo
       enddo
      enddo
!$omp end parallel do
!
       end subroutine add_lower_nnd
!
!  ---------------------------------------------------------------------
!
       subroutine add_lower_3xnnd(NP, NB, NL, NPL, PEsmpTOT, NVECT,     &
     &           npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3, AL,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NB, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k
       integer (kind = kint) :: ii, ix, im, k1
!
!
!cdir parallel do private(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
!$omp parallel do private(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
!poption indep (S1,S2,S3,V1,V2,V3,AL,INL,IAL,STACKmc) 
!poption tlocal(iv,iv0,j,iS,iE,i,k,k1,ii,ix,im) 
      do ip= 1, PEsmpTOT
!poption noparallel
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
        do  j= 1, NLhyp(iv)
          iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
          iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
          do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,V1,V2,V3,AL,IAL)
              do i= iv0+1, iv0+iE-iS
                k  = i+iS - iv0
                ii = NB*(i-1) +      k1
                ix = NB*(IAL(k)-1) + k1
                im = NB*NB*(k-1) +   NB*(k1-1) + k1
                S1(ii)= S1(ii) + AL(im)*V1(ix)
                S2(ii)= S2(ii) + AL(im)*V2(ix)
                S3(ii)= S3(ii) + AL(im)*V3(ix)
              enddo
          end do
        enddo
       enddo
      enddo
!$omp end parallel do
!
       end subroutine add_lower_3xnnd
!
!  ---------------------------------------------------------------------
!
      end module cal_4_lower_nnd
