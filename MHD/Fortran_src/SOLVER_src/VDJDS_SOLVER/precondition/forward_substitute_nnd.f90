!
!      module forward_substitute_nnd
!
!     Written by Kemorin
!
!       subroutine forward_substitute_1xnnd(N, NP, NB, NL, NPL,         &
!     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S,  &
!     &           AL, ALU_L)
!
!       subroutine forward_substitute_3xnnd(N, NP, NB, NL, NPL,         &
!     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL,     &
!     &           S1, S2, S3, AL, ALU_L)
!
      module forward_substitute_nnd
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
       subroutine forward_substitute_1xnnd(N, NP, NB, NL, NPL,          &
     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S,   &
     &           AL, ALU_L)
!
       integer (kind = kint), intent(in) :: N, NP, NB, NL, NPL
       integer (kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind=kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind=kint), intent(in) :: NLhyp(NVECT)
       integer(kind=kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind=kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j
       integer (kind = kint) :: k, k1, ii, ix, im
!
!
!poption noparallel
      do iv= 1, NVECT
!cdir parallel do private(iv0,iS,iE,i,j,k,k1,ii,ix,im)
!$omp parallel do private(iv0,iS,iE,i,j,k,k1,ii,ix,im)
!poption indep (S,AL,INL,IAL,STACKmc)
!poption tlocal(iv0,j,iS,iE,i,k,k1,ii,ix,im)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
            do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,AL,IAL)
              do i= iv0+1, iv0+iE-iS
                k= i+iS - iv0
                ii =    NB*( i - 1  ) +     k1
                ix =    NB*(IAL(k)-1)             + k1
                im = NB*NB*( k - 1  ) + NB*(k1-1) + k1
                S(ii) = S(ii) - AL(im) * S(ix)
              enddo
            end do
          end do
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i,k1,ii,ix,im)
!$omp parallel do private(iS,iE,i,k1,ii,ix,im)
!poption indep (S,ALU_L,STACKmc)
!poption tlocal(iS,iE,i,k1,k2,ii,ix,im)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )

!          do i= iS, iE
!            ii = NB*(i-1)+1
!            S(ii) = S(ii)
!          end do

          do k1 = NB, 1, -1
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,ALU_L)
            do i= iS, iE
              ii = NB*(i-1)+k1
              im = NB*NB*(i-1) + NB*(k1-1) + k1
              S(ii) = ALU_L(im) * S(ii)
            end do
          end do

        enddo
!$omp end parallel do

      enddo

       end subroutine forward_substitute_1xnnd
!
!  ---------------------------------------------------------------------
!
       subroutine forward_substitute_3xnnd(N, NP, NB, NL, NPL,          &
     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL,      &
     &           S1, S2, S3, AL, ALU_L)
!
       integer (kind = kint), intent(in) :: N, NP, NB, NL, NPL
       integer (kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind=kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind=kint), intent(in) :: NLhyp(NVECT)
       integer(kind=kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind=kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j
       integer (kind = kint) :: k, k1, ii, ix, im
!
!
!poption noparallel
      do iv= 1, NVECT
!cdir parallel do private(iv0,iS,iE,i,j,k,k1,ii,ix,im)
!$omp parallel do private(iv0,iS,iE,i,j,k,k1,ii,ix,im)
!poption indep (S1,S2,S3,AL,INL,IAL,STACKmc)
!poption tlocal(iv0,j,iS,iE,i,k,k1,ii,ix,im)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
            do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,AL,IAL)
              do i= iv0+1, iv0+iE-iS
                k= i+iS - iv0
                ii =    NB*( i - 1  ) +     k1
                ix =    NB*(IAL(k)-1)             + k1
                im = NB*NB*( k - 1  ) + NB*(k1-1) + k1
                S1(ii) = S1(ii) - AL(im) * S1(ix)
                S2(ii) = S2(ii) - AL(im) * S2(ix)
                S3(ii) = S3(ii) - AL(im) * S3(ix)
              enddo
            end do
          end do
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i,k1,ii,ix,im)
!$omp parallel do private(iS,iE,i,k1,ii,ix,im)
!poption indep (S1,S2,S3,ALU_L,STACKmc)
!poption tlocal(iS,iE,i,k1,k2,ii,ix,im)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )

!          do i= iS, iE
!            ii = NB*(i-1)+1
!            S1(ii) = S1(ii)
!            S2(ii) = S2(ii)
!            S3(ii) = S3(ii)
!          end do

          do k1 = NB, 1, -1
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,ALU_L)
            do i= iS, iE
              ii = NB*(i-1)+k1
              im = NB*NB*(i-1) + NB*(k1-1) + k1
              S1(ii) = ALU_L(im) * S1(ii)
              S2(ii) = ALU_L(im) * S2(ii)
              S3(ii) = ALU_L(im) * S3(ii)
            end do
          end do

        enddo
!$omp end parallel do

      enddo

       end subroutine forward_substitute_3xnnd
!
!  ---------------------------------------------------------------------
!
      end module forward_substitute_nnd
