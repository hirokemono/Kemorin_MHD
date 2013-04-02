!
!      module forward_substitute_11
!
!     Written by Kemorin
!
!       subroutine forward_substitute_1x11(N, NP, NL, NPL, PEsmpTOT,    &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)
!
!       subroutine forward_substitute_3x11(N, NP, NL, NPL, PEsmpTOT,    &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3,   &
!     &           AL, ALU_L)
!
      module forward_substitute_11
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
       subroutine forward_substitute_1x11(N, NP, NL, NPL, PEsmpTOT,     &
     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)
!
       integer(kind = kint), intent(in) :: N, NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT) 
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
!poption noparallel
      do iv= 1, NVECT
!cdir parallel do private(iv0,iS,iE,i,j,k,kk)
!$omp parallel do private(iv0,iS,iE,i,j,k,kk)
!poption indep (S,AL,INL,IAL,STACKmc)
!poption tlocal(iv0,j,iS,iE,i,k,kk)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,AL,IAL)
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAL(k)
              S(i) = S(i) - AL(k) * S(kk)
            enddo
          enddo
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (S,ALU_L,STACKmc)
!poption tlocal(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,ALU_L)
          do i= iS, iE
             S(i)  = ALU_L(i) * S(i)
          end do
        enddo
!$omp end parallel do

      enddo

       end subroutine forward_substitute_1x11
!
!  ---------------------------------------------------------------------
!
       subroutine forward_substitute_3x11(N, NP, NL, NPL, PEsmpTOT,     &
     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3,    &
     &           AL, ALU_L)
!
       integer(kind = kint), intent(in) :: N, NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT) 
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
!poption noparallel
      do iv= 1, NVECT
!cdir parallel do private(iv0,iS,iE,i,j,k,kk)
!$omp parallel do private(iv0,iS,iE,i,j,k,kk)
!poption indep (S1,S2,S3,AL,INL,IAL,STACKmc)
!poption tlocal(iv0,j,iS,iE,i,k,kk)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,AL,IAL)
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAL(k)
              S1(i) = S1(i) - AL(k) * S1(kk)
              S2(i) = S2(i) - AL(k) * S2(kk)
              S3(i) = S3(i) - AL(k) * S3(kk)
            enddo
          enddo
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (S1,S2,S3,ALU_L,STACKmc)
!poption tlocal(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,ALU_L)
          do i= iS, iE
             S1(i)  = ALU_L(i) * S1(i)
             S2(i)  = ALU_L(i) * S2(i)
             S3(i)  = ALU_L(i) * S3(i)
          end do
        enddo
!$omp end parallel do

      enddo

       end subroutine forward_substitute_3x11
!
!  ---------------------------------------------------------------------
!
      end module forward_substitute_11
