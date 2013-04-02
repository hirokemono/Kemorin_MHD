!
!      module forward_substitute_33d
!
!     Written by Kemorin
!
!       subroutine forward_substitute_1x33d(N, NP, NL, NPL, PEsmpTOT,   &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)
!
!       subroutine forward_substitute_3x33d(N, NP, NL, NPL, PEsmpTOT,   &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3,   &
!     &           AL, ALU_L)
!
      module forward_substitute_33d
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
       subroutine forward_substitute_1x33d(N, NP, NL, NPL, PEsmpTOT,    &
     &       NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)
!
       integer(kind = kint), intent(in) :: N, NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
!
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!poption noparallel
      do iv= 1, NVECT
!cdir parallel do private(iv0,j,iS,iE,i,k,kk)
!$omp parallel do private(iv0,j,iS,iE,i,k,kk)
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
              kk= IAL(k)
              S(3*i-2) = S(3*i-2) - AL(9*k-8)*S(3*kk-2)
              S(3*i-1) = S(3*i-1) - AL(9*k-4)*S(3*kk-1)
              S(3*i  ) = S(3*i  ) - AL(9*k  )*S(3*kk  )
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
            S(3*i-2) = ALU_L(9*i  )*S(3*i  )
            S(3*i-1) = ALU_L(9*i-4)*S(3*i-1)
            S(3*i  ) = ALU_L(9*i-8)*S(3*i-2)
          enddo
        enddo
!$omp end parallel do

      enddo

       end subroutine forward_substitute_1x33d
!
!  ---------------------------------------------------------------------
!
       subroutine forward_substitute_3x33d(N, NP, NL, NPL, PEsmpTOT,    &
     &       NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3,        &
     &       AL, ALU_L)
!
       integer(kind = kint), intent(in) :: N, NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
!
       real(kind = kreal), intent(in) :: AL(9*NPL)
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP), S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!poption noparallel
      do iv= 1, NVECT
!cdir parallel do private(iv0,j,iS,iE,i,k,kk)
!$omp parallel do private(iv0,j,iS,iE,i,k,kk)
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
              kk= IAL(k)
              S1(3*i-2) = S1(3*i-2) - AL(9*k-8)*S1(3*kk-2)
              S2(3*i-2) = S2(3*i-2) - AL(9*k-8)*S2(3*kk-2)
              S3(3*i-2) = S3(3*i-2) - AL(9*k-8)*S3(3*kk-2)
!
              S1(3*i-1) = S1(3*i-1) - AL(9*k-4)*S1(3*kk-1)
              S2(3*i-1) = S2(3*i-1) - AL(9*k-4)*S2(3*kk-1)
              S3(3*i-1) = S3(3*i-1) - AL(9*k-4)*S3(3*kk-1)
!
              S1(3*i  ) = S1(3*i  ) - AL(9*k  )*S1(3*kk  )
              S2(3*i  ) = S2(3*i  ) - AL(9*k  )*S2(3*kk  )
              S3(3*i  ) = S3(3*i  ) - AL(9*k  )*S3(3*kk  )
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
            S1(3*i-2) = ALU_L(9*i  )*S1(3*i  )
            S2(3*i-2) = ALU_L(9*i  )*S2(3*i  )
            S3(3*i-2) = ALU_L(9*i  )*S3(3*i  )
!
            S1(3*i-1) = ALU_L(9*i-4)*S1(3*i-1)
            S2(3*i-1) = ALU_L(9*i-4)*S2(3*i-1)
            S3(3*i-1) = ALU_L(9*i-4)*S3(3*i-1)
!
            S1(3*i  ) = ALU_L(9*i-8)*S1(3*i-2)
            S2(3*i  ) = ALU_L(9*i-8)*S2(3*i-2)
            S3(3*i  ) = ALU_L(9*i-8)*S3(3*i-2)
          enddo
        enddo
!$omp end parallel do

      enddo

       end subroutine forward_substitute_3x33d
!
!  ---------------------------------------------------------------------
!
      end module forward_substitute_33d
