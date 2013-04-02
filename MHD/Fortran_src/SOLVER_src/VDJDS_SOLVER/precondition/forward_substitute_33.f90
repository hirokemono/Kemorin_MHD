!
!      module forward_substitute_33
!
!     Written by Kemorin
!
!       subroutine forward_substitute_1x33(N, NP, NL, NPL, PEsmpTOT,    &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, ALU_L)
!
!       subroutine forward_substitute_3x33(N, NP, NL, NPL, PEsmpTOT,    &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S1, S2, S3,   &
!     &           AL, ALU_L)
!
      module forward_substitute_33
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
       subroutine forward_substitute_1x33(N, NP, NL, NPL, PEsmpTOT,     &
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
       real(kind = kreal) :: X1, X2, X3, Zm0, Zm1, Zm2
!
!
!poption noparallel
      do iv= 1, NVECT
!$omp parallel do private(iv0,j,iS,iE,i,k,kk,Zm0,Zm1,Zm2)
!poption indep (S,AL,INL,IAL,STACKmc,Zm0,Zm1,Zm2)
!poption tlocal(iv0,j,iS,iE,i,k,kk)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
!cdir nodep
!voption indep (S,AL,IAL)
            do i= iv0+1, iv0+iE-iS
               k= i+iS - iv0
              kk= IAL(k)
              Zm2= S(3*kk-2)
              Zm1= S(3*kk-1)
              Zm0= S(3*kk  )
              S(3*i-2) = S(3*i-2)                                       &
     &                - AL(9*k-8)*Zm2 - AL(9*k-7)*Zm1 - AL(9*k-6)*Zm0
              S(3*i-1) = S(3*i-1)                                       &
     &                - AL(9*k-5)*Zm2 - AL(9*k-4)*Zm1 - AL(9*k-3)*Zm0
              S(3*i  ) = S(3*i  )                                       &
     &                - AL(9*k-2)*Zm2 - AL(9*k-1)*Zm1 - AL(9*k  )*Zm0
            enddo
          enddo
        enddo
!$omp end parallel do

!$omp parallel do private(iS,iE,i,X1,X2,X3)
!poption indep (S,ALU_L,STACKmc,X1,X2,X3)
!poption tlocal(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!cdir nodep
!voption indep (S,ALU_L)
          do i= iS, iE
            X1= S(3*i-2)
            X2= S(3*i-1)
            X3= S(3*i  )
            X2= X2 - ALU_L(9*i-5)*X1
            X3= X3 - ALU_L(9*i-2)*X1 - ALU_L(9*i-1)*X2
            X3= ALU_L(9*i  )*  X3
            X2= ALU_L(9*i-4)*( X2 - ALU_L(9*i-3)*X3 )
            X1= ALU_L(9*i-8)*( X1 - ALU_L(9*i-6)*X3 - ALU_L(9*i-7)*X2)
            S(3*i-2) = X1
            S(3*i-1) = X2
            S(3*i  ) = X3
          enddo
        enddo
!$omp end parallel do

      enddo

       end subroutine forward_substitute_1x33
!
!  ---------------------------------------------------------------------
!
       subroutine forward_substitute_3x33(N, NP, NL, NPL, PEsmpTOT,     &
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
       real(kind = kreal) :: X11, X12, X13, Zm10, Zm11, Zm12
       real(kind = kreal) :: X21, X22, X23, Zm20, Zm21, Zm22
       real(kind = kreal) :: X31, X32, X33, Zm30, Zm31, Zm32
!
!
!poption noparallel
      do iv= 1, NVECT
!$omp parallel do  &
!$omp& private(iv0,j,iS,iE,i,k,kk) &
!$omp& private(Zm10,Zm11,Zm12,Zm20,Zm21,Zm22,Zm30,Zm31,Zm32)
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
              Zm12= S1(3*kk-2)
              Zm11= S1(3*kk-1)
              Zm10= S1(3*kk  )
              Zm22= S2(3*kk-2)
              Zm21= S2(3*kk-1)
              Zm20= S2(3*kk  )
              Zm32= S3(3*kk-2)
              Zm31= S3(3*kk-1)
              Zm30= S3(3*kk  )
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
            enddo
          enddo
        enddo
!$omp end parallel do

!$omp parallel do private(iS,iE,i,X11,X12,X13,X21,X22,X23,X31,X32,X33)
!poption indep (S1,S2,S3,ALU_L,STACKmc)
!poption indep (X11,X12,X13,X21,X22,X23,X31,X32,X33)
!poption tlocal(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,ALU_L)
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
            X12= X12 - ALU_L(9*i-5)*X11
            X22= X22 - ALU_L(9*i-5)*X21
            X32= X32 - ALU_L(9*i-5)*X31
!
            X13= X13 - ALU_L(9*i-2)*X11 - ALU_L(9*i-1)*X12
            X23= X23 - ALU_L(9*i-2)*X21 - ALU_L(9*i-1)*X22
            X33= X33 - ALU_L(9*i-2)*X31 - ALU_L(9*i-1)*X32
!
            X13= ALU_L(9*i  )*  X13
            X23= ALU_L(9*i  )*  X23
            X33= ALU_L(9*i  )*  X33
!
            X12= ALU_L(9*i-4)*( X12 - ALU_L(9*i-3)*X13 )
            X22= ALU_L(9*i-4)*( X22 - ALU_L(9*i-3)*X23 )
            X32= ALU_L(9*i-4)*( X32 - ALU_L(9*i-3)*X33 )
!
            X11= ALU_L(9*i-8)                                           &
     &          *( X11 - ALU_L(9*i-6)*X13 - ALU_L(9*i-7)*X12)
            X21= ALU_L(9*i-8)                                           &
     &          *( X21 - ALU_L(9*i-6)*X23 - ALU_L(9*i-7)*X22)
            X31= ALU_L(9*i-8)                                           &
     &          *( X31 - ALU_L(9*i-6)*X33 - ALU_L(9*i-7)*X32)
!
            S1(3*i-2) = X11
            S1(3*i-1) = X12
            S1(3*i  ) = X13
            S2(3*i-2) = X21
            S2(3*i-1) = X22
            S2(3*i  ) = X23
            S3(3*i-2) = X31
            S3(3*i-1) = X32
            S3(3*i  ) = X33
!
          enddo
        enddo
!$omp end parallel do

      enddo

       end subroutine forward_substitute_3x33
!
!  ---------------------------------------------------------------------
!
      end module forward_substitute_33
