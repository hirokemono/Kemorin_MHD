!
!      module gauss_zeidel_hp_11
!
!     Written by Kemorin
!
!       subroutine gauss_zeidel_hp_11_l(iv, NP, NL, NPL, PEsmpTOT,      &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V)
!       subroutine gauss_zeidel_hp_3x11_l(iv, NP, NL, NPL,              &
!     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL,     &
!     &           S1, S2, S3, AL, V1, V2, V3)
!
!       subroutine gauss_zeidel_hp_11_u(iv, NP, N, NU, NPU, PEsmpTOT,   &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
!     $           S, AU, ALU_U, V)
!       subroutine gauss_zeidel_hp_3x11_u(iv, NP, N, NU, NPU,           &
!     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,     &
!     &           S1, S2, S3, AU, ALU_U)
!
!       ALU_U = 1 / Diag (Ordered by DJDS ordering for upper component)
!
      module gauss_zeidel_hp_11
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
       subroutine gauss_zeidel_hp_11_l(iv, NP, NL, NPL, PEsmpTOT,       &
     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT) 
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k, kk
!
!
!$omp parallel do private(iv0,iS,iE,i,j,k,kk)
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
              S(i) = S(i) - AL(k) * V(kk)
              if(S(i).ne. 0.0d0 .or.  V(kk).ne.0.0d0 ) write(*,*) 'AL', i,kk,AL(k), V(kk), S(i)
            end do
          end do
        end do
!$omp end parallel do
!

       end subroutine gauss_zeidel_hp_11_l
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_3x11_l(iv, NP, NL, NPL,               &
     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL,      &
     &           S1, S2, S3, AL, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: iv
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
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k, kk
!
!
!$omp parallel do private(iv0,iS,iE,i,j,k,kk)
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
              S1(i) = S1(i) - AL(k) * V1(kk)
              S2(i) = S2(i) - AL(k) * V2(kk)
              S3(i) = S3(i) - AL(k) * V3(kk)
            enddo
          enddo
        enddo
!$omp end parallel do
!
       end subroutine gauss_zeidel_hp_3x11_l
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_11_u(iv, NP, N, NU, NPU, PEsmpTOT,    &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           S, AU, ALU_U, V)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, N, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: V(NP)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k, kk, iv2,ivv
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk,iv2,ivv)
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
              S(i) = S(i) - AU(k) * V(kk)
              if( S(i).ne. 0.0d0 .or. V(kk).ne.0.0d0 ) write(*,*) 'AU', i,kk,AU(k)*ALU_U(i), V(kk), S(i)
            end do
          end do
!
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_U)
          do i= iS, iE
            S(i)  =  ALU_U(i) * S(i)
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
              S(i)  =  V(i)
            end do
          end do
!
        end do
!$omp end parallel do
!
       end subroutine gauss_zeidel_hp_11_u
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_3x11_u(iv, NP, N, NU, NPU,            &
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

       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k, kk, iv2, ivv
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk,iv2,ivv)
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
              S1(i) = S1(i) - AU(k) * V1(kk)
              S2(i) = S2(i) - AU(k) * V2(kk)
              S3(i) = S3(i) - AU(k) * V3(kk)
            end do
          end do
!
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,ALU_U)
          do i= iS, iE
            S1(i)  =  ALU_U(i) * S1(i)
            S2(i)  =  ALU_U(i) * S2(i)
            S3(i)  =  ALU_U(i) * S3(i)
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
              S1(i)  =  V1(i)
              S2(i)  =  V2(i)
              S3(i)  =  V3(i)
            end do
          end do
!
        end do
!$omp end parallel do
!
       end subroutine gauss_zeidel_hp_3x11_u
!
!  ---------------------------------------------------------------------
!
      end module gauss_zeidel_hp_11
