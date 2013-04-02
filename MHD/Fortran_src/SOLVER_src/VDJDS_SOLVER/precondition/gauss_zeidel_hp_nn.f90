!
!      module gauss_zeidel_hp_nn
!
!     Written by Kemorin
!
!       subroutine gauss_zeidel_hp_nn_l(iv, NP, NB, NL, NPL, PEsmpTOT,  &
!     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V)
!       subroutine gauss_zeidel_hp_3xnn_l(iv, NP, NB, NL, NPL,          &
!     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL,     &
!     &           S1, S2, S3, AL, V1, V2, V3)
!
!       subroutine gauss_zeidel_hp_nn_u(iv, NP, N, NB, NU, NPU,         &
!     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,     &
!     &           S, AU, ALU_U, V)
!       subroutine gauss_zeidel_hp_3xnn_u(iv, NP, N, NB, NU, NPU,       &
!     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,     &
!     &           S1, S2, S3, AU, ALU_U, V1, V2, V3)
!
!        at (NB*NB*i-8, NB*NB*i-4, NB*NB*i) 
!       ALU_U = 1 / Diag (Ordered by DJDS ordering for upper component)
!        else
!       ALU_U = Diag (Ordered by DJDS ordering for upper component)
!
      module gauss_zeidel_hp_nn
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
       subroutine gauss_zeidel_hp_nn_l(iv, NP, NB, NL, NPL, PEsmpTOT,   &
     &           NVECT, npLX1, STACKmc, NLhyp, INL, IAL, S, AL, V)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, NB, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT) 
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k
       integer (kind = kint) :: ii, ix, im, k1, k2
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,ii,ix,im,k1,k2)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
            do k1 = 1, NB
              do k2 = 1, NB
!cdir nodep
!voption indep (S,AL,IAL)
                do i= iv0+1, iv0+iE-iS
                  k= i+iS - iv0
                  ii =    NB*( i - 1  ) + k1
                  ix =    NB*(IAL(k)-1)             + k2
                  im = NB*NB*( k - 1  ) + NB*(k1-1) + k2
                  S(ii) = S(ii) - AL(im) * V(ix)
                end do
              end do
            end do
          end do
        end do
!$omp end parallel do

       end subroutine gauss_zeidel_hp_nn_l
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_3xnn_l(iv, NP, NB, NL, NPL,           &
     &           PEsmpTOT, NVECT, npLX1, STACKmc, NLhyp, INL, IAL,      &
     &           S1, S2, S3, AL, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, NB, NL, NPL
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npLX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NLhyp(NVECT) 
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)

       real(kind = kreal), intent(in) :: AL(NB*NB*NPL)
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
!
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k
       integer (kind = kint) :: ii, ix, im, k1, k2
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,ii,ix,im,k1,k2)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j  )
            do k1 = 1, NB
              do k2 = 1, NB
!cdir nodep
!voption indep (S1,S2,S3,AL,IAL)
                do i= iv0+1, iv0+iE-iS
                  k= i+iS - iv0
                  ii =    NB*( i - 1  ) + k1
                  ix =    NB*(IAL(k)-1)             + k2
                  im = NB*NB*( k - 1  ) + NB*(k1-1) + k2
                  S1(ii) = S1(ii) - AL(im) * V1(ix)
                  S2(ii) = S2(ii) - AL(im) * V2(ix)
                  S3(ii) = S3(ii) - AL(im) * V3(ix)
                end do
              end do
            end do
          end do
        end do
!$omp end parallel do
!
       end subroutine gauss_zeidel_hp_3xnn_l
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_nn_u(iv, NP, N, NB, NU, NPU,          &
     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,      &
     &           S, AU, ALU_U, V)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, N, NB, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k
       integer (kind = kint) :: ii, ix, im, k1, k2, kk, iv2, ivv
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,ii,ix,im,k1,k2,kk,iv2,ivv)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
            do k1 = 1, NB
              do k2 = 1, NB
!cdir nodep
!voption indep (S,AL,IAL)
                do i= iv0+1, iv0+iE-iS
                  k= i+iS - iv0
                  ii =    NB*( i - 1  ) + k1
                  ix =    NB*(IAU(k)-1)             + k2
                  im = NB*NB*( k - 1  ) + NB*(k1-1) + k2
                  S(ii) = S(ii) - AU(im) * V(ix)
                end do
              end do
            end do
          end do
!
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
          do k1 = 1, NB
            do k2 = 1, NB-1
              kk = mod(k1+k2-1,NB) + 1
!cdir nodep noloopchg
!voption indep (S,ALU_U)
              do i= iS, iE
                ii =    NB*( i - 1  ) + k1
                ix =    NB*( i - 1  )             + kk
                im = NB*NB*( i - 1  ) + NB*(k1-1) + kk
                S(ii)  = S(ii) - ALU_U(im) * S(ix)
              end do
            end do
!
            do i= iS, iE
              ii =    NB*( i - 1  ) + k1
              im = NB*NB*( i - 1  ) + NB*(k1-1) + k1
              S(ii)  = ALU_U(im) * S(ii)
            end do
          end do
!
          do k1 = 1, NB
            do iv2 = 1, NVECT-1
              ivv = mod(iv2+iv-1,NVECT) + 1
              iS= STACKmc(PEsmpTOT*(ivv-1)+ip-1) + 1
              iE= STACKmc(PEsmpTOT*(ivv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_U)
              do i= iS, iE
                ii =    NB*( i - 1  ) + k1
                S(ii)  =  V(ii)
              end do
            end do
          end do
!
        end do
!$omp end parallel do

       end subroutine gauss_zeidel_hp_nn_u
!
!  ---------------------------------------------------------------------
!
       subroutine gauss_zeidel_hp_3xnn_u(iv, NP, N, NB, NU, NPU,        &
     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,      &
     &           S1, S2, S3, AU, ALU_U, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: iv
       integer(kind = kint), intent(in) :: NP, N, NB, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N)
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
!
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, i, j, k
       integer (kind = kint) :: ii, ix, im, k1, k2, kk, iv2, ivv
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,ii,ix,im,k1,k2,kk,iv2,ivv)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip-1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
            do k1 = 1, NB
              do k2 = 1, NB
!cdir nodep
!voption indep (S1,S2,S3,AL,IAL)
                do i= iv0+1, iv0+iE-iS
                  k= i+iS - iv0
                  ii =    NB*( i - 1  ) + k1
                  ix =    NB*(IAU(k)-1)             + k2
                  im = NB*NB*( k - 1  ) + NB*(k1-1) + k2
                  S1(ii) = S1(ii) - AU(im) * V1(ix)
                  S2(ii) = S2(ii) - AU(im) * V2(ix)
                  S3(ii) = S3(ii) - AU(im) * V3(ix)
                end do
              end do
            end do
          end do
!
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
          do k1 = 1, NB
            do k2 = 1, NB-1
              kk = mod(k1+k2-1,NB) + 1
!cdir nodep noloopchg
!voption indep (S1,S2,S3,ALU_U)
              do i= iS, iE
                ii =    NB*( i - 1  ) + k1
                ix =    NB*( i - 1  )             + kk
                im = NB*NB*( i - 1  ) + NB*(k1-1) + kk
                S1(ii)  = S1(ii) - ALU_U(im) * S1(ix)
                S2(ii)  = S2(ii) - ALU_U(im) * S2(ix)
                S3(ii)  = S3(ii) - ALU_U(im) * S3(ix)
              end do
            end do
!
            do i= iS, iE
              ii =    NB*( i - 1  ) + k1
              im = NB*NB*( i - 1  ) + NB*(k1-1) + k1
              S1(ii)  = ALU_U(im) * S1(ii)
              S2(ii)  = ALU_U(im) * S3(ii)
              S3(ii)  = ALU_U(im) * S2(ii)
            end do
          end do
!
          do k1 = 1, NB
            do iv2 = 1, NVECT-1
              ivv = mod(iv2+iv-1,NVECT) + 1
              iS= STACKmc(PEsmpTOT*(ivv-1)+ip-1) + 1
              iE= STACKmc(PEsmpTOT*(ivv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_U)
              do i= iS, iE
                ii =    NB*( i - 1  ) + k1
                S1(ii)  =  V1(ii)
                S2(ii)  =  V2(ii)
                S3(ii)  =  V3(ii)
              end do
            end do
          end do
!
        end do
!$omp end parallel do

       end subroutine gauss_zeidel_hp_3xnn_u
!
!  ---------------------------------------------------------------------
!
      end module gauss_zeidel_hp_nn
