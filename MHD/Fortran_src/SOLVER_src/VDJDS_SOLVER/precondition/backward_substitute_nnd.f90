!
!      module backward_substitute_nnd
!
!     Written by Kemorin
!
!       subroutine backward_substitute_1xnnd(N, NP, NB, NU, NPU,        &
!     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,     &
!     &           S, WP, AU, ALU_U)
!
!       subroutine backward_substitute_3xnnd(N, NP, NB, NU, NPU,        &
!     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,     &
!     &           S1, S2, S3, WP1, WP2, WP3, AU, ALU_U)
!
      module backward_substitute_nnd
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
       subroutine backward_substitute_1xnnd(N, NP, NB, NU, NPU,         &
     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,      &
     &           S, WP, AU, ALU_U)
!
       integer(kind = kint), intent(in) :: N, NP, NB, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N  )
       real(kind = kreal), intent(inout) :: S(NB*NP ), WP(NB*NP )
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j
       integer (kind = kint) :: k, kk, k1, im, ix, ii
!
!
      iv= NVECT
!cdir parallel do private(iS,iE,i,k1,ii,ix,im)
!$omp parallel do private(iS,iE,i,k1,ii,ix,im)
!poption indep (WP,S,ALU_U,STACKmc) tlocal (iS,iE,i,k1,ii,ix,im)
      do ip= 1, PEsmpTOT
        iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
        iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP,S,ALU_U)
          do i= iS, iE
            ii =    NB*i
            im = NB*NB*i
            WP(ii) = ALU_U(im) * WP(ii)
          end do

        do k1 = NB-1, 1, -1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP,S,ALU_U)
          do i= iS, iE
            ii =    NB*(i-1)     + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            WP(ii) = ALU_U(im) * WP(ii)
          end do
        end do

        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP,S,ALU_U)
          do i= iS, iE
            ii =    NB*(i-1)     + k1
            S(ii)  =  S(ii) - WP(ii)
          end do
        end do

      enddo
!$omp end parallel do

!poption noparallel      
      do iv= NVECT-1, 1, -1
!cdir parallel do private(iv0,iS,iE,i,j,k,k1,ii,ix,im)
!$omp parallel do private(iv0,iS,iE,i,j,k,k1,ii,ix,im)
!poption indep (WP,S,AU,INU,IAU,STACKmc)
!poption tlocal(iv0,iS,iE,i,k,k1,k2,ii,ix,im)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
            do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP,S,AU,IAU)
                do i= iv0+1, iv0+iE-iS
                  k= i+iS - iv0
                  ii =    NB*(  i-1   )     + k1
                  ix =    NB*(IAU(k)-1)             + k1
                  im = NB*NB*(  k-1   ) + NB*(k1-1) + k1
                  WP(ii) = WP(ii) + AU(im)*S(ix)
                enddo

            end do

          enddo
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i,k1,ii,ix,im)
!$omp parallel do private(iS,iE,i,k1,ii,ix,im)
!poption indep (WP,S,ALU_U,STACKmc) tlocal (iS,iE,i,k1,ii,ix,im)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )

!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP,S,ALU_U)
!          do i= iS, iE
!            WP(ii+1) = WP(ii+1)
!          end do

          do k1 = NB, 1, -1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP,S,ALU_U)
            do i= iS, iE
              ii =    NB*(i-1)     + k1
              im = NB*NB*(i-1) + NB*(k1-1) + k1
              WP(ii) = ALU_U(im) * WP(ii)
              S(ii)  =  S(ii) - WP(ii)
            end do
          end do

        enddo
!$omp end parallel do
      enddo

      end subroutine backward_substitute_1xnnd
!
!  ---------------------------------------------------------------------
!
       subroutine backward_substitute_3xnnd(N, NP, NB, NU, NPU,         &
     &           PEsmpTOT, NVECT, npUX1, STACKmc, NUhyp, INU, IAU,      &
     &           S1, S2, S3, WP1, WP2, WP3, AU, ALU_U)
!
       integer(kind = kint), intent(in) :: N, NP, NB, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: ALU_U(NB*NB*N  )
       real(kind = kreal), intent(inout) :: S1(NB*NP ), WP1(NB*NP )
       real(kind = kreal), intent(inout) :: S2(NB*NP ), WP2(NB*NP )
       real(kind = kreal), intent(inout) :: S3(NB*NP ), WP3(NB*NP )
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j
       integer (kind = kint) :: k, kk, k1, im, ix, ii
!
!
      iv= NVECT
!cdir parallel do private(iS,iE,i,k1,ii,ix,im)
!$omp parallel do private(iS,iE,i,k1,ii,ix,im)
!poption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U,STACKmc)
!poption tlocal (iS,iE,i,k1,ii,ix,im)
      do ip= 1, PEsmpTOT
        iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
        iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP1,WP2,WP3,ALU_U)
          do i= iS, iE
            ii =    NB*i
            im = NB*NB*i
            WP1(ii) = ALU_U(im) * WP1(ii)
            WP2(ii) = ALU_U(im) * WP2(ii)
            WP3(ii) = ALU_U(im) * WP3(ii)
          end do

        do k1 = NB-1, 1, -1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP1,WP2,WP3,ALU_U)
          do i= iS, iE
            ii =    NB*(i-1)     + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            WP1(ii) = ALU_U(im) * WP1(ii)
            WP2(ii) = ALU_U(im) * WP2(ii)
            WP3(ii) = ALU_U(im) * WP3(ii)
          end do
        end do

        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U)
          do i= iS, iE
            ii =    NB*(i-1)     + k1
            S1(ii)  =  S1(ii) - WP1(ii)
            S2(ii)  =  S2(ii) - WP2(ii)
            S3(ii)  =  S3(ii) - WP3(ii)
          end do
        end do

      enddo
!$omp end parallel do

!poption noparallel      
      do iv= NVECT-1, 1, -1
!cdir parallel do private(iv0,iS,iE,i,j,k,k1,ii,ix,im)
!$omp parallel do private(iv0,iS,iE,i,j,k,k1,ii,ix,im)
!poption indep (WP1,WP2,WP3,S1,S2,S3,AU,INU,IAU,STACKmc)
!poption tlocal(iv0,iS,iE,i,k,k1,k2,ii,ix,im)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
            do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP1,WP2,WP3,S1,S2,S3,AU,IAU)
                do i= iv0+1, iv0+iE-iS
                  k= i+iS - iv0
                  ii =    NB*(  i-1   )     + k1
                  ix =    NB*(IAU(k)-1)             + k1
                  im = NB*NB*(  k-1   ) + NB*(k1-1) + k1
                  WP1(ii) = WP1(ii) + AU(im)*S1(ix)
                  WP2(ii) = WP2(ii) + AU(im)*S2(ix)
                  WP3(ii) = WP3(ii) + AU(im)*S3(ix)
                enddo

            end do

          enddo
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i,k1,ii,ix,im)
!$omp parallel do private(iS,iE,i,k1,ii,ix,im)
!poption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U,STACKmc)
!poption tlocal (iS,iE,i,k1,ii,ix,im)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )

!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP1,WP2,WP3,ALU_U)
!          do i= iS, iE
!            WP1(ii+1) = WP1(ii+1)
!            WP2(ii+1) = WP2(ii+1)
!            WP3(ii+1) = WP3(ii+1)
!          end do

          do k1 = NB, 1, -1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP1,WP2,WP3,ALU_U)
            do i= iS, iE
              ii =    NB*(i-1)     + k1
              im = NB*NB*(i-1) + NB*(k1-1) + k1
              WP1(ii) = ALU_U(im) * WP1(ii)
              WP2(ii) = ALU_U(im) * WP2(ii)
              WP3(ii) = ALU_U(im) * WP3(ii)
              S1(ii)  =  S1(ii) - WP1(ii)
              S2(ii)  =  S2(ii) - WP2(ii)
              S3(ii)  =  S3(ii) - WP3(ii)
            end do
          end do

        enddo
!$omp end parallel do
      enddo

      end subroutine backward_substitute_3xnnd
!
!  ---------------------------------------------------------------------
!
      end module backward_substitute_nnd
