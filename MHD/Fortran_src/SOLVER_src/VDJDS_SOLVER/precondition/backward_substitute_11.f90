!
!      module backward_substitute_11
!
!     Written by Kemorin
!
!       subroutine backward_substitute_1x11(N, NP, NU, NPU, PEsmpTOT,   &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
!     &           S, WP, AU, ALU_U)
!
!       subroutine backward_substitute_3x11(N, NP, NU, NPU, PEsmpTOT,   &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
!     &           S1, S2, S3, WP1, WP2, WP3, AU, ALU_U)
!
      module backward_substitute_11
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
       subroutine backward_substitute_1x11(N, NP, NU, NPU, PEsmpTOT,    &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           S, WP, AU, ALU_U)
!
       integer(kind = kint), intent(in) :: N, NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(inout) :: S(NP), WP(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
      iv= NVECT
!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (WP,S,ALU_U,STACKmc) tlocal (iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
        iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP,S,ALU_U)
        do i= iS, iE
          S(i)  =  S(i) - ALU_U(i) * WP(i)
        end do
      enddo
!$omp end parallel do

!poption noparallel      
      do iv= NVECT-1, 1, -1
!cdir parallel do private(iv0,iS,iE,i,j,k,kk)
!$omp parallel do private(iv0,iS,iE,i,j,k,kk)
!poption indep (WP,S,AU,INU,IAU,STACKmc)
!poption tlocal(iv0,iS,iE,i,k,kk)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP,S,AU,IAU)
            do i= iv0+1, iv0+iE-iS
                   k = i+iS - iv0
                  kk = IAU(k)
               WP(i) = WP(i) + AU(k)*S(kk)
            enddo
          enddo
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (WP,S,ALU_U,STACKmc) tlocal (iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP,S,ALU_U)
          do i= iS, iE
            S(i)  =  S(i) - ALU_U(i) * WP(i)
          end do

        enddo
!$omp end parallel do
      enddo

      end subroutine backward_substitute_1x11
!
!  ---------------------------------------------------------------------
!
       subroutine backward_substitute_3x11(N, NP, NU, NPU, PEsmpTOT,    &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           S1, S2, S3, WP1, WP2, WP3, AU, ALU_U)
!
       integer(kind = kint), intent(in) :: N, NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: ALU_U(N)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
       real(kind = kreal), intent(inout) :: WP1(NP), WP2(NP), WP3(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
      iv= NVECT
!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U,STACKmc) tlocal (iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
        iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U)
        do i= iS, iE
          S1(i)  =  S1(i) - ALU_U(i) * WP1(i)
          S2(i)  =  S2(i) - ALU_U(i) * WP2(i)
          S3(i)  =  S3(i) - ALU_U(i) * WP3(i)
        end do
      enddo
!$omp end parallel do

!poption noparallel      
      do iv= NVECT-1, 1, -1
!cdir parallel do private(iv0,iS,iE,i,j,k,kk)
!$omp parallel do private(iv0,iS,iE,i,j,k,kk)
!poption indep (WP1,WP2,WP3,S1,S2,S3,AU,INU,IAU,STACKmc)
!poption tlocal(iv0,iS,iE,i,k,kk)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP1,WP2,WP3,S1,S2,S3,AU,IAU)
            do i= iv0+1, iv0+iE-iS
                   k = i+iS - iv0
                  kk = IAU(k)
               WP1(i) = WP1(i) + AU(k)*S1(kk)
               WP2(i) = WP2(i) + AU(k)*S2(kk)
               WP3(i) = WP3(i) + AU(k)*S3(kk)
            enddo
          enddo
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U,STACKmc) tlocal (iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U)
          do i= iS, iE
            S1(i)  =  S1(i) - ALU_U(i) * WP1(i)
            S2(i)  =  S2(i) - ALU_U(i) * WP2(i)
            S3(i)  =  S3(i) - ALU_U(i) * WP3(i)
          end do

        enddo
!$omp end parallel do
      enddo

      end subroutine backward_substitute_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module backward_substitute_11
