!
!      module cal_4_upper_11
!
!     Written by Kemorin
!
!       subroutine subtract_upper_11(NP, NU, NPU, PEsmpTOT, NVECT,      &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!       subroutine subtract_upper_3x11(NP, NU, NPU, PEsmpTOT, NVECT,    &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,      &
!     &           V1, V2, V3 )
!
!       subroutine add_upper_11(NP, NU, NPU, PEsmpTOT, NVECT,           &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!       subroutine add_upper_3x11(NP, NU, NPU, PEsmpTOT, NVECT,         &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,      &
!     &           V1, V2, V3 )
!
      module cal_4_upper_11
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
       subroutine subtract_upper_11(NP, NU, NPU, PEsmpTOT, NVECT,       &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!
       integer(kind = kint), intent(in) :: NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk)
      do ip= 1, PEsmpTOT
        do iv= 1, NVECT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAU(k)
              S(i) = S(i) - AU(k) * V(kk)
            enddo
          enddo
        enddo
      enddo
!$omp end parallel do

       end subroutine subtract_upper_11
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_upper_3x11(NP, NU, NPU, PEsmpTOT, NVECT,     &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk)
      do ip= 1, PEsmpTOT
        do iv= 1, NVECT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAU(k)
              S1(i) = S1(i) - AU(k) * V1(kk)
              S2(i) = S2(i) - AU(k) * V2(kk)
              S3(i) = S3(i) - AU(k) * V3(kk)
            enddo
          enddo
        enddo
      enddo
!$omp end parallel do

       end subroutine subtract_upper_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_upper_11(NP, NU, NPU, PEsmpTOT, NVECT,            &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!
       integer(kind = kint), intent(in) :: NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk) 
      do ip= 1, PEsmpTOT
        do iv= 1, NVECT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAU(k)
              S(i) = S(i) + AU(k) * V(kk)
            enddo
          enddo
        enddo
      enddo
!$omp end parallel do
!
       end subroutine add_upper_11
!
!  ---------------------------------------------------------------------
!
       subroutine add_upper_3x11(NP, NU, NPU, PEsmpTOT, NVECT,          &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk) 
      do ip= 1, PEsmpTOT
        do iv= 1, NVECT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              kk = IAU(k)
              S1(i) = S1(i) + AU(k) * V1(kk)
              S2(i) = S2(i) + AU(k) * V2(kk)
              S3(i) = S3(i) + AU(k) * V3(kk)
            enddo
          enddo
        enddo
      enddo
!$omp end parallel do
!
       end subroutine add_upper_3x11
!
!  ---------------------------------------------------------------------
!
      end module cal_4_upper_11
