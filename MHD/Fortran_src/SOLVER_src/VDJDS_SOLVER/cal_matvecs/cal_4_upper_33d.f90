!
!      module cal_4_upper_33d
!
!     Written by Kemorin
!
!       subroutine subtract_upper_33d(NP, NU, NPU, PEsmpTOT, NVECT,     &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!       subroutine subtract_upper_3x33d(NP, NU, NPU, PEsmpTOT, NVECT,   &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,      &
!     &           V1, V2, V3 )
!
!       subroutine add_upper_33d(NP, NU, NPU, PEsmpTOT, NVECT,          &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!       subroutine add_upper_3x33d(NP, NU, NPU, PEsmpTOT, NVECT,        &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,      &
!     &           V1, V2, V3 )
!
      module cal_4_upper_33d
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
       subroutine subtract_upper_33d(NP, NU, NPU, PEsmpTOT, NVECT,      &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!
       integer(kind = kint), intent(in) :: NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk) 
      do ip= 1, PEsmpTOT
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
        do  j= 1, NUhyp(iv)
          iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
          iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!voption indep (S,V,IAU,AU)
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
          do i= iv0+1, iv0+iE-iS
              k = i+iS - iv0
             kk = IAU(k)
            S(3*i-2) = S(3*i-2) - AU(9*k-8)*V(3*kk-2)
            S(3*i-1) = S(3*i-1) - AU(9*k-4)*V(3*kk-1)
            S(3*i  ) = S(3*i  ) - AU(9*k  )*V(3*kk  )
          enddo
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine subtract_upper_33d
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_upper_3x33d(NP, NU, NPU, PEsmpTOT, NVECT,    &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk) 
      do ip= 1, PEsmpTOT
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
        do  j= 1, NUhyp(iv)
          iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
          iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!voption indep (S1,S2,S3,V1,V2,V3,IAU,AU)
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep
          do i= iv0+1, iv0+iE-iS
              k = i+iS - iv0
             kk = IAU(k)
            S1(3*i-2) = S1(3*i-2) - AU(9*k-8)*V1(3*kk-2)
            S2(3*i-2) = S2(3*i-2) - AU(9*k-8)*V2(3*kk-2)
            S3(3*i-2) = S3(3*i-2) - AU(9*k-8)*V3(3*kk-2)
            S1(3*i-1) = S1(3*i-1) - AU(9*k-4)*V1(3*kk-1)
            S2(3*i-1) = S2(3*i-1) - AU(9*k-4)*V2(3*kk-1)
            S3(3*i-1) = S3(3*i-1) - AU(9*k-4)*V3(3*kk-1)
            S1(3*i  ) = S1(3*i  ) - AU(9*k  )*V1(3*kk  )
            S2(3*i  ) = S2(3*i  ) - AU(9*k  )*V2(3*kk  )
            S3(3*i  ) = S3(3*i  ) - AU(9*k  )*V3(3*kk  )
          enddo
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine subtract_upper_3x33d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_upper_33d(NP, NU, NPU, PEsmpTOT, NVECT,           &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!
       integer(kind = kint), intent(in) :: NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!$omp parallel do private(iv0,j,iS,iE,i,k,kk)
      do ip= 1, PEsmpTOT
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
        do  j= 1, NUhyp(iv)
          iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
          iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!voption indep (S,V,IAU,AU)
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
          do i= iv0+1, iv0+iE-iS
              k = i+iS - iv0
             kk = IAU(k)
            S(3*i-2) = S(3*i-2) + AU(9*k-8)*V(3*kk-2)
            S(3*i-1) = S(3*i-1) + AU(9*k-4)*V(3*kk-1)
            S(3*i  ) = S(3*i  ) + AU(9*k  )*V(3*kk  )
          enddo
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine add_upper_33d
!
!  ---------------------------------------------------------------------
!
       subroutine add_upper_3x33d(NP, NU, NPU, PEsmpTOT, NVECT,         &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(in) :: AU(9*NPU)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
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
!voption indep (S1,S2,S3,V1,V2,V3,IAU,AU)
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep
          do i= iv0+1, iv0+iE-iS
              k = i+iS - iv0
             kk = IAU(k)
            S1(3*i-2) = S1(3*i-2) + AU(9*k-8)*V1(3*kk-2)
            S2(3*i-2) = S2(3*i-2) + AU(9*k-8)*V2(3*kk-2)
            S3(3*i-2) = S3(3*i-2) + AU(9*k-8)*V3(3*kk-2)
            S1(3*i-1) = S1(3*i-1) + AU(9*k-4)*V1(3*kk-1)
            S2(3*i-1) = S2(3*i-1) + AU(9*k-4)*V2(3*kk-1)
            S3(3*i-1) = S3(3*i-1) + AU(9*k-4)*V3(3*kk-1)
            S1(3*i  ) = S1(3*i  ) + AU(9*k  )*V1(3*kk  )
            S2(3*i  ) = S2(3*i  ) + AU(9*k  )*V2(3*kk  )
            S3(3*i  ) = S3(3*i  ) + AU(9*k  )*V3(3*kk  )
          enddo
        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine add_upper_3x33d
!
!  ---------------------------------------------------------------------
!
      end module cal_4_upper_33d
