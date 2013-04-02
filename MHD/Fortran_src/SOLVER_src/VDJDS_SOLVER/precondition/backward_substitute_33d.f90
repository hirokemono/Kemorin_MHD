!
!      module backward_substitute_33d
!
!     Written by Kemorin
!
!       subroutine backward_substitute_1x33d(N, NP, NU, NPU, PEsmpTOT,  &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
!     &           S, WP, AU, ALU_U)
!
!       subroutine backward_substitute_3x33d(N, NP, NU, NPU, PEsmpTOT,  &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
!     &           S1, S2, S3, WP1, WP2, WP3, AU, ALU_U)
!
      module backward_substitute_33d
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
       subroutine backward_substitute_1x33d(N, NP, NU, NPU, PEsmpTOT,   &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           S, WP, AU, ALU_U)
!
       integer(kind = kint), intent(in) :: N, NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp (NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: AU(9*NPU), ALU_U(9*N)
       real(kind = kreal), intent(inout) :: S(3*NP), WP(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
      iv= NVECT
!!cdir parallel do private(iS,iE,i)
!!$omp parallel do private(iS,iE,i)
!!poption indep (S,ALU_U,STACKmc) tlocal (iS,iE,i)
!!      do ip= 1, PEsmpTOT
!!        iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
!!        iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!!OCL VECTOR, NOVREC
!!cdir nodep
!!voption indep (S,ALU_U)
!!        do i= iS, iE
!!          S(3*i-2)=  S(3*i-2)
!!          S(3*i-1)=  S(3*i-1)
!!          S(3*i  )=  S(3*i  )
!!        enddo
!!      enddo
!!!$omp end parallel do

!poption noparallel
      do iv= NVECT-1, 1, -1
!cdir parallel do private(iv0,iS,iE,i)
!$omp parallel do private(iv0,iS,iE,i)
!poption indep (WP,S,AU,INU,IAU,STACKmc)
!poption tlocal(iv0,iS,iE,i)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP,S,AU,IAU)
            do i= iv0+1, iv0+iE-iS
               k= i+iS - iv0
              kk= IAU(k)
              WP(3*i-2) = WP(3*i-2) + AU(9*k-8)*S(3*kk-2)
              WP(3*i-1) = WP(3*i-1) + AU(9*k-4)*S(3*kk-1)
              WP(3*i  ) = WP(3*i  ) + AU(9*k  )*S(3*kk  )
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
!cdir nodep
!voption indep (WP,S,ALU_U)
          do i= iS, iE
            S(3*i-2)=  S(3*i-2) - ALU_U(9*i-8)*WP(3*i-2)
            S(3*i-1)=  S(3*i-1) - ALU_U(9*i-4)*WP(3*i-1)
            S(3*i  )=  S(3*i  ) - ALU_U(9*i  )*WP(3*i  )
          enddo
        enddo
!$omp end parallel do
      enddo

       end subroutine backward_substitute_1x33d
!
!  ---------------------------------------------------------------------
!
       subroutine backward_substitute_3x33d(N, NP, NU, NPU, PEsmpTOT,   &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           S1, S2, S3, WP1, WP2, WP3, AU, ALU_U)
!
       integer(kind = kint), intent(in) :: N, NP, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp (NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: AU(9*NPU), ALU_U(9*N)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
       real(kind = kreal), intent(inout) :: WP1(3*NP), WP2(3*NP)
       real(kind = kreal), intent(inout) :: WP3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
      iv= NVECT
!!cdir parallel do private(iS,iE,i)
!!$omp parallel do private(iS,iE,i)
!!poption indep (S1,S2,S3,ALU_U,STACKmc) tlocal (iS,iE,i)
!!      do ip= 1, PEsmpTOT
!!        iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
!!        iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!!OCL VECTOR, NOVREC
!!cdir nodep
!!voption indep (S1,S2,S3,ALU_U)
!!        do i= iS, iE
!!          S1(3*i-2)=  S1(3*i-2)
!!          S1(3*i-1)=  S1(3*i-1)
!!          S1(3*i  )=  S1(3*i  )
!!          S2(3*i-2)=  S2(3*i-2)
!!          S2(3*i-1)=  S2(3*i-1)
!!          S2(3*i  )=  S2(3*i  )
!!          S3(3*i-2)=  S3(3*i-2)
!!          S3(3*i-1)=  S3(3*i-1)
!!          S3(3*i  )=  S3(3*i  )
!!        enddo
!!      enddo
!!!$omp end parallel do

!poption noparallel
      do iv= NVECT-1, 1, -1
!cdir parallel do private(iv0,iS,iE,i)
!$omp parallel do private(iv0,iS,iE,i)
!poption indep (WP1,WP2,WP3,S1,S2,S3,AU,INU,IAU,STACKmc)
!poption tlocal(iv0,iS,iE,i)
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP1,WP2,WP3,S1,S2,S3,AU,IAU)
            do i= iv0+1, iv0+iE-iS
               k= i+iS - iv0
              kk= IAU(k)
!
              WP1(3*i-2) = WP1(3*i-2) + AU(9*k-8)*S1(3*kk-2)
              WP2(3*i-2) = WP2(3*i-2) + AU(9*k-8)*S2(3*kk-2)
              WP3(3*i-2) = WP3(3*i-2) + AU(9*k-8)*S3(3*kk-2)
!
              WP1(3*i-1) = WP1(3*i-1) + AU(9*k-4)*S1(3*kk-1)
              WP2(3*i-1) = WP2(3*i-1) + AU(9*k-4)*S2(3*kk-1)
              WP3(3*i-1) = WP3(3*i-1) + AU(9*k-4)*S3(3*kk-1)
!
              WP1(3*i  ) = WP1(3*i  ) + AU(9*k  )*S1(3*kk  )
              WP2(3*i  ) = WP2(3*i  ) + AU(9*k  )*S2(3*kk  )
              WP3(3*i  ) = WP3(3*i  ) + AU(9*k  )*S3(3*kk  )
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
!cdir nodep
!voption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U)
          do i= iS, iE
            S1(3*i-2)=  S1(3*i-2) - ALU_U(9*i-8)*WP1(3*i-2)
            S2(3*i-2)=  S2(3*i-2) - ALU_U(9*i-8)*WP2(3*i-2)
            S3(3*i-2)=  S3(3*i-2) - ALU_U(9*i-8)*WP3(3*i-2)
!
            S1(3*i-1)=  S1(3*i-1) - ALU_U(9*i-4)*WP1(3*i-1)
            S2(3*i-1)=  S2(3*i-1) - ALU_U(9*i-4)*WP2(3*i-1)
            S3(3*i-1)=  S3(3*i-1) - ALU_U(9*i-4)*WP3(3*i-1)
!
            S1(3*i  )=  S1(3*i  ) - ALU_U(9*i  )*WP1(3*i  )
            S2(3*i  )=  S2(3*i  ) - ALU_U(9*i  )*WP2(3*i  )
            S3(3*i  )=  S3(3*i  ) - ALU_U(9*i  )*WP3(3*i  )
          enddo
        enddo
!$omp end parallel do
      enddo

       end subroutine backward_substitute_3x33d
!
!  ---------------------------------------------------------------------
!
      end module backward_substitute_33d
