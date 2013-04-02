!
!      module backward_substitute_33
!
!     Written by Kemorin
!
!       subroutine backward_substitute_1x33(N, NP, NU, NPU, PEsmpTOT,   &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
!     &           S, WP, AU, ALU_U)
!
!       subroutine backward_substitute_3x33(N, NP, NU, NPU, PEsmpTOT,   &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
!     &           S1, S2, S3, WP1, WP2, WP3, AU, ALU_U)
!
      module backward_substitute_33
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
       subroutine backward_substitute_1x33(N, NP, NU, NPU, PEsmpTOT,    &
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
       real(kind = kreal) :: X1, X2, X3, Zm0, Zm1, Zm2
!
!
      iv= NVECT
!!cdir parallel do private(iS,iE,i,X1,X2,X3)
!!$omp parallel do private(iS,iE,i,X1,X2,X3)
!!poption indep (S,ALU_U,STACKmc) tlocal (iS,iE,i,X1,X2,X3)
!      do ip= 1, PEsmpTOT
!        iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
!        iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!!OCL VECTOR, NOVREC
!!cdir nodep
!!voption indep (S,ALU_U)
!        do i= iS, iE
!          X1= 0.d0
!          X2= 0.d0
!          X3= 0.d0
!          X2= X2 - ALU_U(9*i-5)*X1
!          X3= X3 - ALU_U(9*i-2)*X1 - ALU_U(9*i-1)*X2
!          X3= ALU_U(9*i  )*  X3
!          X2= ALU_U(9*i-4)*( X2 - ALU_U(9*i-3)*X3 )
!          X1= ALU_U(9*i-8)*( X1 - ALU_U(9*i-6)*X3 - ALU_U(9*i-7)*X2)
!          S(3*i-2)=  S(3*i-2) - X1
!          S(3*i-1)=  S(3*i-1) - X2
!          S(3*i  )=  S(3*i  ) - X3
!        enddo
!      enddo
!!$omp end parallel do

!poption noparallel
      do iv= NVECT-1, 1, -1
!cdir parallel do private(iv0,iS,iE,i,Zm0,Zm1,Zm2)
!$omp parallel do private(iv0,iS,iE,i,Zm0,Zm1,Zm2)
!poption indep (WP,S,AU,INU,IAU,STACKmc)
!poption tlocal(iv0,iS,iE,i,Zm0,Zm1,Zm2)
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
              Zm2= S(3*kk-2)
              Zm1= S(3*kk-1)
              Zm0= S(3*kk  )
              WP(3*i-2) = WP(3*i-2)                                     &
     &                 + AU(9*k-8)*Zm2 + AU(9*k-7)*Zm1 + AU(9*k-6)*Zm0
              WP(3*i-1) = WP(3*i-1)                                     &
     &                 + AU(9*k-5)*Zm2 + AU(9*k-4)*Zm1 + AU(9*k-3)*Zm0
              WP(3*i  ) = WP(3*i  )                                     &
     &                 + AU(9*k-2)*Zm2 + AU(9*k-1)*Zm1 + AU(9*k  )*Zm0
            enddo
          enddo
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i,X1,X2,X3)
!$omp parallel do private(iS,iE,i,X1,X2,X3)
!poption indep (WP,S,ALU_U,STACKmc) tlocal (iS,iE,i,X1,X2,X3)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP,S,ALU_U)
          do i= iS, iE
            X1= WP(3*i-2)
            X2= WP(3*i-1)
            X3= WP(3*i  )
!            X1 = X1
            X2= X2 - ALU_U(9*i-5)*X1
            X3= X3 - ALU_U(9*i-2)*X1 - ALU_U(9*i-1)*X2
            X3= ALU_U(9*i  )*  X3
            X2= ALU_U(9*i-4)*( X2 - ALU_U(9*i-3)*X3 )
            X1= ALU_U(9*i-8)*( X1 - ALU_U(9*i-6)*X3 - ALU_U(9*i-7)*X2)
            S(3*i-2)=  S(3*i-2) - X1
            S(3*i-1)=  S(3*i-1) - X2
            S(3*i  )=  S(3*i  ) - X3
          enddo
        enddo
!$omp end parallel do
      enddo
!
       end subroutine backward_substitute_1x33
!
!  ---------------------------------------------------------------------
!
       subroutine backward_substitute_3x33(N, NP, NU, NPU, PEsmpTOT,    &
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
       real(kind = kreal) :: X11, X12, X13, Zm10, Zm11, Zm12
       real(kind = kreal) :: X21, X22, X23, Zm20, Zm21, Zm22
       real(kind = kreal) :: X31, X32, X33, Zm30, Zm31, Zm32
!
!
      iv= NVECT
!!cdir parallel do private(iS,iE,i,X11,X12,X13,X21,X22,X23,X31,X32,X33)
!!$omp parallel do private(iS,iE,i,X11,X12,X13,X21,X22,X23,X31,X32,X33)
!!poption indep (S1,S2,S3,ALU_U,STACKmc) 
!!poption tlocal (iS,iE,i,X11,X12,X13,X21,X22,X23,X31,X32,X33)
!      do ip= 1, PEsmpTOT
!        iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
!        iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!!OCL VECTOR, NOVREC
!!cdir nodep
!!voption indep (S1,S2,S3,ALU_U)
!        do i= iS, iE
!          X11= 0.d0
!          X12= 0.d0
!          X13= 0.d0
!          X21= 0.d0
!          X22= 0.d0
!          X23= 0.d0
!          X31= 0.d0
!          X32= 0.d0
!          X33= 0.d0
!
!          X12= X12 - ALU_U(9*i-5)*X11
!          X22= X22 - ALU_U(9*i-5)*X21
!          X32= X32 - ALU_U(9*i-5)*X31
!
!          X13= X13 - ALU_U(9*i-2)*X11 - ALU_U(9*i-1)*X12
!          X23= X23 - ALU_U(9*i-2)*X21 - ALU_U(9*i-1)*X22
!          X33= X33 - ALU_U(9*i-2)*X31 - ALU_U(9*i-1)*X32
!
!          X13= ALU_U(9*i  )*  X13
!          X23= ALU_U(9*i  )*  X23
!          X33= ALU_U(9*i  )*  X33
!
!          X12= ALU_U(9*i-4)*( X12 - ALU_U(9*i-3)*X13 )
!          X22= ALU_U(9*i-4)*( X22 - ALU_U(9*i-3)*X23 )
!          X32= ALU_U(9*i-4)*( X32 - ALU_U(9*i-3)*X33 )
!
!          X11= ALU_U(9*i-8)                                            &
!     &        *( X11 - ALU_U(9*i-6)*X13 - ALU_U(9*i-7)*X12)
!          X21= ALU_U(9*i-8)                                            &
!     &        *( X21 - ALU_U(9*i-6)*X23 - ALU_U(9*i-7)*X22)
!          X31= ALU_U(9*i-8)                                            &
!     &        *( X31 - ALU_U(9*i-6)*X33 - ALU_U(9*i-7)*X32)
!
!          S1(3*i-2)=  S1(3*i-2) - X11
!          S2(3*i-2)=  S2(3*i-2) - X21
!          S3(3*i-2)=  S3(3*i-2) - X31
!
!          S1(3*i-1)=  S1(3*i-1) - X12
!          S2(3*i-1)=  S2(3*i-1) - X22
!          S3(3*i-1)=  S3(3*i-1) - X32
!
!          S1(3*i  )=  S1(3*i  ) - X13
!          S2(3*i  )=  S2(3*i  ) - X23
!          S3(3*i  )=  S3(3*i  ) - X33
!        enddo
!      enddo
!!$omp end parallel do

!poption noparallel
      do iv= NVECT-1, 1, -1
!cdir parallel do 
!cdir& private(iv0,iS,iE,i)
!cdir& private(Zm10,Zm11,Zm12,Zm20,Zm21,Zm22,Zm30,Zm31,Zm32)
!$omp parallel do &
!$omp& private(iv0,iS,iE,i) &
!$omp& private(Zm10,Zm11,Zm12,Zm20,Zm21,Zm22,Zm30,Zm31,Zm32)
!poption indep (WP1,WP2,WP3,S1,S2,S3,AU,INU,IAU,STACKmc)
!poption tlocal(iv0,iS,iE,i,Zm10,Zm11,Zm12,Zm20,Zm21,Zm22,Zm30,Zm31,Zm32)
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
              WP1(3*i-2) = WP1(3*i-2)                                   &
     &               + AU(9*k-8)*Zm12 + AU(9*k-7)*Zm11 + AU(9*k-6)*Zm10
              WP2(3*i-2) = WP2(3*i-2)                                   &
     &               + AU(9*k-8)*Zm22 + AU(9*k-7)*Zm21 + AU(9*k-6)*Zm20
              WP3(3*i-2) = WP3(3*i-2)                                   &
     &               + AU(9*k-8)*Zm32 + AU(9*k-7)*Zm31 + AU(9*k-6)*Zm30
!
              WP1(3*i-1) = WP1(3*i-1)                                   &
     &               + AU(9*k-5)*Zm12 + AU(9*k-4)*Zm11 + AU(9*k-3)*Zm10
              WP2(3*i-1) = WP2(3*i-1)                                   &
     &               + AU(9*k-5)*Zm22 + AU(9*k-4)*Zm21 + AU(9*k-3)*Zm20
              WP3(3*i-1) = WP3(3*i-1)                                   &
     &               + AU(9*k-5)*Zm32 + AU(9*k-4)*Zm31 + AU(9*k-3)*Zm30
!
              WP1(3*i  ) = WP1(3*i  )                                   &
     &               + AU(9*k-2)*Zm12 + AU(9*k-1)*Zm11 + AU(9*k  )*Zm10
              WP2(3*i  ) = WP2(3*i  )                                   &
     &               + AU(9*k-2)*Zm22 + AU(9*k-1)*Zm21 + AU(9*k  )*Zm20
              WP3(3*i  ) = WP3(3*i  )                                   &
     &               + AU(9*k-2)*Zm32 + AU(9*k-1)*Zm31 + AU(9*k  )*Zm30
            enddo
          enddo
        enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i,X11,X12,X13,X21,X22,X23,X31,X32,X33)
!$omp parallel do private(iS,iE,i,X11,X12,X13,X21,X22,X23,X31,X32,X33)
!poption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U,STACKmc) 
!poption tlocal (iS,iE,i,X11,X12,X13,X21,X22,X23,X31,X32,X33)
        do ip= 1, PEsmpTOT
          iS= STACKmc(PEsmpTOT*(iv-1)+ip-1) + 1
          iE= STACKmc(PEsmpTOT*(iv-1)+ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP1,WP2,WP3,S1,S2,S3,ALU_U)
          do i= iS, iE
            X11= WP1(3*i-2)
            X12= WP1(3*i-1)
            X13= WP1(3*i  )
            X21= WP2(3*i-2)
            X22= WP2(3*i-1)
            X23= WP2(3*i  )
            X31= WP3(3*i-2)
            X32= WP3(3*i-1)
            X33= WP3(3*i  )
!
!            X11 = X11
!            X21 = X21
!            X31 = X31
!
            X12= X12 - ALU_U(9*i-5)*X11
            X22= X22 - ALU_U(9*i-5)*X21
            X32= X32 - ALU_U(9*i-5)*X31
!
            X13= X13 - ALU_U(9*i-2)*X11 - ALU_U(9*i-1)*X12
            X23= X23 - ALU_U(9*i-2)*X21 - ALU_U(9*i-1)*X22
            X33= X33 - ALU_U(9*i-2)*X31 - ALU_U(9*i-1)*X32
!
            X13= ALU_U(9*i  )*  X13
            X23= ALU_U(9*i  )*  X23
            X33= ALU_U(9*i  )*  X33
!
            X12= ALU_U(9*i-4)*( X12 - ALU_U(9*i-3)*X13 )
            X22= ALU_U(9*i-4)*( X22 - ALU_U(9*i-3)*X23 )
            X32= ALU_U(9*i-4)*( X32 - ALU_U(9*i-3)*X33 )
!
            X11= ALU_U(9*i-8)                                           &
     &          *( X11 - ALU_U(9*i-6)*X13 - ALU_U(9*i-7)*X12)
            X21= ALU_U(9*i-8)                                           &
     &          *( X21 - ALU_U(9*i-6)*X23 - ALU_U(9*i-7)*X22)
            X31= ALU_U(9*i-8)                                           &
     &          *( X31 - ALU_U(9*i-6)*X33 - ALU_U(9*i-7)*X32)
!
            S1(3*i-2)=  S1(3*i-2) - X11
            S2(3*i-2)=  S2(3*i-2) - X21
            S3(3*i-2)=  S3(3*i-2) - X31
!
            S1(3*i-1)=  S1(3*i-1) - X12
            S2(3*i-1)=  S2(3*i-1) - X22
            S3(3*i-1)=  S3(3*i-1) - X32
!
            S1(3*i  )=  S1(3*i  ) - X13
            S2(3*i  )=  S2(3*i  ) - X23
            S3(3*i  )=  S3(3*i  ) - X33
          enddo
        enddo
!$omp end parallel do
      enddo
!
       end subroutine backward_substitute_3x33
!
!  ---------------------------------------------------------------------
!
      end module backward_substitute_33
