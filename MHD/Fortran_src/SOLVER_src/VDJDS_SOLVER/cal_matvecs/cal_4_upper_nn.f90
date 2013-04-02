!
!      module cal_4_upper_nn
!
!     Written by Kemorin
!
!       subroutine subtract_upper_nn(NP, NB, NU, NPU, PEsmpTOT, NVECT,  &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!       subroutine subtract_upper_3xnn(NP, NB, NU, NPU, PEsmpTOT,       &
!     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,               &
!     &           S1, S2, S3, AU, V1, V2, V3 )
!
!       subroutine add_upper_nn(NP, NB, NU, NPU, PEsmpTOT, NVECT,       &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!       subroutine add_upper_3xnn(NP, NB, NU, NPU, PEsmpTOT, NVECT,     &
!     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,      &
!     &           V1, V2, V3 )
!
      module cal_4_upper_nn
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
       subroutine subtract_upper_nn(NP, NB, NU, NPU, PEsmpTOT, NVECT,   &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!
       integer(kind = kint), intent(in) :: NP, NB, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k
       integer (kind = kint) :: k1, k2, ii, ix, im
!
!
!cdir parallel do private(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
!$omp parallel do private(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
!poption indep (S,V,IAU,INU,STACKmc)
!poption tlocal(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT
!poption noparallel
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
        do  j= 1, NUhyp(iv)
          iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
          iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )

          do k1 = 1, NB
            do k2 = 1, NB

!voption indep (S,V,IAU,AU)
!OCL VECTOR, NOVREC
!cdir nodep
              do i= iv0+1, iv0+iE-iS
                k= i+iS - iv0
                ii =    NB*(  i-1   )    + k1
                ix =    NB*(IAU(k)-1)             + k2
                im = NB*NB*(  k-1   ) + NB*(k1-1) + k2
                S(ii) = S(ii) - AU(im) * V(ix)
              enddo

            end do
          end do

        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine subtract_upper_nn
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_upper_3xnn(NP, NB, NU, NPU, PEsmpTOT,        &
     &           NVECT, npUX1, STACKmc, NUhyp, INU, IAU,                &
     &           S1, S2, S3, AU, V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NB, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k
       integer (kind = kint) :: k1, k2, ii, ix, im
!
!
!cdir parallel do private(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
!$omp parallel do private(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
!poption indep (S1,S2,S3,V1,V2,V3,IAU,INU,STACKmc)
!poption tlocal(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT
!poption noparallel
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
        do  j= 1, NUhyp(iv)
          iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
          iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )

          do k1 = 1, NB
            do k2 = 1, NB

!voption indep (S1,S2,S3,V1,V2,V3,IAU,AU)
!OCL VECTOR, NOVREC
!cdir nodep
              do i= iv0+1, iv0+iE-iS
                k= i+iS - iv0
                ii =    NB*(  i-1   )    + k1
                ix =    NB*(IAU(k)-1)             + k2
                im = NB*NB*(  k-1   ) + NB*(k1-1) + k2
                S1(ii) = S1(ii) - AU(im) * V1(ix)
                S2(ii) = S2(ii) - AU(im) * V2(ix)
                S3(ii) = S3(ii) - AU(im) * V3(ix)
              enddo

            end do
          end do

        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine subtract_upper_3xnn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_upper_nn(NP, NB, NU, NPU, PEsmpTOT, NVECT,        &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S, AU, V )
!
       integer(kind = kint), intent(in) :: NP, NB, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k
       integer (kind = kint) :: k1, k2, ii, ix, im
!
!
!cdir parallel do private(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
!$omp parallel do private(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
!poption indep (S,V,IAU,INU,STACKmc)
!poption tlocal(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT
!poption noparallel
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
        do  j= 1, NUhyp(iv)
          iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
          iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )

          do k1 = 1, NB
            do k2 = 1, NB

!voption indep (S,V,IAU,AU)
!OCL VECTOR, NOVREC
!cdir nodep
              do i= iv0+1, iv0+iE-iS
                k= i+iS - iv0
                ii =    NB*(  i-1   )    + k1
                ix =    NB*(IAU(k)-1)             + k2
                im = NB*NB*(  k-1   ) + NB*(k1-1) + k2
                S(ii) = S(ii) + AU(im) * V(ix)
              enddo

            end do
          end do

        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine add_upper_nn
!
!  ---------------------------------------------------------------------
!
       subroutine add_upper_3xnn(NP, NB, NU, NPU, PEsmpTOT, NVECT,      &
     &           npUX1, STACKmc, NUhyp, INU, IAU, S1, S2, S3, AU,       &
     &           V1, V2, V3 )
!
       integer(kind = kint), intent(in) :: NP, NB, NU, NPU
       integer(kind = kint), intent(in) :: PEsmpTOT, NVECT, npUX1
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAU(NPU)

       real(kind = kreal), intent(in) :: AU(NB*NB*NPU)
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k
       integer (kind = kint) :: k1, k2, ii, ix, im
!
!
!cdir parallel do private(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
!$omp parallel do private(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
!poption indep (S1,S2,S3,V1,V2,V3,IAU,INU,STACKmc)
!poption tlocal(iv0,j,iS,iE,i,k,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT
!poption noparallel
       do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
!poption noparallel
        do  j= 1, NUhyp(iv)
          iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
          iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )

          do k1 = 1, NB
            do k2 = 1, NB

!voption indep (S1,S2,S3,V1,V2,V3,IAU,AU)
!OCL VECTOR, NOVREC
!cdir nodep
              do i= iv0+1, iv0+iE-iS
                k= i+iS - iv0
                ii =    NB*(  i-1   )    + k1
                ix =    NB*(IAU(k)-1)             + k2
                im = NB*NB*(  k-1   ) + NB*(k1-1) + k2
                S1(ii) = S1(ii) + AU(im) * V1(ix)
                S2(ii) = S2(ii) + AU(im) * V2(ix)
                S3(ii) = S3(ii) + AU(im) * V3(ix)
              enddo

            end do
          end do

        enddo
       enddo
      enddo
!$omp end parallel do

       end subroutine add_upper_3xnn
!
!  ---------------------------------------------------------------------
!
      end module cal_4_upper_nn
