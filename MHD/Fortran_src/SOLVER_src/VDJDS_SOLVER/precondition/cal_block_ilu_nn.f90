!
!      module cal_block_ilu_nn
!
!     Written by Kemorin
!
!       subroutine cal_bl_ilu_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,       &
!     &           S, ALU_L)
!       subroutine cal_bl_ilu_2xnn(NP, N, NB, PEsmpTOT, STACKmcG,       &
!     &           S1, S2, ALU_L)
!       subroutine cal_bl_ilu_3xnn(NP, N, NB, PEsmpTOT, STACKmcG,       &
!     &           S1, S2, S3, ALU_L)
!
!       subroutine cal_bl_ilu_1xnnd(NP, N, NB, PEsmpTOT, STACKmcG,      &
!     &           S, ALU_L)
!       subroutine cal_bl_ilu_2xnnd(NP, N, NB, PEsmpTOT, STACKmcG,      &
!     &           S1, S2, ALU_L)
!       subroutine cal_bl_ilu_3xnnd(NP, N, NB, PEsmpTOT, STACKmcG,      &
!     &           S1, S2, S3, ALU_L)
!
      module cal_block_ilu_nn
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
       subroutine cal_bl_ilu_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,        &
     &           S, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, k2, ii, im, ix
!
!
!$omp parallel do private(iS,iE,i,ii,ix,im,k1,k2)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )

        do k1 = 2, NB
          do k2 = 1, k1-1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_L)
            do i= iS, iE
              ii =    NB*(i-1)     + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S(ii) = S(ii) - ALU_L(im) *S(ix)
            end do
          end do
        end do

!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_L)
        do i= iS, iE
          ii =    NB*i
          im = NB*NB*i
          S(ii) = ALU_L(im) * S(ii)
        end do

        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_L)
            do i= iS, iE
              ii =    NB*(i-1) + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S(ii) = S(ii) - ALU_L(im) *S(ix)
            end do
          end do
!
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_L)
          do i= iS, iE
            ii =    NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S(ii) = ALU_L(im) * S(ii)
          end do
        end do

      enddo
!$omp end parallel do
!
       end subroutine cal_bl_ilu_1xnn
!
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_2xnn(NP, N, NB, PEsmpTOT, STACKmcG,        &
     &           S1, S2, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, k2, ii, im, ix
!
!
!$omp parallel do private(iS,iE,i,ii,ix,im,k1,k2)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )

        do k1 = 2, NB
          do k2 = 1, k1-1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,ALU_L)
            do i= iS, iE
              ii =    NB*(i-1)     + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S1(ii) = S1(ii) - ALU_L(im) *S1(ix)
              S2(ii) = S2(ii) - ALU_L(im) *S2(ix)
            end do
          end do
        end do

!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,ALU_L)
        do i= iS, iE
          ii =    NB*i
          im = NB*NB*i
          S1(ii) = ALU_L(im) * S1(ii)
          S2(ii) = ALU_L(im) * S2(ii)
        end do

        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,ALU_L)
            do i= iS, iE
              ii =    NB*(i-1) + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S1(ii) = S1(ii) - ALU_L(im) *S1(ix)
              S2(ii) = S2(ii) - ALU_L(im) *S2(ix)
            end do
          end do
!
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,ALU_L)
          do i= iS, iE
            ii =    NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S1(ii) = ALU_L(im) * S1(ii)
            S2(ii) = ALU_L(im) * S2(ii)
          end do
        end do

      enddo
!$omp end parallel do
!
       end subroutine cal_bl_ilu_2xnn
!
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_3xnn(NP, N, NB, PEsmpTOT, STACKmcG,        &
     &           S1, S2, S3, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, k2, ii, im, ix
!
!
!$omp parallel do private(iS,iE,i,ii,ix,im,k1,k2)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )

        do k1 = 2, NB
          do k2 = 1, k1-1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,ALU_L)
            do i= iS, iE
              ii =    NB*(i-1)     + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S1(ii) = S1(ii) - ALU_L(im) *S1(ix)
              S2(ii) = S2(ii) - ALU_L(im) *S2(ix)
              S3(ii) = S3(ii) - ALU_L(im) *S3(ix)
            end do
          end do
        end do

!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,ALU_L)
        do i= iS, iE
          ii =    NB*i
          im = NB*NB*i
          S1(ii) = ALU_L(im) * S1(ii)
          S2(ii) = ALU_L(im) * S2(ii)
          S3(ii) = ALU_L(im) * S3(ii)
        end do

        do k1 = NB-1, 1, -1
          do k2 = NB, k1+1, -1
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,ALU_L)
            do i= iS, iE
              ii =    NB*(i-1) + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S1(ii) = S1(ii) - ALU_L(im) *S1(ix)
              S2(ii) = S2(ii) - ALU_L(im) *S2(ix)
              S3(ii) = S3(ii) - ALU_L(im) *S3(ix)
            end do
          end do
!
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,ALU_L)
          do i= iS, iE
            ii =    NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S1(ii) = ALU_L(im) * S1(ii)
            S2(ii) = ALU_L(im) * S2(ii)
            S3(ii) = ALU_L(im) * S3(ii)
          end do
        end do

      enddo
!$omp end parallel do
!
       end subroutine cal_bl_ilu_3xnn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_1xnnd(NP, N, NB, PEsmpTOT, STACKmcG,       &
     &           S, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, ii, im
!
!
!$omp parallel do private(iS,iE,i,ii,im,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )

        do k1 = 1,NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,ALU_L)
          do i= iS, iE
            ii =    NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S(ii) = ALU_L(im) * S(ii)
          end do
        end do
       end do
!$omp end parallel do
!
       end subroutine cal_bl_ilu_1xnnd
!
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_2xnnd(NP, N, NB, PEsmpTOT, STACKmcG,       &
     &           S1, S2, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, ii, im
!
!
!$omp parallel do private(iS,iE,i,ii,im,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )

        do k1 = 1,NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,ALU_L)
          do i= iS, iE
            ii =    NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S1(ii) = ALU_L(im) * S1(ii)
            S2(ii) = ALU_L(im) * S2(ii)
          end do
        end do
      end do
!$omp end parallel do
!
       end subroutine cal_bl_ilu_2xnnd
!
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_3xnnd(NP, N, NB, PEsmpTOT, STACKmcG,       &
     &           S1, S2, S3, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, ii, im
!
!
!$omp parallel do private(iS,iE,i,ii,im,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )

        do k1 = 1,NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,ALU_L)
          do i= iS, iE
            ii =    NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S1(ii) = ALU_L(im) * S1(ii)
            S2(ii) = ALU_L(im) * S2(ii)
            S3(ii) = ALU_L(im) * S3(ii)
          end do
        end do
      enddo
!$omp end parallel do
!
       end subroutine cal_bl_ilu_3xnnd
!
!  ---------------------------------------------------------------------
!
      end module cal_block_ilu_nn
