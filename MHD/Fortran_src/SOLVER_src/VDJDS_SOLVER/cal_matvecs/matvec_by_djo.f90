!matvec_by_djo.f90
!     module matvec_by_djo
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine matvec_djo_11(NP, NC, NCM, INM, IAM, AM, B, X,        &
!     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!      subroutine matvec_djo_13(NP, NC, NCM, INM, IAM, AM, B, X,        &
!     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!      subroutine matvec_djo_16(NP, NC, NCM, INM, IAM, AM, B, X,        &
!     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!      subroutine matvec_djo_1N(NB, NP, NC, NCM, INM, IAM, AM, B, X,    &
!     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!
!      subroutine matvec_djo_33(NP, NC, NCM, INM, IAM, AM, B, X,        &
!     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!      subroutine matvec_djo_NN(NB, NP, NC, NCM, INM, IAM, AM, B, X,    &
!     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!
      module matvec_by_djo
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_11(NP, NC, NCM, INM, IAM, AM, B, X,         &
     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!
      integer(kind=kint), intent(in) :: NP, NC, NCM, np_smp
      integer(kind=kint), intent(in) :: NUM_NCOMP
      integer(kind=kint), intent(in) :: INOD_DJO(NC)
      integer(kind=kint), intent(in) :: NUM_SUM(0:NUM_NCOMP)
      integer(kind=kint), intent(in) :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind=kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
      real(kind = kreal), intent(in) :: B(NP)
!
      real(kind = kreal), intent(inout) :: X(NC)
!
      integer(kind = kint) :: ip, ic
      integer(kind = kint) :: inum, inod, jnod, k, kk, icou, ist, ied
!
!
      if(NP .le. 0) return
!
!$omp parallel do
      do inod = 1, NC
        X(inod) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(ip,ic,icou,ist,ied,k,kk,inum,inod,jnod)
      do ip = 1, np_smp
        do icou = 1, NUM_NCOMP
          ic = ip + (icou-1) * np_smp
          ist = IEND_SUM_smp(ic-1)+1
          ied = IEND_SUM_smp(ic)
          do k = 1, NUM_SUM(icou)
!cdir nodep
            do inum = ist, ied
              inod = INOD_DJO(inum)
              kk = k + INM(inum-1)
              jnod = IAM(kk)
              X(inod) = X(inod) + AM(kk) * B(jnod)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine matvec_djo_11
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_13(NP, NC, NCM, INM, IAM, AM, B, X,         &
     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!
      integer(kind=kint), intent(in) :: NP, NC, NCM, np_smp
      integer(kind=kint), intent(in) :: NUM_NCOMP
      integer(kind=kint), intent(in) :: INOD_DJO(NC)
      integer(kind=kint), intent(in) :: NUM_SUM(0:NUM_NCOMP)
      integer(kind=kint), intent(in) :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind=kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
      real(kind = kreal), intent(in) :: B(3*NP)
!
      real(kind = kreal), intent(inout) :: X(3*NC)
!
      integer(kind = kint) :: ip, ic
      integer(kind = kint) :: inum, inod, jnod, k, kk, icou, ist, ied
!
!
      if(NP .le. 0) return
!
!$omp parallel do
      do inod = 1, NC
        X(3*inod-2) = 0.0d0
        X(3*inod-1) = 0.0d0
        X(3*inod  ) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(ip,ic,icou,ist,ied,k,kk,inum,inod,jnod)
      do ip = 1, np_smp
        do icou = 1, NUM_NCOMP
          ic = ip + (icou-1) * np_smp
          ist = IEND_SUM_smp(ic-1)+1
          ied = IEND_SUM_smp(ic)
          do k = 1, NUM_SUM(icou)
!cdir nodep
            do inum = ist, ied
              inod = INOD_DJO(inum)
              kk = k + INM(inum-1)
              jnod = IAM(kk)
              X(3*inod-2) = X(3*inod-2) + AM(kk) * B(3*jnod-2)
              X(3*inod-1) = X(3*inod-1) + AM(kk) * B(3*jnod-1)
              X(3*inod  ) = X(3*inod  ) + AM(kk) * B(3*jnod  )
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine matvec_djo_13
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_16(NP, NC, NCM, INM, IAM, AM, B, X,         &
     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!
      integer(kind=kint), intent(in) :: NP, NC, NCM, np_smp
      integer(kind=kint), intent(in) :: NUM_NCOMP
      integer(kind=kint), intent(in) :: INOD_DJO(NC)
      integer(kind=kint), intent(in) :: NUM_SUM(0:NUM_NCOMP)
      integer(kind=kint), intent(in) :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind=kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
      real(kind = kreal), intent(in) :: B(6*NP)
!
      real(kind = kreal), intent(inout) :: X(6*NC)
!
      integer(kind = kint) :: ip, ic
      integer(kind = kint) :: inum, inod, jnod, k, kk, icou, ist, ied
!
!
      if(NP .le. 0) return
!
!$omp parallel do
      do inod = 1, NC
        X(6*inod-5) = 0.0d0
        X(6*inod-4) = 0.0d0
        X(6*inod-3) = 0.0d0
        X(6*inod-2) = 0.0d0
        X(6*inod-1) = 0.0d0
        X(6*inod  ) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(ip,ic,icou,ist,ied,k,kk,inum,inod,jnod)
      do ip = 1, np_smp
        do icou = 1, NUM_NCOMP
          ic = ip + (icou-1) * np_smp
          ist = IEND_SUM_smp(ic-1)+1
          ied = IEND_SUM_smp(ic)
          do k = 1, NUM_SUM(icou)
!cdir nodep
            do inum = ist, ied
              inod = INOD_DJO(inum)
              kk = k + INM(inum-1)
              jnod = IAM(kk)
              X(6*inod-5) = X(6*inod-5) + AM(kk) * B(6*jnod-5)
              X(6*inod-4) = X(6*inod-4) + AM(kk) * B(6*jnod-4)
              X(6*inod-3) = X(6*inod-3) + AM(kk) * B(6*jnod-3)
              X(6*inod-2) = X(6*inod-2) + AM(kk) * B(6*jnod-2)
              X(6*inod-1) = X(6*inod-1) + AM(kk) * B(6*jnod-1)
              X(6*inod  ) = X(6*inod  ) + AM(kk) * B(6*jnod  )
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine matvec_djo_16
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_1N(NB, NP, NC, NCM, INM, IAM, AM, B, X,     &
     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!
      integer(kind=kint), intent(in) :: NB, NP, NC, NCM, np_smp
      integer(kind=kint), intent(in) :: NUM_NCOMP
      integer(kind=kint), intent(in) :: INOD_DJO(NC)
      integer(kind=kint), intent(in) :: NUM_SUM(0:NUM_NCOMP)
      integer(kind=kint), intent(in) :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind=kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
      real(kind = kreal), intent(in) :: B(NB*NP)
!
      real(kind = kreal), intent(inout) :: X(NB*NC)
!
      integer(kind = kint) :: ip, ic
      integer(kind = kint) :: inum, ii, jj, k, kk, icou
      integer(kind = kint) ::  ist, ied, nd
!
!
      if(NP .le. 0) return
!
!$omp parallel private(inum,ii)
      do nd = 1,  NB
!$omp do
        do inum = 1, NC
          ii = (inum-1) * NB + nd
          X(ii) = 0.0d0
        end do
!$omp end do
      end do
!$omp end parallel
!
!$omp parallel do private(ip,ic,icou,ist,ied,k,kk,inum,nd,ii,jj)
      do ip = 1, np_smp
        do icou = 1, NUM_NCOMP
          ic = ip + (icou-1) * np_smp
          ist = IEND_SUM_smp(ic-1)+1
          ied = IEND_SUM_smp(ic)
          do k = 1, NUM_SUM(icou)
            do nd = 1,  NB
!cdir nodep
              do inum = ist, ied
                ii = (INOD_DJO(inum)-1) * NB + nd
                kk = k + INM(inum-1)
                jj = NB*(IAM(kk)-1) + nd
                X(ii) = X(ii) + AM(kk) * B(jj)
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine matvec_djo_1N
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_33(NP, NC, NCM, INM, IAM, AM, B, X,         &
     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!
      integer(kind=kint), intent(in) :: NP, NC, NCM, np_smp
      integer(kind=kint), intent(in) :: NUM_NCOMP
      integer(kind=kint), intent(in) :: INOD_DJO(NC)
      integer(kind=kint), intent(in) :: NUM_SUM(0:NUM_NCOMP)
      integer(kind=kint), intent(in) :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind=kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(9*NCM)
      real(kind = kreal), intent(in) :: B(3*NP)
!
      real(kind = kreal), intent(inout) :: X(3*NC)
!
      integer(kind = kint) :: ip, ic
      integer(kind = kint) :: inum, inod, jnod, k, kk, icou
      integer(kind = kint) ::  ist, ied
!
!
      if(NP .le. 0) return
!
!$omp parallel do
      do inod = 1, NC
        X(3*inod-2) = 0.0d0
        X(3*inod-1) = 0.0d0
        X(3*inod  ) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(ip,ic,icou,ist,ied,k,kk,inum,inod,jnod)
      do ip = 1, np_smp
        do icou = 1, NUM_NCOMP
          ic = ip + (icou-1) * np_smp
          ist = IEND_SUM_smp(ic-1)+1
          ied = IEND_SUM_smp(ic)
          do k = 1, NUM_SUM(icou)
!cdir nodep
            do inum = ist, ied
              inod = INOD_DJO(inum)
              kk = k + INM(inum-1)
              jnod = IAM(kk)
              X(3*inod-2) = X(3*inod-2) + AM(9*kk-8) * B(3*jnod-2)      &
     &                                  + AM(9*kk-7) * B(3*jnod-1)      &
     &                                  + AM(9*kk-6) * B(3*jnod  )
              X(3*inod-1) = X(3*inod-1) + AM(9*kk-5) * B(3*jnod-2)      &
     &                                  + AM(9*kk-4) * B(3*jnod-1)      &
     &                                  + AM(9*kk-3) * B(3*jnod  )
              X(3*inod  ) = X(3*inod  ) + AM(9*kk-2) * B(3*jnod-2)      &
     &                                  + AM(9*kk-1) * B(3*jnod-1)      &
     &                                  + AM(9*kk  ) * B(3*jnod  )
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine matvec_djo_33
!
!-----------------------------------------------------------------------
!
      subroutine matvec_djo_NN(NB, NP, NC, NCM, INM, IAM, AM, B, X,     &
     &          NUM_NCOMP, INOD_DJO, NUM_SUM, np_smp, IEND_SUM_smp)
!
      integer(kind=kint), intent(in) :: NP, NB, NC, NCM, np_smp
      integer(kind=kint), intent(in) :: NUM_NCOMP
      integer(kind=kint), intent(in) :: INOD_DJO(NC)
      integer(kind=kint), intent(in) :: NUM_SUM(0:NUM_NCOMP)
      integer(kind=kint), intent(in) :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind=kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NB*NB*NCM)
      real(kind = kreal), intent(in) :: B(NB*NP)
!
      real(kind = kreal), intent(inout) :: X(NB*NC)
!
      integer(kind = kint) :: ip, ic
      integer(kind = kint) :: inum, ii, jj, k, kk, icou
      integer(kind = kint) ::  ist, ied, n1, n2
!
!
      if(NP .le. 0) return
!
!$omp parallel private(inum,ii)
      do n1 = 1,  NB
!$omp do
        do inum = 1, NC
          ii = (inum-1) * NB + n1
          X(ii) = 0.0d0
        end do
!$omp end do
      end do
!$omp end parallel
!
!$omp parallel do private(ip,ic,icou,ist,ied,k,kk,inum,n1,n2,ii,jj)
      do ip = 1, np_smp
        do icou = 1, NUM_NCOMP
          ic = ip + (icou-1) * np_smp
          ist = IEND_SUM_smp(ic-1)+1
          ied = IEND_SUM_smp(ic)
          do k = 1, NUM_SUM(icou)
            do n1 = 1,  NB
              do n2 = 1,  NB
!cdir nodep
                do inum = ist, ied
                  ii = (INOD_DJO(inum)-1) * NB + n1
                  kk =   (k + INM(inum-1) - 1) * NB + n2
                  jj = (IAM(kk)-1) * NB + n2
                  X(ii) = X(ii) + AM(kk) * B(jj)
                end do
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine matvec_djo_NN
!
!-----------------------------------------------------------------------
!
      end module matvec_by_djo
