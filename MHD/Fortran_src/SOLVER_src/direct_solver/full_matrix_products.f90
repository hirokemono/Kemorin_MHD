!full_matrix_products.f90
!
!      module full_matrix_products
!
!     Written by H. Matsui
!
!      subroutine prod_mat_mat_full(NP, NA, NB, A, B, AB)
!      subroutine prod_full_mat_crs_mat_11(NP, NA, NB, A,               &
!     &          NPL, NPU, INL_B, INU_B, IAL_B, IAU_B,                  &
!     &          BD_crs, BL_crs, BU_crs, AB)
!      subroutine prod_djo_mat_crs_mat_11(NP, NA, NB,                   &
!     &          NAN, INOD_djo, INM_djo, IAM_djo, AM_djo,               &
!     &          NPL, NPU, INL_B, INU_B, IAL_B, IAU_B,                  &
!     &          BD_crs, BL_crs, BU_crs, AB)
!
!       Product of matrices  AB = A B
!
      module full_matrix_products
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
      subroutine prod_mat_mat_full(NP, NA, NB, A, B, AB)
!
      integer(kind=kint), intent(in) :: NP, NA, NB
      real(kind = kreal), intent(in) :: A(NA,NP), B(NP,NB)
      real(kind = kreal), intent(inout) :: AB(NA,NB)
!
      integer(kind = kint) :: i, j, k
!
!
      AB = 0.0d0
!$omp parallel do private(i,j,k)
      do i = 1, NB
        do j = 1, NA
          do k = 1, NP
            AB(j,i) = AB(j,i) + A(j,k) * B(k,i)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine prod_mat_mat_full
!
!-----------------------------------------------------------------------
!
      subroutine prod_full_mat_crs_mat_11(NP, NA, NB, A,                &
     &          NPL, NPU, INL_B, INU_B, IAL_B, IAU_B,                   &
     &          BD_crs, BL_crs, BU_crs, AB)
!
      integer(kind=kint), intent(in) :: NA, NB
      real(kind = kreal), intent(in) :: A(NA,NP)
!
      integer(kind = kint), intent(in) :: NP, NPL, NPU
      integer(kind = kint), intent(in) :: INL_B(0:NP)
      integer(kind = kint), intent(in) :: INU_B(0:NP)
      integer(kind = kint), intent(in) :: IAL_B(NPL)
      integer(kind = kint), intent(in) :: IAU_B(NPU)
      real(kind = kreal), intent(in) ::  BD_crs(NP)
      real(kind = kreal), intent(in) ::  BL_crs(NPL)
      real(kind = kreal), intent(in) ::  BU_crs(NPU)
!
      real(kind = kreal), intent(inout) :: AB(NA,NB)
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: ii, ist, ied
!
!
      AB = 0.0d0
!
!$omp parallel do private(i,j,k,ist,ied,ii)
      do j = 1, NA
        do k = 1, NP
          AB(j,k) = AB(j,k) + A(j,k) * BD_crs(k)
        end do
!
        do k = 1, NP
          ist = INL_B(k-1) + 1
          ied = INL_B(k  )
          do ii = ist, ied
            i = IAL_B(ii)
            AB(j,i) = AB(j,i) + A(j,k) * BL_crs(ii)
          end do
        end do
!
        do k = 1, NP
          ist = INU_B(k-1) + 1
          ied = INU_B(k  )
          do ii = ist, ied
            i = IAU_B(ii)
            AB(j,i) = AB(j,i) + A(j,k) * BU_crs(ii)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine prod_full_mat_crs_mat_11
!
!-----------------------------------------------------------------------
!
      subroutine prod_djo_mat_crs_mat_11(NP, NA, NB,                    &
     &          NAM, INOD_djo, INM_djo, IAM_djo, AM_djo,                &
     &          NPL, NPU, INL_B, INU_B, IAL_B, IAU_B,                   &
     &          BD_crs, BL_crs, BU_crs, AB)
!
      integer(kind=kint ) ::  NA
      integer(kind=kint ) ::  NAM
      integer(kind=kint ), intent(in) :: INOD_djo(NA)
      integer(kind=kint ), intent(in) :: INM_djo(0:NA)
      integer(kind=kint ), intent(in) :: IAM_djo(NAM)
      real   (kind=kreal), intent(in) ::  AM_djo(NAM)
!
      integer(kind = kint), intent(in) :: NP, NPL, NPU
      integer(kind = kint), intent(in) :: INL_B(0:NP)
      integer(kind = kint), intent(in) :: INU_B(0:NP)
      integer(kind = kint), intent(in) :: IAL_B(NPL)
      integer(kind = kint), intent(in) :: IAU_B(NPU)
      real(kind = kreal), intent(in) ::  BD_crs(NP)
      real(kind = kreal), intent(in) ::  BL_crs(NPL)
      real(kind = kreal), intent(in) ::  BU_crs(NPU)
!
      integer(kind=kint), intent(in) :: NB
      real(kind = kreal), intent(inout) :: AB(NA,NB)
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: ii, ist, ied
      integer(kind = kint) :: kk, kst, ked, jj
!
!
      AB = 0.0d0
!
!$omp parallel do private(i,j,k,ist,ied,ii,jj,kst,ked,kk)
      do jj = 1, NA
        j = INOD_djo(jj)
!
        kst = INM_djo(jj-1) + 1
        ked = INM_djo(jj  )
        do kk = kst, ked
          k = IAM_djo(kk)
          AB(j,k) = AB(j,k) + AM_djo(kk) * BD_crs(k)
!
          ist = INL_B(k-1) + 1
          ied = INL_B(k  )
          do ii = ist, ied
            i = IAL_B(ii)
            AB(j,i) = AB(j,i) + AM_djo(kk) * BL_crs(ii)
          end do
!
          ist = INU_B(k-1) + 1
          ied = INU_B(k  )
          do ii = ist, ied
            i = IAU_B(ii)
            AB(j,i) = AB(j,i) + AM_djo(kk) * BU_crs(ii)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine prod_djo_mat_crs_mat_11
!
!-----------------------------------------------------------------------
!
      subroutine prod_full_mat_djo_mat_11(NP, NA, NB, A,                &
     &          NPM, INOD_djo, INM_djo, IAM_djo, BM_djo, AB)
!
      integer(kind=kint), intent(in) :: NA, NB
      real(kind = kreal), intent(in) :: A(NA,NP)
!
      integer(kind=kint ) ::  NP
      integer(kind=kint ) ::  NPM
      integer(kind=kint ), intent(in) :: INOD_djo(NP)
      integer(kind=kint ), intent(in) :: INM_djo(0:NP)
      integer(kind=kint ), intent(in) :: IAM_djo(NPM)
      real   (kind=kreal), intent(in) ::  BM_djo(NPM)
!
      real(kind = kreal), intent(inout) :: AB(NA,NB)
!
      integer(kind = kint) :: i, j, k, kk, ist, ied, ii
!
!
      AB = 0.0d0
!$omp parallel do private(i,j,k,kk,ist,ied,ii)
      do j = 1, NA
        do kk = 1, NP
          k = INOD_djo(kk)
          ist = INM_djo(kk-1) + 1
          ied = INM_djo(kk  )
          do ii = ist, ied
            i = IAM_djo(ii)
            AB(j,i) = AB(j,i) + A(j,k) * BM_djo(ii)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine prod_full_mat_djo_mat_11
!
!-----------------------------------------------------------------------
!
      subroutine prod_rs2_mat_djo_mat_11(NP, NA, NB,                    &
     &          NMA_rs2, INA_rs2, IA_rs2, A_rs2,                        &
     &          NPM, INOD_djo, INOD_djo_rev, INM_djo, IAM_djo, BM_djo,  &
     &          AB)
!
      integer(kind=kint), intent(in) :: NB
!
      integer(kind=kint), intent(in) :: NA, NMA_rs2
      integer(kind=kint), intent(in) :: INA_rs2(NA)
      integer(kind=kint), intent(in) :: IA_rs2(NA,NMA_rs2)
      real(kind = kreal), intent(in) :: A_rs2(NA,NMA_rs2)
!
      integer(kind=kint ) ::  NP
      integer(kind=kint ) ::  NPM
      integer(kind=kint ), intent(in) :: INOD_djo(NP)
      integer(kind=kint ), intent(inout) :: INOD_djo_rev(NP)
      integer(kind=kint ), intent(in) :: INM_djo(0:NP)
      integer(kind=kint ), intent(in) :: IAM_djo(NPM)
      real   (kind=kreal), intent(in) ::  BM_djo(NPM)
!
      real(kind = kreal), intent(inout) :: AB(NA,NB)
!
      integer(kind = kint) :: i, j, k, kk, ist, ied, ii, kl
!
!
      do kk = 1, NP
        k = INOD_djo(kk)
        INOD_djo_rev(k) = kk
      end do
!
      AB = 0.0d0
!$omp parallel do private(i,j,k,kk,ist,ied,ii,kl)
      do j = 1, NA
        do kl = 1, INA_rs2(j)
          k =  IA_rs2(j,kl)
          kk = INOD_djo_rev(k)
!
          ist = INM_djo(kk-1) + 1
          ied = INM_djo(kk  )
          do ii = ist, ied
            i = IAM_djo(ii)
            AB(j,i) = AB(j,i) + A_rs2(j,kl) * BM_djo(ii)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine prod_rs2_mat_djo_mat_11
!
!-----------------------------------------------------------------------
!
      end module full_matrix_products
