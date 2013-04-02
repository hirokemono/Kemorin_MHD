!rs2_prodict_by_djo_crs.f90
!
!      module rs2_prodict_by_djo_crs
!
!     Written by H. Matsui
!
!      subroutine rs2_prod_djo_num_crs_mat(NP, NA, NB,                  &
!     &          NAM, INOD_djo, INM_djo, IAM_djo,                       &
!     &          NPL, NPU, INL_B, INU_B, IAL_B, IAU_B,                  &
!     &          BU_crs, NMAB_rs2, INAB_rs2, iflag)
!      subroutine rs2_prod_djo_mat_crs_mat_11(NP, NA, NB,               &
!     &          NAM, INOD_djo, INM_djo, IAM_djo, AM_djo,               &
!     &          NPL, NPU, INL_B, INU_B, IAL_B, IAU_B,                  &
!     &          BD_crs, BL_crs, BU_crs, NMAB_rs2, IAB_rs2, AB_rs2,     &
!     &          iflag, AB_tmp)
!
!       Product of matrices  AB = A B
!         B: Compressed raw strage
!         A: Ordered raw strage
!         AB: 2 dimensinoal raw strage
!
      module rs2_prodict_by_djo_crs
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
      subroutine rs2_prod_djo_num_crs_mat(NP, NA, NB,                   &
     &          NAM, INOD_djo, INM_djo, IAM_djo,                        &
     &          NPL, NPU, INL_B, INU_B, IAL_B, IAU_B,                   &
     &          NMAB_rs2, INAB_rs2, iflag)
!
      integer(kind=kint ), intent(in) ::  NA
      integer(kind=kint ), intent(in) ::  NAM
      integer(kind=kint ), intent(in) :: INOD_djo(NA)
      integer(kind=kint ), intent(in) :: INM_djo(0:NA)
      integer(kind=kint ), intent(in) :: IAM_djo(NAM)
!
      integer(kind = kint), intent(in) :: NP, NPL, NPU
      integer(kind = kint), intent(in) :: INL_B(0:NP)
      integer(kind = kint), intent(in) :: INU_B(0:NP)
      integer(kind = kint), intent(in) :: IAL_B(NPL)
      integer(kind = kint), intent(in) :: IAU_B(NPU)
!
      integer(kind=kint), intent(in) :: NB
!
      integer(kind=kint), intent(inout) :: NMAB_rs2
      integer(kind=kint), intent(inout) :: INAB_rs2(NA)
!
      integer(kind=kint), intent(inout) :: iflag(NB)
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: ii, ist, ied
      integer(kind = kint) :: kk, kst, ked, jj, icou
!
!
      INAB_rs2(1:NA) = 0
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,ist,ied,ii,jj,kst,ked,kk,iflag,icou)
      do jj = 1, NA
        j = INOD_djo(jj)
        iflag(1:NB) =  0
!
        kst = INM_djo(jj-1) + 1
        ked = INM_djo(jj  )
        do kk = kst, ked
          k = IAM_djo(kk)
          iflag(k) =  1
!
          ist = INL_B(k-1) + 1
          ied = INL_B(k  )
          do ii = ist, ied
            i = IAL_B(ii)
            iflag(i) =  1
          end do
!
          ist = INU_B(k-1) + 1
          ied = INU_B(k  )
          do ii = ist, ied
            i =  IAU_B(ii)
            iflag(i) =  1
          end do
        end do
!
        icou = 0
        do i = 1, NB
          icou = icou + iflag(i)
        end do
        INAB_rs2(j) = icou
      end do
!$omp end parallel do
!
      NMAB_rs2 = 0
      do j = 1, NA
        NMAB_rs2 = max(NMAB_rs2,INAB_rs2(j))
      end do
!
      end subroutine rs2_prod_djo_num_crs_mat
!
!-----------------------------------------------------------------------
!
      subroutine rs2_prod_djo_mat_crs_mat_11(NP, NA, NB,                &
     &          NAM, INOD_djo, INM_djo, IAM_djo, AM_djo,                &
     &          NPL, NPU, INL_B, INU_B, IAL_B, IAU_B,                   &
     &          BD_crs, BL_crs, BU_crs, NMAB_rs2, IAB_rs2, AB_rs2,      &
     &          iflag, AB_tmp)
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
      integer(kind=kint), intent(in) :: NB, NMAB_rs2
!
      integer(kind=kint), intent(inout) :: IAB_rs2(NA,NMAB_rs2)
      real(kind = kreal), intent(inout) :: AB_rs2(NA,NMAB_rs2)
!
      integer(kind=kint), intent(inout) :: iflag(NB)
      real(kind = kreal), intent(inout) :: AB_tmp(NB)
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: ii, ist, ied
      integer(kind = kint) :: kk, kst, ked, jj, icou
!
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,ist,ied,ii,jj,kst,ked,kk,AB_tmp,iflag,icou)
      do jj = 1, NA
        j = INOD_djo(jj)
        AB_tmp(1:NB) = 0.0d0
        iflag(1:NB) =  0
!
        kst = INM_djo(jj-1) + 1
        ked = INM_djo(jj  )
        do kk = kst, ked
          k = IAM_djo(kk)
          iflag(k) =  1
          AB_tmp(k) = AB_tmp(k) + AM_djo(kk) * BD_crs(k)
!
          ist = INL_B(k-1) + 1
          ied = INL_B(k  )
          do ii = ist, ied
            i = IAL_B(ii)
            iflag(i) =  1
            AB_tmp(i) = AB_tmp(i) + AM_djo(kk) * BL_crs(ii)
          end do
!
          ist = INU_B(k-1) + 1
          ied = INU_B(k  )
          do ii = ist, ied
            i =  IAU_B(ii)
            iflag(i) =  1
            AB_tmp(i) = AB_tmp(i) + AM_djo(kk) * BU_crs(ii)
          end do
        end do
!
        icou = 0
        do i = 1, NB
          if(iflag(i) .gt. 0) then
            icou = icou + 1
            IAB_rs2(j,icou) = i
            AB_rs2(j,icou) =  AB_tmp(i)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine rs2_prod_djo_mat_crs_mat_11
!
!-----------------------------------------------------------------------
!
      end module rs2_prodict_by_djo_crs
