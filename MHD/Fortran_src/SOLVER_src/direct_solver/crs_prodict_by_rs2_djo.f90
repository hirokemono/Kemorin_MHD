!crs_prodict_by_rs2_djo.f90
!
!      module crs_prodict_by_rs2_djo
!
!     Written by H. Matsui
!
!      subroutine crs_num_rs2_mat_djo_mat_11(NP, NA, NB,                &
!     &          NMA_rs2, INA_rs2, IA_rs2,                              &
!     &          NPM, INOD_djo, INOD_djo_rev, INM_djo, IAM_djo,          &
!     &          NAL, NAU, NL_AB, NU_AB, INL_AB, INU_AB, iflag)
!      subroutine crs_prod_rs2_mat_djo_mat_11(NP, NA, NB,               &
!     &          NMA_rs2, INA_rs2, IA_rs2, A_rs2,                       &
!     &          NPM, INOD_djo, INOD_djo_rev, INM_djo, IAM_djo, BM_djo, &
!     &          NAL, NAU, INL_AB, INU_AB, IAL_AB, IAU_AB,              &
!     &          ABD_crs, ABL_crs, ABU_crs, iflag, AB_tmp)
!
!       Product of matrices  AB = A B
!         A: 2 dimensinoal raw strage
!         B: Ordered raw strage
!         AB: Compressed raw_strage
!
      module crs_prodict_by_rs2_djo
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
!
      subroutine crs_num_rs2_mat_djo_mat_11(NP, NA, NB,                 &
     &          NMA_rs2, INA_rs2, IA_rs2,                               &
     &          NPM, INOD_djo, INOD_djo_rev, INM_djo, IAM_djo,          &
     &          NAL, NAU, NL_AB, NU_AB, INL_AB, INU_AB, iflag)
!
      integer(kind=kint), intent(in) :: NB
!
      integer(kind=kint), intent(in) :: NA, NMA_rs2
      integer(kind=kint), intent(in) :: INA_rs2(NA)
      integer(kind=kint), intent(in) :: IA_rs2(NA,NMA_rs2)
!
      integer(kind=kint ) ::  NP
      integer(kind=kint ) ::  NPM
      integer(kind=kint ), intent(in) :: INOD_djo(NP)
      integer(kind=kint ), intent(inout) :: INOD_djo_rev(NP)
      integer(kind=kint ), intent(in) :: INM_djo(0:NP)
      integer(kind=kint ), intent(in) :: IAM_djo(NPM)
!
      integer(kind = kint), intent(inout) :: NAL, NAU
      integer(kind = kint), intent(inout) :: NL_AB(NA)
      integer(kind = kint), intent(inout) :: NU_AB(NA)
      integer(kind = kint), intent(inout) :: INL_AB(0:NA)
      integer(kind = kint), intent(inout) :: INU_AB(0:NA)
!
      integer(kind=kint), intent(inout) :: iflag(NB)
!
      integer(kind = kint) :: i, j, k, kk, ist, ied, ii, kl
!
!
      do kk = 1, NP
        k = INOD_djo(kk)
        INOD_djo_rev(k) = kk
      end do
!
!$omp parallel do private(i,j,k,kk,ist,ied,ii,kl,iflag)
      do j = 1, NA
        iflag(1:NB) =  0
!
        do kl = 1, INA_rs2(j)
          k =  IA_rs2(j,kl)
          kk = INOD_djo_rev(k)
!
          ist = INM_djo(kk-1) + 1
          ied = INM_djo(kk  )
          do ii = ist, ied
            i = IAM_djo(ii)
            iflag(i) = 1
          end do
        end do
!
        NL_AB(j) = 0
        do i = 1, j-1
          NL_AB(j) = NL_AB(j) + iflag(i)
        end do
!
        NU_AB(j) = 0
        do i = j+1, NB
          NU_AB(j) = NU_AB(j) + iflag(i)
        end do
      end do
!$omp end parallel do
!
      do j = 1, NA
        INL_AB(j) = INL_AB(j-1) + NL_AB(j)
        INU_AB(j) = INU_AB(j-1) + NU_AB(j)
      end do
      NAL = INL_AB(NA)
      NAU = INU_AB(NA)
!
      end subroutine crs_num_rs2_mat_djo_mat_11
!
!-----------------------------------------------------------------------
!
      subroutine crs_prod_rs2_mat_djo_mat_11(NP, NA, NB,                &
     &          NMA_rs2, INA_rs2, IA_rs2, A_rs2,                        &
     &          NPM, INOD_djo, INOD_djo_rev, INM_djo, IAM_djo, BM_djo,  &
     &          NAL, NAU, INL_AB, INU_AB, IAL_AB, IAU_AB,               &
     &          ABD_crs, ABL_crs, ABU_crs, iflag, AB_tmp)
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
      integer(kind = kint), intent(in) :: NAL, NAU
      integer(kind = kint), intent(in) :: INL_AB(0:NA)
      integer(kind = kint), intent(in) :: INU_AB(0:NA)
!
      integer(kind = kint), intent(inout) :: IAL_AB(NAL)
      integer(kind = kint), intent(inout) :: IAU_AB(NAU)
      real(kind = kreal), intent(inout) ::  ABD_crs(NA)
      real(kind = kreal), intent(inout) ::  ABL_crs(NAL)
      real(kind = kreal), intent(inout) ::  ABU_crs(NAU)
!
      integer(kind=kint), intent(inout) :: iflag(NB)
      real(kind = kreal), intent(inout) :: AB_tmp(NB)
!
      integer(kind = kint) :: i, j, k, kk, ist, ied, ii, kl, icou
!
!
      do kk = 1, NP
        k = INOD_djo(kk)
        INOD_djo_rev(k) = kk
      end do
!
!$omp parallel do private(i,j,k,kk,ist,ied,ii,kl,AB_tmp,iflag,icou)
      do j = 1, NA
        AB_tmp(1:NB) = 0.0d0
        iflag(1:NB) =  0
!
        do kl = 1, INA_rs2(j)
          k =  IA_rs2(j,kl)
          kk = INOD_djo_rev(k)
!
          ist = INM_djo(kk-1) + 1
          ied = INM_djo(kk  )
          do ii = ist, ied
            i = IAM_djo(ii)
            iflag(i) = 1
            AB_tmp(i) = AB_tmp(i) + A_rs2(j,kl) * BM_djo(ii)
          end do
        end do
!
        if(iflag(j) .gt. 0) then
          ABD_crs(j) =  AB_tmp(j)
        end if
!
        icou = INL_AB(j-1)
        do i = 1, j-1
          if(iflag(i) .gt. 0) then
            icou = icou + 1
            IAL_AB(icou) = i
            ABL_crs(icou) =  AB_tmp(i)
          end if
        end do
!
        icou = INU_AB(j-1)
        do i = j+1, NB
          if(iflag(i) .gt. 0) then
            icou = icou + 1
            IAU_AB(icou) = i
            ABU_crs(icou) =  AB_tmp(i)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine crs_prod_rs2_mat_djo_mat_11
!
!-----------------------------------------------------------------------
!
      end module crs_prodict_by_rs2_djo
