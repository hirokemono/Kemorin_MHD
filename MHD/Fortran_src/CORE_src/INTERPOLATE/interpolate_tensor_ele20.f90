!>@file   interpolate_tensor_ele20.f90
!!@brief  module interpolate_tensor_ele20
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation for symmetric tensor in quardorature element
!!
!!@verbatim
!!      subroutine itp_matvec_tensor_edge3(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_tensor_ele20(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_tensor_ele20
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_tensor_edge3(np_smp, NP, v_org,             &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(6*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(6*NC)
!
      integer (kind = kint) :: ip, ist, ist_s, ied_s, ig
      integer (kind = kint) :: i1, i2, i3
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist, i1,i2,i3)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
!
          i1 =  IAM(ist+ 1)
          i2 =  IAM(ist+ 2)
          i3 =  IAM(ist+ 3)
!
          vect(6*ig-5)                                                  &
     &     =  AM(ist+ 1) * v_org(6* i1-5) + AM(ist+ 2) * v_org(6* i2-5) &
     &      + AM(ist+ 3) * v_org(6* i3-5)
!
          vect(6*ig-4)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-4) + AM(ist+ 2) * v_org(6* i2-4) &
     &      + AM(ist+ 3) * v_org(6* i3-4)
!
          vect(6*ig-3)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-3) + AM(ist+ 2) * v_org(6* i2-3) &
     &      + AM(ist+ 3) * v_org(6* i3-3)
!
          vect(6*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-2) + AM(ist+ 2) * v_org(6* i2-2) &
     &      + AM(ist+ 3) * v_org(6* i3-2)
!
          vect(6*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-1) + AM(ist+ 2) * v_org(6* i2-1) &
     &      + AM(ist+ 3) * v_org(6* i3-1)
!
          vect(6*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(6* i1  ) + AM(ist+ 2) * v_org(6* i2  ) &
     &      + AM(ist+ 3) * v_org(6* i3  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_tensor_edge3
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_tensor_ele20(np_smp, NP, v_org,             &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(6*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(6*NC)
!
      integer (kind = kint) :: ip, ist, ist_s, ied_s, ig
      integer (kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
      integer (kind = kint) :: i9, i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,i1,i2,i3,i4,i5,i6,i7,i8,i9,&
!$omp&                    i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,i20)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
!
          i1 =  IAM(ist+ 1)
          i2 =  IAM(ist+ 2)
          i3 =  IAM(ist+ 3)
          i4 =  IAM(ist+ 4)
          i5 =  IAM(ist+ 5)
          i6 =  IAM(ist+ 6)
          i7 =  IAM(ist+ 7)
          i8 =  IAM(ist+ 8)
          i9 =  IAM(ist+ 9)
          i10 = IAM(ist+10)
          i11 = IAM(ist+11)
          i12 = IAM(ist+12)
          i13 = IAM(ist+13)
          i14 = IAM(ist+14)
          i15 = IAM(ist+15)
          i16 = IAM(ist+16)
          i17 = IAM(ist+17)
          i18 = IAM(ist+18)
          i19 = IAM(ist+19)
          i20 = IAM(ist+20)
!
          vect(6*ig-5)                                                  &
     &     =  AM(ist+ 1) * v_org(6* i1-5) + AM(ist+ 2) * v_org(6* i2-5) &
     &      + AM(ist+ 3) * v_org(6* i3-5) + AM(ist+ 4) * v_org(6* i4-5) &
     &      + AM(ist+ 5) * v_org(6* i5-5) + AM(ist+ 6) * v_org(6* i6-5) &
     &      + AM(ist+ 7) * v_org(6* i7-5) + AM(ist+ 8) * v_org(6* i8-5) &
     &      + AM(ist+ 9) * v_org(6* i9-5) + AM(ist+10) * v_org(6*i10-5) &
     &      + AM(ist+11) * v_org(6*i11-5) + AM(ist+12) * v_org(6*i12-5) &
     &      + AM(ist+13) * v_org(6*i13-5) + AM(ist+14) * v_org(6*i14-5) &
     &      + AM(ist+15) * v_org(6*i15-5) + AM(ist+16) * v_org(6*i16-5) &
     &      + AM(ist+17) * v_org(6*i17-5) + AM(ist+18) * v_org(6*i18-5) &
     &      + AM(ist+19) * v_org(6*i19-5) + AM(ist+20) * v_org(6*i20-5)
!
          vect(6*ig-4)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-4) + AM(ist+ 2) * v_org(6* i2-4) &
     &      + AM(ist+ 3) * v_org(6* i3-4) + AM(ist+ 4) * v_org(6* i4-4) &
     &      + AM(ist+ 5) * v_org(6* i5-4) + AM(ist+ 6) * v_org(6* i6-4) &
     &      + AM(ist+ 7) * v_org(6* i7-4) + AM(ist+ 8) * v_org(6* i8-4) &
     &      + AM(ist+ 9) * v_org(6* i9-4) + AM(ist+10) * v_org(6*i10-4) &
     &      + AM(ist+11) * v_org(6*i11-4) + AM(ist+12) * v_org(6*i12-4) &
     &      + AM(ist+13) * v_org(6*i13-4) + AM(ist+14) * v_org(6*i14-4) &
     &      + AM(ist+15) * v_org(6*i15-4) + AM(ist+16) * v_org(6*i16-4) &
     &      + AM(ist+17) * v_org(6*i17-4) + AM(ist+18) * v_org(6*i18-4) &
     &      + AM(ist+19) * v_org(6*i19-4) + AM(ist+20) * v_org(6*i20-4)
!
          vect(6*ig-3)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-3) + AM(ist+ 2) * v_org(6* i2-3) &
     &      + AM(ist+ 3) * v_org(6* i3-3) + AM(ist+ 4) * v_org(6* i4-3) &
     &      + AM(ist+ 5) * v_org(6* i5-3) + AM(ist+ 6) * v_org(6* i6-3) &
     &      + AM(ist+ 7) * v_org(6* i7-3) + AM(ist+ 8) * v_org(6* i8-3) &
     &      + AM(ist+ 9) * v_org(6* i9-3) + AM(ist+10) * v_org(6*i10-3) &
     &      + AM(ist+11) * v_org(6*i11-3) + AM(ist+12) * v_org(6*i12-3) &
     &      + AM(ist+13) * v_org(6*i13-3) + AM(ist+14) * v_org(6*i14-3) &
     &      + AM(ist+15) * v_org(6*i15-3) + AM(ist+16) * v_org(6*i16-3) &
     &      + AM(ist+17) * v_org(6*i17-3) + AM(ist+18) * v_org(6*i18-3) &
     &      + AM(ist+19) * v_org(6*i19-3) + AM(ist+20) * v_org(6*i20-3)
!
          vect(6*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-2) + AM(ist+ 2) * v_org(6* i2-2) &
     &      + AM(ist+ 3) * v_org(6* i3-2) + AM(ist+ 4) * v_org(6* i4-2) &
     &      + AM(ist+ 5) * v_org(6* i5-2) + AM(ist+ 6) * v_org(6* i6-2) &
     &      + AM(ist+ 7) * v_org(6* i7-2) + AM(ist+ 8) * v_org(6* i8-2) &
     &      + AM(ist+ 9) * v_org(6* i9-2) + AM(ist+10) * v_org(6*i10-2) &
     &      + AM(ist+11) * v_org(6*i11-2) + AM(ist+12) * v_org(6*i12-2) &
     &      + AM(ist+13) * v_org(6*i13-2) + AM(ist+14) * v_org(6*i14-2) &
     &      + AM(ist+15) * v_org(6*i15-2) + AM(ist+16) * v_org(6*i16-2) &
     &      + AM(ist+17) * v_org(6*i17-2) + AM(ist+18) * v_org(6*i18-2) &
     &      + AM(ist+19) * v_org(6*i19-2) + AM(ist+20) * v_org(6*i20-2)
!
          vect(6*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-1) + AM(ist+ 2) * v_org(6* i2-1) &
     &      + AM(ist+ 3) * v_org(6* i3-1) + AM(ist+ 4) * v_org(6* i4-1) &
     &      + AM(ist+ 5) * v_org(6* i5-1) + AM(ist+ 6) * v_org(6* i6-1) &
     &      + AM(ist+ 7) * v_org(6* i7-1) + AM(ist+ 8) * v_org(6* i8-1) &
     &      + AM(ist+ 9) * v_org(6* i9-1) + AM(ist+10) * v_org(6*i10-1) &
     &      + AM(ist+11) * v_org(6*i11-1) + AM(ist+12) * v_org(6*i12-1) &
     &      + AM(ist+13) * v_org(6*i13-1) + AM(ist+14) * v_org(6*i14-1) &
     &      + AM(ist+15) * v_org(6*i15-1) + AM(ist+16) * v_org(6*i16-1) &
     &      + AM(ist+17) * v_org(6*i17-1) + AM(ist+18) * v_org(6*i18-1) &
     &      + AM(ist+19) * v_org(6*i19-1) + AM(ist+20) * v_org(6*i20-1)
!
          vect(6*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(6* i1  ) + AM(ist+ 2) * v_org(6* i2  ) &
     &      + AM(ist+ 3) * v_org(6* i3  ) + AM(ist+ 4) * v_org(6* i4  ) &
     &      + AM(ist+ 5) * v_org(6* i5  ) + AM(ist+ 6) * v_org(6* i6  ) &
     &      + AM(ist+ 7) * v_org(6* i7  ) + AM(ist+ 8) * v_org(6* i8  ) &
     &      + AM(ist+ 9) * v_org(6* i9  ) + AM(ist+10) * v_org(6*i10  ) &
     &      + AM(ist+11) * v_org(6*i11  ) + AM(ist+12) * v_org(6*i12  ) &
     &      + AM(ist+13) * v_org(6*i13  ) + AM(ist+14) * v_org(6*i14  ) &
     &      + AM(ist+15) * v_org(6*i15  ) + AM(ist+16) * v_org(6*i16  ) &
     &      + AM(ist+17) * v_org(6*i17  ) + AM(ist+18) * v_org(6*i18  ) &
     &      + AM(ist+19) * v_org(6*i19  ) + AM(ist+20) * v_org(6*i20  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_tensor_ele20
!
! ----------------------------------------------------------------------
!
      end module interpolate_tensor_ele20
