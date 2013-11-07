!>@file   interpolate_tensor_ele27.f90
!!@brief  module interpolate_tensor_ele27
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation for symmetric tensor in Lagrange element
!!
!!@verbatim
!!      subroutine itp_matvec_tensor_surf9(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_tensor_ele27(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_tensor_ele27
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
      subroutine itp_matvec_tensor_surf9(np_smp, NP, v_org,             &
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
      integer (kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8, i9
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,                           &
!$omp&                    i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
!
          vect(6*ig-5)                                                  &
     &     =  AM(ist+ 1) * v_org(6* i1-5) + AM(ist+ 2) * v_org(6* i2-5) &
     &      + AM(ist+ 3) * v_org(6* i3-5) + AM(ist+ 4) * v_org(6* i4-5) &
     &      + AM(ist+ 5) * v_org(6* i5-5) + AM(ist+ 6) * v_org(6* i6-5) &
     &      + AM(ist+ 7) * v_org(6* i7-5) + AM(ist+ 8) * v_org(6* i8-5) &
     &      + AM(ist+ 9) * v_org(6* i9-5)
!
          vect(6*ig-4)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-4) + AM(ist+ 2) * v_org(6* i2-4) &
     &      + AM(ist+ 3) * v_org(6* i3-4) + AM(ist+ 4) * v_org(6* i4-4) &
     &      + AM(ist+ 5) * v_org(6* i5-4) + AM(ist+ 6) * v_org(6* i6-4) &
     &      + AM(ist+ 7) * v_org(6* i7-4) + AM(ist+ 8) * v_org(6* i8-4) &
     &      + AM(ist+ 9) * v_org(6* i9-4)
!
          vect(6*ig-3)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-3) + AM(ist+ 2) * v_org(6* i2-3) &
     &      + AM(ist+ 3) * v_org(6* i3-3) + AM(ist+ 4) * v_org(6* i4-3) &
     &      + AM(ist+ 5) * v_org(6* i5-3) + AM(ist+ 6) * v_org(6* i6-3) &
     &      + AM(ist+ 7) * v_org(6* i7-3) + AM(ist+ 8) * v_org(6* i8-3) &
     &      + AM(ist+ 9) * v_org(6* i9-3)
!
          vect(6*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-2) + AM(ist+ 2) * v_org(6* i2-2) &
     &      + AM(ist+ 3) * v_org(6* i3-2) + AM(ist+ 4) * v_org(6* i4-2) &
     &      + AM(ist+ 5) * v_org(6* i5-2) + AM(ist+ 6) * v_org(6* i6-2) &
     &      + AM(ist+ 7) * v_org(6* i7-2) + AM(ist+ 8) * v_org(6* i8-2) &
     &      + AM(ist+ 9) * v_org(6* i9-2)
!
          vect(6*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-1) + AM(ist+ 2) * v_org(6* i2-1) &
     &      + AM(ist+ 3) * v_org(6* i3-1) + AM(ist+ 4) * v_org(6* i4-1) &
     &      + AM(ist+ 5) * v_org(6* i5-1) + AM(ist+ 6) * v_org(6* i6-1) &
     &      + AM(ist+ 7) * v_org(6* i7-1) + AM(ist+ 8) * v_org(6* i8-1) &
     &      + AM(ist+ 9) * v_org(6* i9-1)
!
          vect(6*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(6* i1  ) + AM(ist+ 2) * v_org(6* i2  ) &
     &      + AM(ist+ 3) * v_org(6* i3  ) + AM(ist+ 4) * v_org(6* i4  ) &
     &      + AM(ist+ 5) * v_org(6* i5  ) + AM(ist+ 6) * v_org(6* i6  ) &
     &      + AM(ist+ 7) * v_org(6* i7  ) + AM(ist+ 8) * v_org(6* i8  ) &
     &      + AM(ist+ 9) * v_org(6* i9  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_tensor_surf9
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_tensor_ele27(np_smp, NP, v_org,             &
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
      integer (kind = kint) :: i17, i18, i19, i20, i21, i22, i23, i24
      integer (kind = kint) :: i25, i26, i27
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,i1,i2,i3,i4,i5,i6,i7,i8,i9,&
!$omp&                    i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,i20,  &
!$omp&                    i21,i22,i23,i24,i25,i26,i27)
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
          i21 = IAM(ist+21)
          i22 = IAM(ist+22)
          i23 = IAM(ist+23)
          i24 = IAM(ist+24)
          i25 = IAM(ist+25)
          i26 = IAM(ist+26)
          i27 = IAM(ist+27)
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
     &      + AM(ist+19) * v_org(6*i19-5) + AM(ist+20) * v_org(6*i20-5) &
     &      + AM(ist+21) * v_org(6*i21-5) + AM(ist+22) * v_org(6*i22-5) &
     &      + AM(ist+23) * v_org(6*i23-5) + AM(ist+24) * v_org(6*i24-5) &
     &      + AM(ist+25) * v_org(6*i25-5) + AM(ist+26) * v_org(6*i26-5) &
     &      + AM(ist+27) * v_org(6*i27-5)
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
     &      + AM(ist+19) * v_org(6*i19-4) + AM(ist+20) * v_org(6*i20-4) &
     &      + AM(ist+21) * v_org(6*i21-4) + AM(ist+22) * v_org(6*i22-4) &
     &      + AM(ist+23) * v_org(6*i23-4) + AM(ist+24) * v_org(6*i24-4) &
     &      + AM(ist+25) * v_org(6*i25-4) + AM(ist+26) * v_org(6*i26-4) &
     &      + AM(ist+27) * v_org(6*i27-4)
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
     &      + AM(ist+19) * v_org(6*i19-3) + AM(ist+20) * v_org(6*i20-3) &
     &      + AM(ist+21) * v_org(6*i21-3) + AM(ist+22) * v_org(6*i22-3) &
     &      + AM(ist+23) * v_org(6*i23-3) + AM(ist+24) * v_org(6*i24-3) &
     &      + AM(ist+25) * v_org(6*i25-3) + AM(ist+26) * v_org(6*i26-3) &
     &      + AM(ist+27) * v_org(6*i27-3)
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
     &      + AM(ist+19) * v_org(6*i19-2) + AM(ist+20) * v_org(6*i20-2) &
     &      + AM(ist+21) * v_org(6*i21-2) + AM(ist+22) * v_org(6*i22-2) &
     &      + AM(ist+23) * v_org(6*i23-2) + AM(ist+24) * v_org(6*i24-2) &
     &      + AM(ist+25) * v_org(6*i25-2) + AM(ist+26) * v_org(6*i26-2) &
     &      + AM(ist+27) * v_org(6*i27-2)
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
     &      + AM(ist+19) * v_org(6*i19-1) + AM(ist+20) * v_org(6*i20-1) &
     &      + AM(ist+21) * v_org(6*i21-1) + AM(ist+22) * v_org(6*i22-1) &
     &      + AM(ist+23) * v_org(6*i23-1) + AM(ist+24) * v_org(6*i24-1) &
     &      + AM(ist+25) * v_org(6*i25-1) + AM(ist+26) * v_org(6*i26-1) &
     &      + AM(ist+27) * v_org(6*i27-1)
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
     &      + AM(ist+19) * v_org(6*i19  ) + AM(ist+20) * v_org(6*i20  ) &
     &      + AM(ist+21) * v_org(6*i21  ) + AM(ist+22) * v_org(6*i22  ) &
     &      + AM(ist+23) * v_org(6*i23  ) + AM(ist+24) * v_org(6*i24  ) &
     &      + AM(ist+25) * v_org(6*i25  ) + AM(ist+26) * v_org(6*i26  ) &
     &      + AM(ist+27) * v_org(6*i27  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_tensor_ele27
!
! ----------------------------------------------------------------------
!
      end module interpolate_tensor_ele27
