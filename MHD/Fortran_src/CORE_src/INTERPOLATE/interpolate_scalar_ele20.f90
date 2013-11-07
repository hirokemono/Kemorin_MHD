!>@file   interpolate_scalar_ele20.f90
!!@brief  module interpolate_scalar_ele20
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation for scaslar in quardorature element
!!
!!@verbatim
!!      subroutine itp_matvec_scalar_edge3(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_scalar_ele20(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_scalar_ele20
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
      subroutine itp_matvec_scalar_edge3(np_smp, NP, v_org,             &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NC)
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
          vect(ig) =  AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 ) &
     &              + AM(ist+ 3) * v_org(i3 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_scalar_edge3
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_scalar_ele20(np_smp, NP, v_org,             &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NC)
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
          vect(ig) =  AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 ) &
     &              + AM(ist+ 3) * v_org(i3 ) + AM(ist+ 4) * v_org(i4 ) &
     &              + AM(ist+ 5) * v_org(i5 ) + AM(ist+ 6) * v_org(i6 ) &
     &              + AM(ist+ 7) * v_org(i7 ) + AM(ist+ 8) * v_org(i8 ) &
     &              + AM(ist+ 9) * v_org(i9 ) + AM(ist+10) * v_org(i10) &
     &              + AM(ist+11) * v_org(i11) + AM(ist+12) * v_org(i12) &
     &              + AM(ist+13) * v_org(i13) + AM(ist+14) * v_org(i14) &
     &              + AM(ist+15) * v_org(i15) + AM(ist+16) * v_org(i16) &
     &              + AM(ist+17) * v_org(i17) + AM(ist+18) * v_org(i18) &
     &              + AM(ist+19) * v_org(i19) + AM(ist+20) * v_org(i20)
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_scalar_ele20
!
! ----------------------------------------------------------------------
!
      end module interpolate_scalar_ele20
