!>@file   interpolate_fields_ele20.f90
!!@brief  module interpolate_fields_ele20
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation of arbitorary fields in quardorature element
!!
!!@verbatim
!!      subroutine itp_matvec_fields_edge3(np_smp, NP, NB, v_org,       &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_fields_ele20(np_smp, NP, NB, v_org,       &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_fields_ele20
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
      subroutine itp_matvec_fields_edge3(np_smp, NP, NB, v_org,         &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, NB
      real (kind=kreal), intent(in) :: v_org(NB*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NB*NC)
!
      integer (kind = kint) :: ip, ist, ist_s, ied_s, ig, i, nd
      integer (kind = kint) :: i1, i2, i3
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,i1,i2,i3,i,nd)
      do ip = 1, np_smp
        ist_s = NB*IEND_SUM_smp(ip-1) + 1
        ied_s = NB*IEND_SUM_smp(ip)
        do i = ist_s, ied_s
          nd = mod(i-ione,NB) + ione
          ig = (i - nd) / NB + ione
!
          ist = INM(ig-1)
!
          i1 =  nd + NB * (IAM(ist+ 1) - 1)
          i2 =  nd + NB * (IAM(ist+ 2) - 1)
          i3 =  nd + NB * (IAM(ist+ 3) - 1)
!
          vect(i)  =  AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 ) &
     &              + AM(ist+ 3) * v_org(i3 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_fields_edge3
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_fields_ele20(np_smp, NP, NB, v_org,         &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, NB
      real (kind=kreal), intent(in) :: v_org(NB*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NB*NC)
!
      integer (kind = kint) :: ip, ist, ist_s, ied_s, ig, i, nd
      integer (kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
      integer (kind = kint) :: i9, i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,i1,i2,i3,i4,i5,i6,i7,i8,i9,&
!$omp&                    i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,i20,  &
!$omp&                    i,nd)
      do ip = 1, np_smp
        ist_s = NB*IEND_SUM_smp(ip-1) + 1
        ied_s = NB*IEND_SUM_smp(ip)
        do i = ist_s, ied_s
          nd = mod(i-ione,NB) + ione
          ig = (i - nd) / NB + ione
!
          ist = INM(ig-1)
!
          i1 =  nd + NB * (IAM(ist+ 1) - 1)
          i2 =  nd + NB * (IAM(ist+ 2) - 1)
          i3 =  nd + NB * (IAM(ist+ 3) - 1)
          i4 =  nd + NB * (IAM(ist+ 4) - 1)
          i5 =  nd + NB * (IAM(ist+ 5) - 1)
          i6 =  nd + NB * (IAM(ist+ 6) - 1)
          i7 =  nd + NB * (IAM(ist+ 7) - 1)
          i8 =  nd + NB * (IAM(ist+ 8) - 1)
          i9 =  nd + NB * (IAM(ist+ 9) - 1)
          i10 = nd + NB * (IAM(ist+10) - 1)
          i11 = nd + NB * (IAM(ist+11) - 1)
          i12 = nd + NB * (IAM(ist+12) - 1)
          i13 = nd + NB * (IAM(ist+13) - 1)
          i14 = nd + NB * (IAM(ist+14) - 1)
          i15 = nd + NB * (IAM(ist+15) - 1)
          i16 = nd + NB * (IAM(ist+16) - 1)
          i17 = nd + NB * (IAM(ist+17) - 1)
          i18 = nd + NB * (IAM(ist+18) - 1)
          i19 = nd + NB * (IAM(ist+19) - 1)
          i20 = nd + NB * (IAM(ist+20) - 1)
!
          vect(i) =   AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 ) &
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
      end subroutine itp_matvec_fields_ele20
!
! ----------------------------------------------------------------------
!
      end module interpolate_fields_ele20
