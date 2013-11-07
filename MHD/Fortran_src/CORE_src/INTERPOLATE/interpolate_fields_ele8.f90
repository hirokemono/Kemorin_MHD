!>@file   interpolate_fields_ele8.f90
!!@brief  module interpolate_fields_ele8
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation of arbitorary fields in tri-linear element
!!
!!@verbatim
!!      subroutine itp_matvec_fields_node(np_smp, NP, NB, v_org,        &
!!     &          NC, NCM, INM, IAM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_fields_edge2(np_smp, NP, NB, v_org,       &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_fields_surf4(np_smp, NP, NB, v_org,       &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_fields_ele8(np_smp, NP, NB, v_org,        &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_fields_ele8
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
      subroutine itp_matvec_fields_node(np_smp, NP, NB, v_org,          &
     &          NC, NCM, INM, IAM, IEND_SUM_smp, vect)
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
!
      real (kind=kreal), intent(inout) :: vect(NB*NC)
!
      integer (kind = kint) :: ip, ist, ist_s, ied_s, ig, i, nd
      integer (kind = kint) :: i1
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,i1,i,nd)
      do ip = 1, np_smp
        ist_s = NB*IEND_SUM_smp(ip-1) + 1
        ied_s = NB*IEND_SUM_smp(ip)
        do i = ist_s, ied_s
          nd = mod(i-1,NB) + 1
          ig = (i - nd) / NB + 1
!
          ist = INM(ig-1)
          i1 =  nd + NB * (IAM(ist+ 1) - 1)
!
          vect(i) =  v_org(i1 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_fields_node
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_fields_edge2(np_smp, NP, NB, v_org,         &
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
      integer (kind = kint) :: i1, i2
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,i1,i2,i,nd)
      do ip = 1, np_smp
        ist_s = NB*IEND_SUM_smp(ip-1) + 1
        ied_s = NB*IEND_SUM_smp(ip)
        do i = ist_s, ied_s
          nd = mod(i-1,NB) + 1
          ig = (i - nd) / NB + 1
!
          ist = INM(ig-1)
!
          i1 =  nd + NB * (IAM(ist+ 1) - 1)
          i2 =  nd + NB * (IAM(ist+ 2) - 1)
!
          vect(i)  =  AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_fields_edge2
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_fields_surf4(np_smp, NP, NB, v_org,         &
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
      integer (kind = kint) :: i1, i2, i3, i4
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,i1,i2,i3,i4,i,nd)
      do ip = 1, np_smp
        ist_s = NB*IEND_SUM_smp(ip-1) + 1
        ied_s = NB*IEND_SUM_smp(ip)
        do i = ist_s, ied_s
          nd = mod(i-1,NB) + 1
          ig = (i - nd) / NB + 1
!
          ist = INM(ig-1)
!
          i1 =  nd + NB * (IAM(ist+ 1) - 1)
          i2 =  nd + NB * (IAM(ist+ 2) - 1)
          i3 =  nd + NB * (IAM(ist+ 3) - 1)
          i4 =  nd + NB * (IAM(ist+ 4) - 1)
!
          vect(i) =   AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 ) &
     &              + AM(ist+ 3) * v_org(i3 ) + AM(ist+ 4) * v_org(i4 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_fields_surf4
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_fields_ele8(np_smp, NP, NB, v_org,          &
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
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist,i1,i2,i3,i4,i5,i6,i7,i8,   &
!$omp&                    i,nd)
      do ip = 1, np_smp
        ist_s = NB*IEND_SUM_smp(ip-1) + 1
        ied_s = NB*IEND_SUM_smp(ip)
        do i = ist_s, ied_s
          nd = mod(i-1,NB) + 1
          ig = (i - nd) / NB + 1
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
!
          vect(i) =   AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 ) &
     &              + AM(ist+ 3) * v_org(i3 ) + AM(ist+ 4) * v_org(i4 ) &
     &              + AM(ist+ 5) * v_org(i5 ) + AM(ist+ 6) * v_org(i6 ) &
     &              + AM(ist+ 7) * v_org(i7 ) + AM(ist+ 8) * v_org(i8 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_fields_ele8
!
! ----------------------------------------------------------------------
!
      end module interpolate_fields_ele8
