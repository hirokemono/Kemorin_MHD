!>@file   interpolate_tensor_ele8.f90
!!@brief  module interpolate_tensor_ele8
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation for symmetric tensor in tri-linear element
!!
!!@verbatim
!!      subroutine itp_matvec_tensor_node(np_smp, NP, v_org,            &
!!     &          NC, NCM, INM, IAM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_tensor_edge2(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_tensor_surf4(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_tensor_ele8(np_smp, NP, v_org,            &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_tensor_ele8
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
      subroutine itp_matvec_tensor_node(np_smp, NP, v_org,              &
     &          NC, NCM, INM, IAM, IEND_SUM_smp, vect)
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
!
      real (kind=kreal), intent(inout) :: vect(6*NC)
!
      integer (kind = kint) :: ip, ist, ist_s, ied_s, ig
      integer (kind = kint) :: i1
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist, i1)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
          i1 =  IAM(ist+ 1)
!
          vect(6*ig-5) = v_org(6* i1-5)
          vect(6*ig-4) = v_org(6* i1-4)
          vect(6*ig-3) = v_org(6* i1-3)
          vect(6*ig-2) = v_org(6* i1-2)
          vect(6*ig-1) = v_org(6* i1-1)
          vect(6*ig  ) = v_org(6* i1  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_tensor_node
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_tensor_edge2(np_smp, NP, v_org,             &
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
      integer (kind = kint) :: i1, i2
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist, i1,i2)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
!
          i1 =  IAM(ist+ 1)
          i2 =  IAM(ist+ 2)
!
          vect(6*ig-5)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-5) + AM(ist+ 2) * v_org(6* i2-5)
!
          vect(6*ig-4)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-4) + AM(ist+ 2) * v_org(6* i2-4)
!
          vect(6*ig-3)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-3) + AM(ist+ 2) * v_org(6* i2-3)
!
          vect(6*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-2) + AM(ist+ 2) * v_org(6* i2-2)
!
          vect(6*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-1) + AM(ist+ 2) * v_org(6* i2-1)
!
          vect(6*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(6* i1  ) + AM(ist+ 2) * v_org(6* i2  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_tensor_edge2
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_tensor_surf4(np_smp, NP, v_org,             &
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
      integer (kind = kint) :: i1, i2, i3, i4
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist, i1,i2,i3,i4)
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
!
          vect(6*ig-5)                                                  &
     &     =  AM(ist+ 1) * v_org(6* i1-5) + AM(ist+ 2) * v_org(6* i2-5) &
     &      + AM(ist+ 3) * v_org(6* i3-5) + AM(ist+ 4) * v_org(6* i4-5)
!
          vect(6*ig-4)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-4) + AM(ist+ 2) * v_org(6* i2-4) &
     &      + AM(ist+ 3) * v_org(6* i3-4) + AM(ist+ 4) * v_org(6* i4-4)
!
          vect(6*ig-3)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-3) + AM(ist+ 2) * v_org(6* i2-3) &
     &      + AM(ist+ 3) * v_org(6* i3-3) + AM(ist+ 4) * v_org(6* i4-3)
!
          vect(6*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-2) + AM(ist+ 2) * v_org(6* i2-2) &
     &      + AM(ist+ 3) * v_org(6* i3-2) + AM(ist+ 4) * v_org(6* i4-2)
!
          vect(6*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-1) + AM(ist+ 2) * v_org(6* i2-1) &
     &      + AM(ist+ 3) * v_org(6* i3-1) + AM(ist+ 4) * v_org(6* i4-1)
!
          vect(6*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(6* i1  ) + AM(ist+ 2) * v_org(6* i2  ) &
     &      + AM(ist+ 3) * v_org(6* i3  ) + AM(ist+ 4) * v_org(6* i4  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_tensor_surf4
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_tensor_ele8(np_smp, NP, v_org,              &
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
!
!
!$omp parallel do private(ist_s,ied_s,ig,ist, i1,i2,i3,i4,i5,i6,i7,i8)
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
!
          vect(6*ig-5)                                                  &
     &     =  AM(ist+ 1) * v_org(6* i1-5) + AM(ist+ 2) * v_org(6* i2-5) &
     &      + AM(ist+ 3) * v_org(6* i3-5) + AM(ist+ 4) * v_org(6* i4-5) &
     &      + AM(ist+ 5) * v_org(6* i5-5) + AM(ist+ 6) * v_org(6* i6-5) &
     &      + AM(ist+ 7) * v_org(6* i7-5) + AM(ist+ 8) * v_org(6* i8-5)
!
          vect(6*ig-4)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-4) + AM(ist+ 2) * v_org(6* i2-4) &
     &      + AM(ist+ 3) * v_org(6* i3-4) + AM(ist+ 4) * v_org(6* i4-4) &
     &      + AM(ist+ 5) * v_org(6* i5-4) + AM(ist+ 6) * v_org(6* i6-4) &
     &      + AM(ist+ 7) * v_org(6* i7-4) + AM(ist+ 8) * v_org(6* i8-4)
!
          vect(6*ig-3)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-3) + AM(ist+ 2) * v_org(6* i2-3) &
     &      + AM(ist+ 3) * v_org(6* i3-3) + AM(ist+ 4) * v_org(6* i4-3) &
     &      + AM(ist+ 5) * v_org(6* i5-3) + AM(ist+ 6) * v_org(6* i6-3) &
     &      + AM(ist+ 7) * v_org(6* i7-3) + AM(ist+ 8) * v_org(6* i8-3)
!
          vect(6*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-2) + AM(ist+ 2) * v_org(6* i2-2) &
     &      + AM(ist+ 3) * v_org(6* i3-2) + AM(ist+ 4) * v_org(6* i4-2) &
     &      + AM(ist+ 5) * v_org(6* i5-2) + AM(ist+ 6) * v_org(6* i6-2) &
     &      + AM(ist+ 7) * v_org(6* i7-2) + AM(ist+ 8) * v_org(6* i8-2)
!
          vect(6*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(6* i1-1) + AM(ist+ 2) * v_org(6* i2-1) &
     &      + AM(ist+ 3) * v_org(6* i3-1) + AM(ist+ 4) * v_org(6* i4-1) &
     &      + AM(ist+ 5) * v_org(6* i5-1) + AM(ist+ 6) * v_org(6* i6-1) &
     &      + AM(ist+ 7) * v_org(6* i7-1) + AM(ist+ 8) * v_org(6* i8-1)
!
          vect(6*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(6* i1  ) + AM(ist+ 2) * v_org(6* i2  ) &
     &      + AM(ist+ 3) * v_org(6* i3  ) + AM(ist+ 4) * v_org(6* i4  ) &
     &      + AM(ist+ 5) * v_org(6* i5  ) + AM(ist+ 6) * v_org(6* i6  ) &
     &      + AM(ist+ 7) * v_org(6* i7  ) + AM(ist+ 8) * v_org(6* i8  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_tensor_ele8
!
! ----------------------------------------------------------------------
!
      end module interpolate_tensor_ele8
