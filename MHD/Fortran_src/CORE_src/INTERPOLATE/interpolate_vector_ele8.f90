!>@file   interpolate_vector_ele8.f90
!!@brief  module interpolate_vector_ele8
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation for vector in tri-linear element
!!
!!@verbatim
!!      subroutine itp_matvec_vector_node(np_smp, NP, v_org,            &
!!     &          NC, NCM, INM, IAM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_vector_edge2(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_vector_surf4(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_vector_ele8(np_smp, NP, v_org,            &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_vector_ele8
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
      subroutine s_interpolate_vector_ele8(np_smp, numnod, numele, ie,  &
     &          v_org, istack_smp, num_points, iele_gauss,              &
     &          xi_gauss, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,8)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(3*numnod)
!
      real (kind=kreal), intent(inout) :: vect(3*num_points)
!
      real (kind=kreal) :: xi, ei, zi
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
!
      real (kind=kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, i2, i3, i4, i5, i6, i7, i8
!
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist,ied,ig,iele,i1,i2,i3,i4,i5,i6,i7,i8,      &
!$omp&                    xi,ei,zi, xi_nega, xi_posi, ei_nega, ei_posi, &
!$omp&                    zi_nega, zi_posi, an1,an2,an3,an4,an5,an6,    &
!$omp&                    an7,an8)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
!
          iele = iele_gauss(ig)
!
          i1 = ie(iele,1)
          i2 = ie(iele,2)
          i3 = ie(iele,3)
          i4 = ie(iele,4)
          i5 = ie(iele,5)
          i6 = ie(iele,6)
          i7 = ie(iele,7)
          i8 = ie(iele,8)
!
          xi = xi_gauss(ig,1)
          ei = xi_gauss(ig,2)
          zi = xi_gauss(ig,3)
!
          xi_nega = one - xi
          xi_posi = one + xi
!
          ei_nega = one - ei
          ei_posi = one + ei
!
          zi_nega = one - zi
          zi_posi = one + zi
!
          an1  = r125 * xi_nega * ei_nega * zi_nega
          an2  = r125 * xi_posi * ei_nega * zi_nega
          an3  = r125 * xi_posi * ei_posi * zi_nega
          an4  = r125 * xi_nega * ei_posi * zi_nega
          an5  = r125 * xi_nega * ei_nega * zi_posi
          an6  = r125 * xi_posi * ei_nega * zi_posi
          an7  = r125 * xi_posi * ei_posi * zi_posi
          an8  = r125 * xi_nega * ei_posi * zi_posi
!
!
          vect(3*ig-2) =  an1  * v_org(3* i1-2) + an2  * v_org(3* i2-2) &
     &                  + an3  * v_org(3* i3-2) + an4  * v_org(3* i4-2) &
     &                  + an5  * v_org(3* i5-2) + an6  * v_org(3* i6-2) &
     &                  + an7  * v_org(3* i7-2) + an8  * v_org(3* i8-2)
!
          vect(3*ig-1) =  an1  * v_org(3* i1-1) + an2  * v_org(3* i2-1) &
     &                  + an3  * v_org(3* i3-1) + an4  * v_org(3* i4-1) &
     &                  + an5  * v_org(3* i5-1) + an6  * v_org(3* i6-1) &
     &                  + an7  * v_org(3* i7-1) + an8  * v_org(3* i8-1)
!
          vect(3*ig  ) =  an1  * v_org(3* i1  ) + an2  * v_org(3* i2  ) &
     &                  + an3  * v_org(3* i3  ) + an4  * v_org(3* i4  ) &
     &                  + an5  * v_org(3* i5  ) + an6  * v_org(3* i6  ) &
     &                  + an7  * v_org(3* i7  ) + an8  * v_org(3* i8  )
!
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_vector_ele8
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_vector_node(np_smp, NP, v_org,              &
     &          NC, NCM, INM, IAM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(3*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(3*NC)
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
          vect(3*ig-2) = v_org(3* i1-2)
          vect(3*ig-1) = v_org(3* i1-1)
          vect(3*ig  ) = v_org(3* i1  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_vector_node
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_vector_edge2(np_smp, NP, v_org,             &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(3*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(3*NC)
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
          vect(3*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(3* i1-2) + AM(ist+ 2) * v_org(3* i2-2)
!
          vect(3*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(3* i1-1) + AM(ist+ 2) * v_org(3* i2-1)
!
          vect(3*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(3* i1  ) + AM(ist+ 2) * v_org(3* i2  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_vector_edge2
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_vector_surf4(np_smp, NP, v_org,             &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(3*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(3*NC)
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
          vect(3*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(3* i1-2) + AM(ist+ 2) * v_org(3* i2-2) &
     &      + AM(ist+ 3) * v_org(3* i3-2) + AM(ist+ 4) * v_org(3* i4-2)
!
          vect(3*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(3* i1-1) + AM(ist+ 2) * v_org(3* i2-1) &
     &      + AM(ist+ 3) * v_org(3* i3-1) + AM(ist+ 4) * v_org(3* i4-1)
!
          vect(3*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(3* i1  ) + AM(ist+ 2) * v_org(3* i2  ) &
     &      + AM(ist+ 3) * v_org(3* i3  ) + AM(ist+ 4) * v_org(3* i4  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_vector_surf4
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_vector_ele8(np_smp, NP, v_org,              &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(3*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(3*NC)
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
          vect(3*ig-2)                                                  &
     &      = AM(ist+ 1) * v_org(3* i1-2) + AM(ist+ 2) * v_org(3* i2-2) &
     &      + AM(ist+ 3) * v_org(3* i3-2) + AM(ist+ 4) * v_org(3* i4-2) &
     &      + AM(ist+ 5) * v_org(3* i5-2) + AM(ist+ 6) * v_org(3* i6-2) &
     &      + AM(ist+ 7) * v_org(3* i7-2) + AM(ist+ 8) * v_org(3* i8-2)
!
          vect(3*ig-1)                                                  &
     &      = AM(ist+ 1) * v_org(3* i1-1) + AM(ist+ 2) * v_org(3* i2-1) &
     &      + AM(ist+ 3) * v_org(3* i3-1) + AM(ist+ 4) * v_org(3* i4-1) &
     &      + AM(ist+ 5) * v_org(3* i5-1) + AM(ist+ 6) * v_org(3* i6-1) &
     &      + AM(ist+ 7) * v_org(3* i7-1) + AM(ist+ 8) * v_org(3* i8-1)
!
          vect(3*ig  )                                                  &
     &      = AM(ist+ 1) * v_org(3* i1  ) + AM(ist+ 2) * v_org(3* i2  ) &
     &      + AM(ist+ 3) * v_org(3* i3  ) + AM(ist+ 4) * v_org(3* i4  ) &
     &      + AM(ist+ 5) * v_org(3* i5  ) + AM(ist+ 6) * v_org(3* i6  ) &
     &      + AM(ist+ 7) * v_org(3* i7  ) + AM(ist+ 8) * v_org(3* i8  )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_vector_ele8
!
! ----------------------------------------------------------------------
!
      end module interpolate_vector_ele8
