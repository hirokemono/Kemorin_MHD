!>@file   interpolate_scalar_ele27.f90
!!@brief  module interpolate_scalar_ele27
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation for scalar in Lagrange element
!!
!!@verbatim
!!      subroutine itp_matvec_scalar_surf9(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_scalar_ele27(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_scalar_ele27
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
      subroutine s_interpolate_scalar_ele27(np_smp, numnod, numele, ie, &
     &          v_org, istack_smp, num_points, iele_gauss,              &
     &          xi_gauss, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,27)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      real (kind=kreal), intent(inout) :: vect(num_points)
!
      real (kind=kreal) :: xi, ei, zi
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal) :: xi_sqre, ei_sqre, zi_sqre
!
      real (kind=kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
      real (kind=kreal) :: an9,  an10, an11, an12, an13, an14
      real (kind=kreal) :: an15, an16, an17, an18, an19, an20
      real (kind=kreal) :: an21, an22, an23, an24, an25, an26, an27
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, i2, i3, i4, i5, i6, i7, i8
      integer (kind = kint) :: i9, i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20, i21, i22, i23, i24
      integer (kind = kint) :: i25, i26, i27
!
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist,ied,ig,iele,i1,i2,i3,i4,i5,i6,i7,i8,i9,   &
!$omp&                    i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,i20,  &
!$omp&                    i21,i22,i23,i24,i25,i26,i27,xi,ei,zi,         &
!$omp&                    xi_nega, xi_posi, ei_nega, ei_posi,           &
!$omp&                    zi_nega, zi_posi, xi_sqre, ei_sqre, zi_sqre,  &
!$omp&                    an1,an2,an3,an4,an5,an6,an7,an8,an9,an10,     &
!$omp&                    an11,an12,an13,an14,an15,an16,an17,an18,      &
!$omp&                    an19,an20,an21,an22,an23,an24,an25,an26,an27)
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
          i9  = ie(iele,9 )
          i10 = ie(iele,10)
          i11 = ie(iele,11)
          i12 = ie(iele,12)
          i13 = ie(iele,13)
          i14 = ie(iele,14)
          i15 = ie(iele,15)
          i16 = ie(iele,16)
          i17 = ie(iele,17)
          i18 = ie(iele,18)
          i19 = ie(iele,19)
          i20 = ie(iele,20)
          i21 = ie(iele,21)
          i22 = ie(iele,22)
          i23 = ie(iele,23)
          i24 = ie(iele,24)
          i25 = ie(iele,25)
          i26 = ie(iele,26)
          i27 = ie(iele,27)
!
          xi = xi_gauss(ig,1)
          ei = xi_gauss(ig,2)
          zi = xi_gauss(ig,3)
!
          xi_nega = one - xi
          xi_posi = one + xi
          xi_sqre = one - xi * xi
!
          ei_nega = one - ei
          ei_posi = one + ei
          ei_sqre = one - ei * ei
!
          zi_nega = one - zi
          zi_posi = one + zi
          zi_sqre = one - zi * zi
!
          an1  = r125 * xi_nega * ei_nega * zi_nega * xi * ei * zi
          an2  = r125 * xi_posi * ei_nega * zi_nega * xi * ei * zi
          an3  = r125 * xi_posi * ei_posi * zi_nega * xi * ei * zi
          an4  = r125 * xi_nega * ei_posi * zi_nega * xi * ei * zi
          an5  = r125 * xi_nega * ei_nega * zi_posi * xi * ei * zi
          an6  = r125 * xi_posi * ei_nega * zi_posi * xi * ei * zi
          an7  = r125 * xi_posi * ei_posi * zi_posi * xi * ei * zi
          an8  = r125 * xi_nega * ei_posi * zi_posi * xi * ei * zi
!
          an9  =  quad * xi_sqre * ei_nega * zi_nega * ei * zi
          an10 =  quad * xi_posi * ei_sqre * zi_nega * xi * zi
          an11 =  quad * xi_sqre * ei_posi * zi_nega * ei * zi
          an12 =  quad * xi_nega * ei_sqre * zi_nega * xi * zi
!
          an13 =  quad * xi_sqre * ei_nega * zi_posi * ei * zi
          an14 =  quad * xi_posi * ei_sqre * zi_posi * xi * zi
          an15 =  quad * xi_sqre * ei_posi * zi_posi * ei * zi
          an16 =  quad * xi_nega * ei_sqre * zi_posi * xi * zi
!
          an17 =  quad * xi_nega * ei_nega * zi_sqre * xi * ei
          an18 =  quad * xi_posi * ei_nega * zi_sqre * xi * ei
          an19 =  quad * xi_posi * ei_posi * zi_sqre * xi * ei
          an20 =  quad * xi_nega * ei_posi * zi_sqre * xi * ei
!
          an21 =   half * xi_nega * ei_sqre * zi_sqre * xi
          an22 =   half * xi_posi * ei_sqre * zi_sqre * xi
          an23 =   half * xi_sqre * ei_nega * zi_sqre * ei
          an24 =   half * xi_sqre * ei_posi * zi_sqre * ei
          an25 =   half * xi_sqre * ei_sqre * zi_nega * zi
          an26 =   half * xi_sqre * ei_sqre * zi_posi * zi
!
          an27 =        xi_sqre * ei_sqre * zi_sqre
!
!
          vect(ig)   =  an1  * v_org(i1 ) + an2  * v_org(i2 )           &
     &                + an3  * v_org(i3 ) + an4  * v_org(i4 )           &
     &                + an5  * v_org(i5 ) + an6  * v_org(i6 )           &
     &                + an7  * v_org(i7 ) + an8  * v_org(i8 )           &
     &                + an9  * v_org(i9 ) + an10 * v_org(i10)           &
     &                + an11 * v_org(i11) + an12 * v_org(i12)           &
     &                + an13 * v_org(i13) + an14 * v_org(i14)           &
     &                + an15 * v_org(i15) + an16 * v_org(i16)           &
     &                + an17 * v_org(i17) + an18 * v_org(i18)           &
     &                + an19 * v_org(i19) + an20 * v_org(i20)           &
     &                + an21 * v_org(i21) + an22 * v_org(i22)           &
     &                + an23 * v_org(i23) + an24 * v_org(i24)           &
     &                + an25 * v_org(i25) + an26 * v_org(i26)           &
     &                + an27 * v_org(i27)
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_scalar_ele27
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_scalar_surf9(np_smp, NP, v_org,             &
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
          vect(ig) =  AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 ) &
     &              + AM(ist+ 3) * v_org(i3 ) + AM(ist+ 4) * v_org(i4 ) &
     &              + AM(ist+ 5) * v_org(i5 ) + AM(ist+ 6) * v_org(i6 ) &
     &              + AM(ist+ 7) * v_org(i7 ) + AM(ist+ 8) * v_org(i8 ) &
     &              + AM(ist+ 9) * v_org(i9 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_scalar_surf9
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_scalar_ele27(np_smp, NP, v_org,             &
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
          vect(ig) =  AM(ist+ 1) * v_org(i1 ) + AM(ist+ 2) * v_org(i2 ) &
     &              + AM(ist+ 3) * v_org(i3 ) + AM(ist+ 4) * v_org(i4 ) &
     &              + AM(ist+ 5) * v_org(i5 ) + AM(ist+ 6) * v_org(i6 ) &
     &              + AM(ist+ 7) * v_org(i7 ) + AM(ist+ 8) * v_org(i8 ) &
     &              + AM(ist+ 9) * v_org(i9 ) + AM(ist+10) * v_org(i10) &
     &              + AM(ist+11) * v_org(i11) + AM(ist+12) * v_org(i12) &
     &              + AM(ist+13) * v_org(i13) + AM(ist+14) * v_org(i14) &
     &              + AM(ist+15) * v_org(i15) + AM(ist+16) * v_org(i16) &
     &              + AM(ist+17) * v_org(i17) + AM(ist+18) * v_org(i18) &
     &              + AM(ist+19) * v_org(i19) + AM(ist+20) * v_org(i20) &
     &              + AM(ist+21) * v_org(i21) + AM(ist+22) * v_org(i22) &
     &              + AM(ist+23) * v_org(i23) + AM(ist+24) * v_org(i24) &
     &              + AM(ist+25) * v_org(i25) + AM(ist+26) * v_org(i26) &
     &              + AM(ist+27) * v_org(i27)
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_scalar_ele27
!
! ----------------------------------------------------------------------
!
      end module interpolate_scalar_ele27
