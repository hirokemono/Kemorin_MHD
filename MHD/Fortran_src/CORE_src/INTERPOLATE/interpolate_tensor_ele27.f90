!
!     module interpolate_tensor_ele27
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_interpolate_tensor_ele27(np_smp, numnod, numele, ie,&
!     &          v_org, istack_smp, num_points, iele_gauss,             &
!     &          xi_gauss, vect)
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
      subroutine s_interpolate_tensor_ele27(np_smp, numnod, numele, ie, &
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
      real (kind=kreal), intent(in) :: v_org(6*numnod)
!
      real (kind=kreal), intent(inout) :: vect(6*num_points)
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
          vect(6*ig-5) =  an1  * v_org(6* i1-5) + an2  * v_org(6* i2-5) &
     &                  + an3  * v_org(6* i3-5) + an4  * v_org(6* i4-5) &
     &                  + an5  * v_org(6* i5-5) + an6  * v_org(6* i6-5) &
     &                  + an7  * v_org(6* i7-5) + an8  * v_org(6* i8-5) &
     &                  + an9  * v_org(6* i9-5) + an10 * v_org(6*i10-5) &
     &                  + an11 * v_org(6*i11-5) + an12 * v_org(6*i12-5) &
     &                  + an13 * v_org(6*i13-5) + an14 * v_org(6*i14-5) &
     &                  + an15 * v_org(6*i15-5) + an16 * v_org(6*i16-5) &
     &                  + an17 * v_org(6*i17-5) + an18 * v_org(6*i18-5) &
     &                  + an19 * v_org(6*i19-5) + an20 * v_org(6*i20-5) &
     &                  + an21 * v_org(6*i21-5) + an22 * v_org(6*i22-5) &
     &                  + an23 * v_org(6*i23-5) + an24 * v_org(6*i24-5) &
     &                  + an25 * v_org(6*i25-5) + an26 * v_org(6*i26-5) &
     &                  + an27 * v_org(6*i27-5)
!
          vect(6*ig-4) =  an1  * v_org(6* i1-4) + an2  * v_org(6* i2-4) &
     &                  + an3  * v_org(6* i3-4) + an4  * v_org(6* i4-4) &
     &                  + an5  * v_org(6* i5-4) + an6  * v_org(6* i6-4) &
     &                  + an7  * v_org(6* i7-4) + an8  * v_org(6* i8-4) &
     &                  + an9  * v_org(6* i9-4) + an10 * v_org(6*i10-4) &
     &                  + an11 * v_org(6*i11-4) + an12 * v_org(6*i12-4) &
     &                  + an13 * v_org(6*i13-4) + an14 * v_org(6*i14-4) &
     &                  + an15 * v_org(6*i15-4) + an16 * v_org(6*i16-4) &
     &                  + an17 * v_org(6*i17-4) + an18 * v_org(6*i18-4) &
     &                  + an19 * v_org(6*i19-4) + an20 * v_org(6*i20-4) &
     &                  + an21 * v_org(6*i21-4) + an22 * v_org(6*i22-4) &
     &                  + an23 * v_org(6*i23-4) + an24 * v_org(6*i24-4) &
     &                  + an25 * v_org(6*i25-4) + an26 * v_org(6*i26-4) &
     &                  + an27 * v_org(6*i27-4)
!
          vect(6*ig-3) =  an1  * v_org(6* i1-3) + an2  * v_org(6* i2-3) &
     &                  + an3  * v_org(6* i3-3) + an4  * v_org(6* i4-3) &
     &                  + an5  * v_org(6* i5-3) + an6  * v_org(6* i6-3) &
     &                  + an7  * v_org(6* i7-3) + an8  * v_org(6* i8-3) &
     &                  + an9  * v_org(6* i9-3) + an10 * v_org(6*i10-3) &
     &                  + an11 * v_org(6*i11-3) + an12 * v_org(6*i12-3) &
     &                  + an13 * v_org(6*i13-3) + an14 * v_org(6*i14-3) &
     &                  + an15 * v_org(6*i15-3) + an16 * v_org(6*i16-3) &
     &                  + an17 * v_org(6*i17-3) + an18 * v_org(6*i18-3) &
     &                  + an19 * v_org(6*i19-3) + an20 * v_org(6*i20-3) &
     &                  + an21 * v_org(6*i21-3) + an22 * v_org(6*i22-3) &
     &                  + an23 * v_org(6*i23-3) + an24 * v_org(6*i24-3) &
     &                  + an25 * v_org(6*i25-3) + an26 * v_org(6*i26-3) &
     &                  + an27 * v_org(6*i27-3)
!
          vect(6*ig-2) =  an1  * v_org(6* i1-2) + an2  * v_org(6* i2-2) &
     &                  + an3  * v_org(6* i3-2) + an4  * v_org(6* i4-2) &
     &                  + an5  * v_org(6* i5-2) + an6  * v_org(6* i6-2) &
     &                  + an7  * v_org(6* i7-2) + an8  * v_org(6* i8-2) &
     &                  + an9  * v_org(6* i9-2) + an10 * v_org(6*i10-2) &
     &                  + an11 * v_org(6*i11-2) + an12 * v_org(6*i12-2) &
     &                  + an13 * v_org(6*i13-2) + an14 * v_org(6*i14-2) &
     &                  + an15 * v_org(6*i15-2) + an16 * v_org(6*i16-2) &
     &                  + an17 * v_org(6*i17-2) + an18 * v_org(6*i18-2) &
     &                  + an19 * v_org(6*i19-2) + an20 * v_org(6*i20-2) &
     &                  + an21 * v_org(6*i21-2) + an22 * v_org(6*i22-2) &
     &                  + an23 * v_org(6*i23-2) + an24 * v_org(6*i24-2) &
     &                  + an25 * v_org(6*i25-2) + an26 * v_org(6*i26-2) &
     &                  + an27 * v_org(6*i27-2)
!
          vect(6*ig-1) =  an1  * v_org(6* i1-1) + an2  * v_org(6* i2-1) &
     &                  + an3  * v_org(6* i3-1) + an4  * v_org(6* i4-1) &
     &                  + an5  * v_org(6* i5-1) + an6  * v_org(6* i6-1) &
     &                  + an7  * v_org(6* i7-1) + an8  * v_org(6* i8-1) &
     &                  + an9  * v_org(6* i9-1) + an10 * v_org(6*i10-1) &
     &                  + an11 * v_org(6*i11-1) + an12 * v_org(6*i12-1) &
     &                  + an13 * v_org(6*i13-1) + an14 * v_org(6*i14-1) &
     &                  + an15 * v_org(6*i15-1) + an16 * v_org(6*i16-1) &
     &                  + an17 * v_org(6*i17-1) + an18 * v_org(6*i18-1) &
     &                  + an19 * v_org(6*i19-1) + an20 * v_org(6*i20-1) &
     &                  + an21 * v_org(6*i21-1) + an22 * v_org(6*i22-1) &
     &                  + an23 * v_org(6*i23-1) + an24 * v_org(6*i24-1) &
     &                  + an25 * v_org(6*i25-1) + an26 * v_org(6*i26-1) &
     &                  + an27 * v_org(6*i27-1)
!
          vect(6*ig  ) =  an1  * v_org(6* i1  ) + an2  * v_org(6* i2  ) &
     &                  + an3  * v_org(6* i3  ) + an4  * v_org(6* i4  ) &
     &                  + an5  * v_org(6* i5  ) + an6  * v_org(6* i6  ) &
     &                  + an7  * v_org(6* i7  ) + an8  * v_org(6* i8  ) &
     &                  + an9  * v_org(6* i9  ) + an10 * v_org(6*i10  ) &
     &                  + an11 * v_org(6*i11  ) + an12 * v_org(6*i12  ) &
     &                  + an13 * v_org(6*i13  ) + an14 * v_org(6*i14  ) &
     &                  + an15 * v_org(6*i15  ) + an16 * v_org(6*i16  ) &
     &                  + an17 * v_org(6*i17  ) + an18 * v_org(6*i18  ) &
     &                  + an19 * v_org(6*i19  ) + an20 * v_org(6*i20  ) &
     &                  + an21 * v_org(6*i21  ) + an22 * v_org(6*i22  ) &
     &                  + an23 * v_org(6*i23  ) + an24 * v_org(6*i24  ) &
     &                  + an25 * v_org(6*i25  ) + an26 * v_org(6*i26  ) &
     &                  + an27 * v_org(6*i27  )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_tensor_ele27
!
! ----------------------------------------------------------------------
!
      end module interpolate_tensor_ele27
