!
!     module interpolate_fields_ele20
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_interpolate_fields_ele20(np_smp, numnod, numele, ie,&
!     &          numdir, v_org, istack_smp, num_points, iele_gauss,     &
!     &          xi_gauss, vect)
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
      subroutine s_interpolate_fields_ele20(np_smp, numnod, numele, ie, &
     &          numdir, v_org, istack_smp, num_points, iele_gauss,      &
     &          xi_gauss, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, numdir
      integer (kind = kint), intent(in) :: ie(numele,20)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numdir*numnod)
!
      real (kind=kreal), intent(inout) :: vect(numdir*num_points)
!
      real (kind=kreal) :: xi, ei, zi
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal) :: xi_sqre, ei_sqre, zi_sqre
!
      real (kind=kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
      real (kind=kreal) :: an9,  an10, an11, an12, an13, an14
      real (kind=kreal) :: an15, an16, an17, an18, an19, an20
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, i2, i3, i4, i5, i6, i7, i8
      integer (kind = kint) :: i9, i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20
!
      integer (kind = kint) :: ig, nd
!
!
!$omp parallel do private(ist,ied,ig,iele,i1,i2,i3,i4,i5,i6,i7,i8,i9,   &
!$omp&                    i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,i20,  &
!$omp&                    xi,ei,zi,xi_nega, xi_posi, ei_nega, ei_posi,  &
!$omp&                    zi_nega, zi_posi, xi_sqre, ei_sqre, zi_sqre,  &
!$omp&                    an1,an2,an3,an4,an5,an6,an7,an8,              &
!$omp&                    an9,an10,an11,an12,an13,an14,an15,an16,       &
!$omp&                    an17,an18,an19,an20,nd)

      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do nd = 1, numdir
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
            an1  = r125 * xi_nega * ei_nega * zi_nega * (-xi-ei-zi-two)
            an2  = r125 * xi_posi * ei_nega * zi_nega * ( xi-ei-zi-two)
            an3  = r125 * xi_posi * ei_posi * zi_nega * ( xi+ei-zi-two)
            an4  = r125 * xi_nega * ei_posi * zi_nega * (-xi+ei-zi-two)
            an5  = r125 * xi_nega * ei_nega * zi_posi * (-xi-ei+zi-two)
            an6  = r125 * xi_posi * ei_nega * zi_posi * ( xi-ei+zi-two)
            an7  = r125 * xi_posi * ei_posi * zi_posi * ( xi+ei+zi-two)
            an8  = r125 * xi_nega * ei_posi * zi_posi * (-xi+ei+zi-two)
!
            an9  =  quad * xi_sqre * ei_nega * zi_nega
            an10 =  quad * xi_posi * ei_sqre * zi_nega
            an11 =  quad * xi_sqre * ei_posi * zi_nega
            an12 =  quad * xi_nega * ei_sqre * zi_nega
!
            an13 =  quad * xi_sqre * ei_nega * zi_posi
            an14 =  quad * xi_posi * ei_sqre * zi_posi
            an15 =  quad * xi_sqre * ei_posi * zi_posi
            an16 =  quad * xi_nega * ei_sqre * zi_posi
!
            an17 =  quad * xi_nega * ei_nega * zi_sqre
            an18 =  quad * xi_posi * ei_nega * zi_sqre
            an19 =  quad * xi_posi * ei_posi * zi_sqre
            an20 =  quad * xi_nega * ei_posi * zi_sqre
!
!
            vect(numdir*(ig-1)+nd)                                      &
     &                         =   an1 * v_org( (numdir*( i1-1)+nd) )   &
     &                          +  an2 * v_org( (numdir*( i2-1)+nd) )   &
     &                          +  an3 * v_org( (numdir*( i3-1)+nd) )   &
     &                          +  an4 * v_org( (numdir*( i4-1)+nd) )   &
     &                          +  an5 * v_org( (numdir*( i5-1)+nd) )   &
     &                          +  an6 * v_org( (numdir*( i6-1)+nd) )   &
     &                          +  an7 * v_org( (numdir*( i7-1)+nd) )   &
     &                          +  an8 * v_org( (numdir*( i8-1)+nd) )   &
     &                          +  an9 * v_org( (numdir*( i9-1)+nd) )   &
     &                          + an10 * v_org( (numdir*(i10-1)+nd) )   &
     &                          + an11 * v_org( (numdir*(i11-1)+nd) )   &
     &                          + an12 * v_org( (numdir*(i12-1)+nd) )   &
     &                          + an13 * v_org( (numdir*(i13-1)+nd) )   &
     &                          + an14 * v_org( (numdir*(i14-1)+nd) )   &
     &                          + an15 * v_org( (numdir*(i15-1)+nd) )   &
     &                          + an16 * v_org( (numdir*(i16-1)+nd) )   &
     &                          + an17 * v_org( (numdir*(i17-1)+nd) )   &
     &                          + an18 * v_org( (numdir*(i18-1)+nd) )   &
     &                          + an19 * v_org( (numdir*(i19-1)+nd) )   &
     &                          + an20 * v_org( (numdir*(i20-1)+nd) )
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_fields_ele20
!
! ----------------------------------------------------------------------
!
      end module interpolate_fields_ele20
