!
!     module interpolate_fields_ele8
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_interpolate_fields_ele8(np_smp, numnod, numele, ie, &
!     &          numdir, v_org, istack_smp, num_points, iele_gauss,     &
!     &          xi_gauss, vect)
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
      subroutine s_interpolate_fields_ele8(np_smp, numnod, numele, ie,  &
     &          numdir, v_org, istack_smp, num_points, iele_gauss,      &
     &          xi_gauss, vect)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, numdir
      integer (kind = kint), intent(in) :: ie(numele,8)
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
!
      real (kind=kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, i2, i3, i4, i5, i6, i7, i8
!
      integer (kind = kint) :: ig, nd
!
!
!$omp parallel do private(ist,ied,ig,iele,i1,i2,i3,i4,i5,i6,i7,i8,      &
!$omp&                    xi,ei,zi, xi_nega, xi_posi, ei_nega, ei_posi, &
!$omp&                    zi_nega, zi_posi, an1,an2,an3,an4,an5,an6,    &
!$omp&                    an7,an8,nd)
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
            vect(numdir*(ig-1)+nd)                                      &
     &                         =   an1 * v_org( (numdir*( i1-1)+nd) )   &
     &                          +  an2 * v_org( (numdir*( i2-1)+nd) )   &
     &                          +  an3 * v_org( (numdir*( i3-1)+nd) )   &
     &                          +  an4 * v_org( (numdir*( i4-1)+nd) )   &
     &                          +  an5 * v_org( (numdir*( i5-1)+nd) )   &
     &                          +  an6 * v_org( (numdir*( i6-1)+nd) )   &
     &                          +  an7 * v_org( (numdir*( i7-1)+nd) )   &
     &                          +  an8 * v_org( (numdir*( i8-1)+nd) )
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_fields_ele8
!
! ----------------------------------------------------------------------
!
      end module interpolate_fields_ele8
