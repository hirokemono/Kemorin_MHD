!
!     module interpolate_fields_surf4
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_interpolate_fields_surf4(np_smp, numnod, numele, ie,&
!     &          numdir, v_org, istack_smp, num_points, iele_gauss,     &
!     &          isurf_gauss, xi_gauss, vect)
!
      module interpolate_fields_surf4
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
      subroutine s_interpolate_fields_surf4(np_smp, numnod, numele, ie, &
     &          numdir, v_org, istack_smp, num_points, iele_gauss,      &
     &          isurf_gauss, xi_gauss, vect)
!
      use m_constants
      use m_geometry_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, numdir
      integer (kind = kint), intent(in) :: ie(numele,8)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: isurf_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numdir*numnod)
!
      real (kind=kreal), intent(inout) :: vect(numdir*num_points)
!
      real (kind=kreal) :: xi, ei
      real (kind=kreal) :: xi_nega, ei_nega
      real (kind=kreal) :: xi_posi, ei_posi
!
      real (kind=kreal) :: an1, an2, an3, an4
!
      integer (kind = kint) :: ip, ist, ied, ld1, ld2
      integer (kind = kint) :: iele, i1, i2, i3, i4
      integer (kind = kint) :: isf, k1, k2, k3, k4
!
      integer (kind = kint) :: ig, nd
!
!
!$omp parallel do                                                       &
!$omp& private(ist,ied,ig,nd,iele,isf,ld1,ld2,                          &
!$omp&         k1, k2, k3, k4, i1, i2, i3, i4, an1,an2,an3,an4,         &
!$omp&         xi,ei,xi_nega,xi_posi,ei_nega,ei_posi)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do nd = 1, numdir
          do ig = ist, ied
!
            iele = iele_gauss(ig)
            isf =  mod(isurf_gauss(ig),100)
!
            ld1 = ishape_dir_surf(1,isf)
            ld2 = ishape_dir_surf(2,isf)
!
            k1 = node_on_sf_4(1,isf)
            k2 = node_on_sf_4(2,isf)
            k3 = node_on_sf_4(3,isf)
            k4 = node_on_sf_4(4,isf)
!
            i1 = ie(iele,k1)
            i2 = ie(iele,k2)
            i3 = ie(iele,k3)
            i4 = ie(iele,k4)
!
            xi = xi_gauss(ig,ld1)
            ei = xi_gauss(ig,ld2)
!
            xi_nega = one - xi
            xi_posi = one + xi
!
            ei_nega = one - ei
            ei_posi = one + ei
!
            an1 = quad * xi_nega * ei_nega
            an2 = quad * xi_posi * ei_nega
            an3 = quad * xi_posi * ei_posi
            an4 = quad * xi_nega * ei_posi
!
!
            vect(numdir*(ig-1)+nd)                                      &
     &                         =   an1 * v_org( (numdir*( i1-1)+nd) )   &
     &                          +  an2 * v_org( (numdir*( i2-1)+nd) )   &
     &                          +  an3 * v_org( (numdir*( i3-1)+nd) )   &
     &                          +  an4 * v_org( (numdir*( i4-1)+nd) )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_fields_surf4
!
! ----------------------------------------------------------------------
!
      end module interpolate_fields_surf4
