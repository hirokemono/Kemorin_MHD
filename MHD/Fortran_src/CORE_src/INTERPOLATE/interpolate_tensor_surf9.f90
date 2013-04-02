!
!     module interpolate_tensor_surf9
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_tensor_surf9(np_smp, numnod, numele, ie,&
!     &          v_org, istack_smp, num_points, iele_gauss, isurf_gauss,&
!     &          xi_gauss, vect)
!
      module interpolate_tensor_surf9
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_tensor_surf9(np_smp, numnod, numele, ie, &
     &          v_org, istack_smp, num_points, iele_gauss, isurf_gauss, &
     &          xi_gauss, vect)
!
      use m_constants
      use m_geometry_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,27)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: isurf_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(6*numnod)
!
      real (kind=kreal), intent(inout) :: vect(6*num_points)
!
      real (kind=kreal) :: xi, ei
      real (kind=kreal) :: xi_nega, ei_nega
      real (kind=kreal) :: xi_posi, ei_posi
      real (kind=kreal) :: xi_sqre, ei_sqre
!
      real (kind=kreal) :: an1, an2, an3, an4, an5, an6, an7, an8, an9
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, i2, i3, i4, i5, i6, i7, i8, i9
      integer (kind = kint) :: isf, k1, k2, k3, k4, k5, k6, k7, k8, k9
      integer (kind = kint) :: ig, ld1, ld2
!
!
!$omp parallel do                                                       &
!$omp& private(ist,ied,ig,iele,isf,ld1,ld2,                             &
!$omp&         k1, k2, k3, k4, k5, k6, k7, k8, k9,                      &
!$omp&         i1, i2, i3, i4, i5, i6, i7, i8, i9,                      &
!$omp&         an1,an2,an3,an4,an5,an6,an7,an8,an9,                     &
!$omp&         xi,ei,xi_nega,xi_posi,xi_sqre,ei_nega,ei_posi,ei_sqre)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
!
          iele = iele_gauss(ig)
          isf =  mod(isurf_gauss(ig),100)
!
          k1 = node_on_sf_9(1,isf)
          k2 = node_on_sf_9(2,isf)
          k3 = node_on_sf_9(3,isf)
          k4 = node_on_sf_9(4,isf)
          k5 = node_on_sf_9(5,isf)
          k6 = node_on_sf_9(6,isf)
          k7 = node_on_sf_9(7,isf)
          k8 = node_on_sf_9(8,isf)
          k9 = node_on_sf_9(9,isf)
!
          ld1 = ishape_dir_surf(1,isf)
          ld2 = ishape_dir_surf(2,isf)
!
          i1 = ie(iele,k1)
          i2 = ie(iele,k2)
          i3 = ie(iele,k3)
          i4 = ie(iele,k4)
          i5 = ie(iele,k5)
          i6 = ie(iele,k6)
          i7 = ie(iele,k7)
          i8 = ie(iele,k8)
          i9 = ie(iele,k9)
!
          xi = xi_gauss(ig,ld1)
          ei = xi_gauss(ig,ld2)
!
          xi_nega = one - xi
          xi_posi = one + xi
          xi_sqre = one - xi*xi
!
          ei_nega = one - ei
          ei_posi = one + ei
          ei_sqre = one - ei*ei
!
          an1 = quad * xi_nega * ei_nega * xi * ei
          an2 = quad * xi_posi * ei_nega * xi * ei
          an3 = quad * xi_posi * ei_posi * xi * ei
          an4 = quad * xi_nega * ei_posi * xi * ei
!
          an5 =  half * xi_sqre * ei_nega * ei
          an6 =  half * xi_posi * ei_sqre * xi
          an7 =  half * xi_sqre * ei_posi * ei
          an8 =  half * xi_nega * ei_sqre * xi
!
          an9 =       xi_sqre * ei_sqre
!
!
          vect(6*ig-5) =  an1  * v_org(6* i1-5) + an2  * v_org(6* i2-5) &
     &                  + an3  * v_org(6* i3-5) + an4  * v_org(6* i4-5) &
     &                  + an5  * v_org(6* i5-5) + an6  * v_org(6* i6-5) &
     &                  + an7  * v_org(6* i7-5) + an8  * v_org(6* i8-5) &
     &                  + an9  * v_org(6* i9-5)
!
          vect(6*ig-4) =  an1  * v_org(6* i1-4) + an2  * v_org(6* i2-4) &
     &                  + an3  * v_org(6* i3-4) + an4  * v_org(6* i4-4) &
     &                  + an5  * v_org(6* i5-4) + an6  * v_org(6* i6-4) &
     &                  + an7  * v_org(6* i7-4) + an8  * v_org(6* i8-4) &
     &                  + an9  * v_org(6* i9-4)
!
          vect(6*ig-3) =  an1  * v_org(6* i1-3) + an2  * v_org(6* i2-3) &
     &                  + an3  * v_org(6* i3-3) + an4  * v_org(6* i4-3) &
     &                  + an5  * v_org(6* i5-3) + an6  * v_org(6* i6-3) &
     &                  + an7  * v_org(6* i7-3) + an8  * v_org(6* i8-3) &
     &                  + an9  * v_org(6* i9-3)
!
          vect(6*ig-2) =  an1  * v_org(6* i1-2) + an2  * v_org(6* i2-2) &
     &                  + an3  * v_org(6* i3-2) + an4  * v_org(6* i4-2) &
     &                  + an5  * v_org(6* i5-2) + an6  * v_org(6* i6-2) &
     &                  + an7  * v_org(6* i7-2) + an8  * v_org(6* i8-2) &
     &                  + an9  * v_org(6* i9-2)
!
          vect(6*ig-1) =  an1  * v_org(6* i1-1) + an2  * v_org(6* i2-1) &
     &                  + an3  * v_org(6* i3-1) + an4  * v_org(6* i4-1) &
     &                  + an5  * v_org(6* i5-1) + an6  * v_org(6* i6-1) &
     &                  + an7  * v_org(6* i7-1) + an8  * v_org(6* i8-1) &
     &                  + an9  * v_org(6* i9-1)
!
          vect(6*ig  ) =  an1  * v_org(6* i1  ) + an2  * v_org(6* i2  ) &
     &                  + an3  * v_org(6* i3  ) + an4  * v_org(6* i4  ) &
     &                  + an5  * v_org(6* i5  ) + an6  * v_org(6* i6  ) &
     &                  + an7  * v_org(6* i7  ) + an8  * v_org(6* i8  ) &
     &                  + an9  * v_org(6* i9  )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_tensor_surf9
!
! ----------------------------------------------------------------------
!
      end module interpolate_tensor_surf9
