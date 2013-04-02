!
!     module interpolate_tensor_surf4
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_tensor_surf4(np_smp, numnod, numele, ie,&
!     &          v_org, istack_smp, num_points, iele_gauss, isurf_gauss,&
!     &          xi_gauss, vect)
!
      module interpolate_tensor_surf4
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
      subroutine s_interpolate_tensor_surf4(np_smp, numnod, numele, ie, &
     &          v_org, istack_smp, num_points, iele_gauss, isurf_gauss, &
     &          xi_gauss, vect)
!
      use m_constants
      use m_geometry_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,8)
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
!
      real (kind=kreal) :: an1, an2, an3, an4
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, i2, i3, i4
      integer (kind = kint) :: isf, k1, k2, k3, k4
      integer (kind = kint) :: ig, ld1, ld2
!
!
!$omp parallel do                                                       &
!$omp& private(ist,ied,ig,iele,isf,ld1,ld2,                             &
!$omp&         k1, k2, k3, k4, i1, i2, i3, i4, an1,an2,an3,an4,         &
!$omp&         xi,ei,xi_nega,xi_posi,ei_nega,ei_posi)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
!
          iele = iele_gauss(ig)
          isf =  mod(isurf_gauss(ig),100)
!
          k1 = node_on_sf_4(1,isf)
          k2 = node_on_sf_4(2,isf)
          k3 = node_on_sf_4(3,isf)
          k4 = node_on_sf_4(4,isf)
!
          ld1 = ishape_dir_surf(1,isf)
          ld2 = ishape_dir_surf(2,isf)
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
          vect(6*ig-5) =  an1  * v_org(6* i1-5) + an2  * v_org(6* i2-5) &
     &                  + an3  * v_org(6* i3-5) + an4  * v_org(6* i4-5)
!
          vect(6*ig-4) =  an1  * v_org(6* i1-4) + an2  * v_org(6* i2-4) &
     &                  + an3  * v_org(6* i3-4) + an4  * v_org(6* i4-4)
!
          vect(6*ig-3) =  an1  * v_org(6* i1-3) + an2  * v_org(6* i2-3) &
     &                  + an3  * v_org(6* i3-3) + an4  * v_org(6* i4-3)
!
          vect(6*ig-2) =  an1  * v_org(6* i1-2) + an2  * v_org(6* i2-2) &
     &                  + an3  * v_org(6* i3-2) + an4  * v_org(6* i4-2)
!
          vect(6*ig-1) =  an1  * v_org(6* i1-1) + an2  * v_org(6* i2-1) &
     &                  + an3  * v_org(6* i3-1) + an4  * v_org(6* i4-1)
!
          vect(6*ig  ) =  an1  * v_org(6* i1  ) + an2  * v_org(6* i2  ) &
     &                  + an3  * v_org(6* i3  ) + an4  * v_org(6* i4  )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_tensor_surf4
!
! ----------------------------------------------------------------------
!
      end module interpolate_tensor_surf4
