!
!     module interpolate_fields_edge3
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_interpolate_fields_edge3(np_smp, numnod, numele, ie,&
!     &          numdir, v_org, istack_smp, num_points, iele_gauss,     &
!     &          iedge_gauss, xi_gauss, vect)
!
      module interpolate_fields_edge3
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
      subroutine s_interpolate_fields_edge3(np_smp, numnod, numele, ie, &
     &          numdir, v_org, istack_smp, num_points, iele_gauss,      &
     &          iedge_gauss, xi_gauss, vect)
!
      use m_constants
      use m_geometry_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, numdir
      integer (kind = kint), intent(in) :: ie(numele,20)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: iedge_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numdir*numnod)
!
      real (kind=kreal), intent(inout) :: vect(numdir*num_points)
!
      real (kind=kreal) :: xi, xi_nega, xi_posi, xi_sqre
!
      real (kind=kreal) :: an1, an2, an3
!
      integer (kind = kint) :: ip, ist, ied, ld1
      integer (kind = kint) :: iele, i1, i2, i3
      integer (kind = kint) :: iedge, k1, k2, k3
!
      integer (kind = kint) :: ig, nd
!
!
!$omp parallel do                                                       &
!$omp& private(ist,ied,ig,nd,iele,iedge,ld1,k1, k2, k3,i1, i2, i3,      &
!$omp&         an1,an2,an3,xi,xi_nega,xi_posi,xi_sqre)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
!
        do nd = 1, numdir
          do ig = ist, ied
!
            iele = iele_gauss(ig)
            iedge =  mod(iedge_gauss(ig),100)
!
            ld1 = ishape_dir_edge(iedge)
!
            k1 = node_on_edge_q(1,iedge)
            k2 = node_on_edge_q(2,iedge)
            k3 = node_on_edge_q(3,iedge)
!
            i1 = ie(iele,k1)
            i2 = ie(iele,k2)
            i3 = ie(iele,k3)
!
            xi = xi_gauss(ig,ld1)
!
            xi_nega = one - xi
            xi_posi = one + xi
            xi_sqre = one - xi * xi
!
            an1 = -half * xi * xi_nega
            an2 =              xi_sqre
            an3 =  half * xi * xi_posi
!
!
            vect(numdir*(ig-1)+nd)                                      &
     &                         =   an1 * v_org( (numdir*( i1-1)+nd) )   &
     &                          +  an2 * v_org( (numdir*( i2-1)+nd) )   &
     &                          +  an3 * v_org( (numdir*( i3-1)+nd) )
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_fields_edge3
!
! ----------------------------------------------------------------------
!
      end module interpolate_fields_edge3
