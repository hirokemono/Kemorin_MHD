!cal_xyz_4_refine.f90
!      module cal_xyz_4_refine
!
!     Written by H. Matsui on Oct., 2007
!
!!      subroutine cal_xyz_on_edge_4_refine                             &
!!     &         (numnod, numedge, ie_edge, xx,                         &
!!     &          ntot_nod_refine_edge, istack_nod_refine_edge,         &
!!     &          xi_refine_edge, x_refine_edge)
!!      subroutine cal_xyz_on_surf_4_refine                             &
!!     &         (numnod, numsurf, ie_surf, xx,                         &
!!     &          ntot_nod_refine_surf, istack_nod_refine_surf,         &
!!     &          xi_refine_surf, x_refine_surf)
!!      subroutine cal_xyz_on_ele_4_refine(numnod, numele, ie, xx,      &
!!     &          ntot_nod_refine_ele, istack_nod_refine_ele,           &
!!     &          xi_refine_ele, x_refine_ele)
!
      module cal_xyz_4_refine
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_xyz_on_edge_4_refine                               &
     &         (numnod, numedge, ie_edge, xx,                           &
     &          ntot_nod_refine_edge, istack_nod_refine_edge,           &
     &          xi_refine_edge, x_refine_edge)
!
      integer(kind=kint), intent(in) :: numnod, numedge
      integer(kind=kint), intent(in)                                    &
     &                   :: ie_edge(numedge,num_linear_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: ntot_nod_refine_edge
      integer(kind = kint), intent(in)                                  &
     &           :: istack_nod_refine_edge(0:numedge)
      real(kind = kreal), intent(in)                                    &
     &           :: xi_refine_edge(ntot_nod_refine_edge,3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: x_refine_edge(ntot_nod_refine_edge,3)
!
      integer(kind = kint) :: iedge, jst, jed, jnum
      integer(kind = kint) :: inod1, inod2
      real(kind = kreal) :: xi_nega, xi_posi
      real(kind = kreal) :: an1, an2
!
!
!$omp parallel do private(iedge,jst,jed,jnum,inod1,inod2,               &
!$omp&           an1,an2,xi_nega,xi_posi)
      do iedge = 1, numedge
        inod1 = ie_edge(iedge,1)
        inod2 = ie_edge(iedge,2)
        jst = istack_nod_refine_edge(iedge-1) + 1
        jed = istack_nod_refine_edge(iedge)
!
        do jnum = jst, jed
!
          xi_nega = one - xi_refine_edge(jnum,1)
          xi_posi = one + xi_refine_edge(jnum,1)
          an1 = half * xi_nega
          an2 = half * xi_posi
!
          x_refine_edge(jnum,1) = an1*xx(inod1,1) + an2*xx(inod2,1)
          x_refine_edge(jnum,2) = an1*xx(inod1,2) + an2*xx(inod2,2)
          x_refine_edge(jnum,3) = an1*xx(inod1,3) + an2*xx(inod2,3)
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xyz_on_edge_4_refine
!
!  ---------------------------------------------------------------------
!
      subroutine cal_xyz_on_surf_4_refine                               &
     &         (numnod, numsurf, ie_surf, xx,                           &
     &          ntot_nod_refine_surf, istack_nod_refine_surf,           &
     &          xi_refine_surf, x_refine_surf)
!
      integer(kind=kint), intent(in) :: numnod, numsurf
      integer(kind=kint), intent(in) :: ie_surf(numsurf,num_linear_sf)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: ntot_nod_refine_surf
      integer(kind = kint), intent(in)                                  &
     &           :: istack_nod_refine_surf(0:numsurf)
      real(kind = kreal), intent(in)                                    &
     &           :: xi_refine_surf(ntot_nod_refine_surf,3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: x_refine_surf(ntot_nod_refine_surf,3)
!
      integer(kind = kint) :: isurf, jst, jed, jnum
      integer(kind = kint) :: inod1, inod2, inod3, inod4
      real(kind = kreal) :: xi_nega, xi_posi
      real(kind = kreal) :: ei_nega, ei_posi
      real(kind = kreal) :: an1, an2, an3, an4
!
!
!$omp parallel do private(isurf,jst,jed,jnum,inod1,inod2,inod3,inod4,   &
!$omp&           an1,an2,an3,an4,xi_nega,xi_posi,ei_nega,ei_posi)
      do isurf = 1, numsurf
        inod1 = ie_surf(isurf,1)
        inod2 = ie_surf(isurf,2)
        inod3 = ie_surf(isurf,3)
        inod4 = ie_surf(isurf,4)
        jst = istack_nod_refine_surf(isurf-1) + 1
        jed = istack_nod_refine_surf(isurf)
!
        do jnum = jst, jed
!
          xi_nega = one - xi_refine_surf(jnum,1)
          xi_posi = one + xi_refine_surf(jnum,1)
          ei_nega = one - xi_refine_surf(jnum,2)
          ei_posi = one + xi_refine_surf(jnum,2)
          an1 = quad * xi_nega * ei_nega
          an2 = quad * xi_posi * ei_nega
          an3 = quad * xi_posi * ei_posi
          an4 = quad * xi_nega * ei_posi
!
          x_refine_surf(jnum,1) =  an1*xx(inod1,1) + an2*xx(inod2,1)    &
     &                           + an3*xx(inod3,1) + an4*xx(inod4,1)
          x_refine_surf(jnum,2) =  an1*xx(inod1,2) + an2*xx(inod2,2)    &
     &                           + an3*xx(inod3,2) + an4*xx(inod4,2)
          x_refine_surf(jnum,3) =  an1*xx(inod1,3) + an2*xx(inod2,3)    &
     &                           + an3*xx(inod3,3) + an4*xx(inod4,3)
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xyz_on_surf_4_refine
!
!  ---------------------------------------------------------------------
!
      subroutine cal_xyz_on_ele_4_refine(numnod, numele, ie, xx,        &
     &          ntot_nod_refine_ele, istack_nod_refine_ele,             &
     &          xi_refine_ele, x_refine_ele)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: ie(numele,num_t_linear)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: ntot_nod_refine_ele
      integer(kind = kint), intent(in)                                  &
     &           :: istack_nod_refine_ele(0:numele)
      real(kind = kreal), intent(in)                                    &
     &           :: xi_refine_ele(ntot_nod_refine_ele,3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: x_refine_ele(ntot_nod_refine_ele,3)
!
      integer(kind = kint) :: iele, jst, jed, jnum
      integer(kind = kint) :: inod1, inod2, inod3, inod4
      integer(kind = kint) :: inod5, inod6, inod7, inod8
      real(kind = kreal) :: xi_nega, xi_posi
      real(kind = kreal) :: ei_nega, ei_posi
      real(kind = kreal) :: zi_nega, zi_posi
      real(kind = kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
!
!
!$omp parallel do private(iele,jst,jed,jnum,                            &
!$omp&           inod1,inod2,inod3,inod4,inod5,inod6,inod7,inod8,       &
!$omp&           an1,an2,an3,an4,an5,an6,an7,an8,xi_nega,xi_posi,       &
!$omp&           ei_nega,ei_posi,zi_nega,zi_posi)
      do iele = 1, numele
        inod1 = ie(iele,1)
        inod2 = ie(iele,2)
        inod3 = ie(iele,3)
        inod4 = ie(iele,4)
        inod5 = ie(iele,5)
        inod6 = ie(iele,6)
        inod7 = ie(iele,7)
        inod8 = ie(iele,8)
        jst = istack_nod_refine_ele(iele-1) + 1
        jed = istack_nod_refine_ele(iele)
!
        do jnum = jst, jed
          xi_nega = one - xi_refine_ele(jnum,1)
          xi_posi = one + xi_refine_ele(jnum,1)
          ei_nega = one - xi_refine_ele(jnum,2)
          ei_posi = one + xi_refine_ele(jnum,2)
          zi_nega = one - xi_refine_ele(jnum,3)
          zi_posi = one + xi_refine_ele(jnum,3)
          an1 = r125 * xi_nega * ei_nega * zi_nega
          an2 = r125 * xi_posi * ei_nega * zi_nega
          an3 = r125 * xi_posi * ei_posi * zi_nega
          an4 = r125 * xi_nega * ei_posi * zi_nega
          an5 = r125 * xi_nega * ei_nega * zi_posi
          an6 = r125 * xi_posi * ei_nega * zi_posi
          an7 = r125 * xi_posi * ei_posi * zi_posi
          an8 = r125 * xi_nega * ei_posi * zi_posi
!
          x_refine_ele(jnum,1) =   an1*xx(inod1,1) + an2*xx(inod2,1)    &
     &                           + an3*xx(inod3,1) + an4*xx(inod4,1)    &
     &                           + an5*xx(inod5,1) + an6*xx(inod6,1)    &
     &                           + an7*xx(inod7,1) + an8*xx(inod8,1)
          x_refine_ele(jnum,2) =   an1*xx(inod1,2) + an2*xx(inod2,2)    &
     &                           + an3*xx(inod3,2) + an4*xx(inod4,2)    &
     &                           + an5*xx(inod5,2) + an6*xx(inod6,2)    &
     &                           + an7*xx(inod7,2) + an8*xx(inod8,2)
          x_refine_ele(jnum,3) =   an1*xx(inod1,3) + an2*xx(inod2,3)    &
     &                           + an3*xx(inod3,3) + an4*xx(inod4,3)    &
     &                           + an5*xx(inod5,3) + an6*xx(inod6,3)    &
     &                           + an7*xx(inod7,3) + an8*xx(inod8,3)
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xyz_on_ele_4_refine
!
!  ---------------------------------------------------------------------
!
      end module cal_xyz_4_refine
