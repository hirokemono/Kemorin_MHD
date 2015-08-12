!cal_sph_4_refine.f90
!      module cal_sph_4_refine
!
!     Written by H. Matsui on Oct., 2007
!
!      subroutine cal_sph_on_edge_4_refine
!      subroutine cal_sph_on_surf_4_refine
!      subroutine cal_sph_on_ele_4_refine
!
!      subroutine cal_sph_xing_med_edge_refine(iedge)
!
      module cal_sph_4_refine
!
      use m_precision
!
      use m_constants
      use m_geometry_data
      use m_refined_node_id
!
      implicit none
!
      integer(kind = kint) :: inod1, inod2, inod3, inod4
      integer(kind = kint) :: inod5, inod6, inod7, inod8
      real(kind = kreal) :: pi
      real(kind = kreal) :: xi_nega, xi_posi
      real(kind = kreal) :: ei_nega, ei_posi
      real(kind = kreal) :: zi_nega, zi_posi
      real(kind = kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
!
      private :: inod1, inod2, inod3, inod4
      private :: inod5, inod6, inod7, inod8
      private :: xi_nega, xi_posi, ei_nega, ei_posi, zi_nega, zi_posi
      private :: an1, an2, an3, an4, an5, an6, an7, an8
      private :: pi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sph_on_edge_4_refine
!
      integer(kind = kint) :: iedge, jst, jed, jnum
!
!
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
          sph_refine_edge(jnum,1) =  an1*radius(inod1)                  &
     &                             + an2*radius(inod2)
          sph_refine_edge(jnum,2) =  an1*colatitude(inod1)              &
     &                             + an2*colatitude(inod2)
          sph_refine_edge(jnum,3) =  an1*longitude(inod1)               &
     &                             + an2*longitude(inod2)
!
        end do
      end do
!
      end subroutine cal_sph_on_edge_4_refine
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sph_on_surf_4_refine
!
      integer(kind = kint) :: isurf, jst, jed, jnum
!
!
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
          sph_refine_surf(jnum,1) =  an1*radius(inod1)                  &
     &                             + an2*radius(inod2)                  &
     &                             + an3*radius(inod3)                  &
     &                             + an4*radius(inod4)
          sph_refine_surf(jnum,2) =  an1*colatitude(inod1)              &
     &                             + an2*colatitude(inod2)              &
     &                             + an3*colatitude(inod3)              &
     &                             + an4*colatitude(inod4)
          sph_refine_surf(jnum,3) =  an1*longitude(inod1)               &
     &                             + an2*longitude(inod2)               &
     &                             + an3*longitude(inod3)               &
     &                             + an4*longitude(inod4)
!
        end do
      end do
!
      end subroutine cal_sph_on_surf_4_refine
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sph_on_ele_4_refine
!
      integer(kind = kint) :: iele, jst, jed, jnum
!
!
      do iele = 1, ele1%numele
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
!
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
          sph_refine_ele(jnum,1) =   an1*radius(inod1)                  &
     &                             + an2*radius(inod2)                  &
     &                             + an3*radius(inod3)                  &
     &                             + an4*radius(inod4)                  &
     &                             + an5*radius(inod5)                  &
     &                             + an6*radius(inod6)                  &
     &                             + an7*radius(inod7)                  &
     &                             + an8*radius(inod8)
          sph_refine_ele(jnum,2) =   an1*colatitude(inod1)              &
     &                             + an2*colatitude(inod2)              &
     &                             + an3*colatitude(inod3)              &
     &                             + an4*colatitude(inod4)              &
     &                             + an4*colatitude(inod5)              &
     &                             + an4*colatitude(inod6)              &
     &                             + an4*colatitude(inod7)              &
     &                             + an4*colatitude(inod8)
          sph_refine_ele(jnum,3) =   an1*longitude(inod1)               &
     &                             + an2*longitude(inod2)               &
     &                             + an3*longitude(inod3)               &
     &                             + an4*longitude(inod4)               &
     &                             + an4*longitude(inod5)               &
     &                             + an4*longitude(inod6)               &
     &                             + an4*longitude(inod7)               &
     &                             + an4*longitude(inod8)
!
        end do
      end do
!
      end subroutine cal_sph_on_ele_4_refine
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_sph_xing_med_edge_refine(iedge)
!
      integer(kind = kint), intent(in) :: iedge
      integer(kind = kint) :: jst, jed, jnum
      real(kind= kreal) :: phi_tmp1, phi_tmp2
!
!
        pi = four*atan(one)
!
        inod1 = ie_edge(iedge,1)
        inod2 = ie_edge(iedge,2)
!
        if (longitude(inod1) .lt. longitude(inod2)) then
          phi_tmp1 = longitude(inod1) + two*pi
          phi_tmp2 = longitude(inod2)
        else
          phi_tmp1 = longitude(inod1)
          phi_tmp2 = longitude(inod2) + two*pi
        end if
!
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
          sph_refine_edge(jnum,3) =  an1*phi_tmp1 + an2*phi_tmp2
          sph_refine_edge(jnum,3)                                       &
     &                  =  mod(sph_refine_edge(jnum,3),(two*pi))
!
        end do
!
      end subroutine cal_sph_xing_med_edge_refine
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sph_xing_med_surf_refine(isurf, phi_max)
!
      integer(kind = kint), intent(in) :: isurf
      real(kind = kreal), intent(in) :: phi_max
      integer(kind = kint) :: jst, jed, jnum
      real(kind= kreal) :: phi_tmp1, phi_tmp2, phi_tmp3, phi_tmp4
!
!
        pi = four*atan(one)
!
        inod1 = ie_surf(isurf,1)
        inod2 = ie_surf(isurf,2)
        inod3 = ie_surf(isurf,3)
        inod4 = ie_surf(isurf,4)
!
        phi_tmp1 = phi_max - longitude(inod1)
        if ( phi_tmp1 .ge. pi) then
          phi_tmp1 = longitude(inod1) + two*pi
        else
          phi_tmp1 = longitude(inod1)
        end if
        phi_tmp2 = phi_max - longitude(inod1)
        if ( phi_tmp2 .ge. pi) then
          phi_tmp2 = longitude(inod2) + two*pi
        else
          phi_tmp2 = longitude(inod2)
        end if
        phi_tmp3 = phi_max - longitude(inod1)
        if ( phi_tmp2 .ge. pi) then
          phi_tmp3 = longitude(inod3) + two*pi
        else
          phi_tmp3 = longitude(inod3)
        end if
        phi_tmp4 = phi_max - longitude(inod1)
        if ( phi_tmp4 .ge. pi) then
          phi_tmp4 = longitude(inod4) + two*pi
        else
          phi_tmp4 = longitude(inod4)
        end if
!
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
          sph_refine_surf(jnum,3) =  an1*phi_tmp1 + an2*phi_tmp2        &
     &                             + an3*phi_tmp3 + an4*phi_tmp4
          sph_refine_surf(jnum,3)                                       &
     &                  =  mod(sph_refine_surf(jnum,3),(two*pi))
!
        end do
!
      end subroutine cal_sph_xing_med_surf_refine
!
!  ---------------------------------------------------------------------
!
      end module cal_sph_4_refine
