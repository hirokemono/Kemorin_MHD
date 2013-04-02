!cal_r_4_refine.f90
!      module cal_r_4_refine
!
!     Written by H. Matsui on Oct., 2007
!
!      subroutine cal_r_on_edge_4_refine
!      subroutine cal_r_on_surf_4_refine
!      subroutine cal_r_on_ele_4_refine
!
      module cal_r_4_refine
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_refined_node_id
!
      implicit none
!
      integer(kind = kint) :: inod1, inod2, inod3, inod4
      integer(kind = kint) :: inod5, inod6, inod7, inod8
      real(kind = kreal) :: xi_nega, xi_posi
      real(kind = kreal) :: ei_nega, ei_posi
      real(kind = kreal) :: zi_nega, zi_posi
      real(kind = kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
!
      private :: inod1, inod2, inod3, inod4
      private :: inod5, inod6, inod7, inod8
      private :: xi_nega, xi_posi, ei_nega, ei_posi, zi_nega, zi_posi
      private :: an1, an2, an3, an4, an5, an6, an7, an8
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_r_on_edge_4_refine
!
      integer(kind = kint) :: iedge, jst, jed, jnum
      real(kind = kreal) :: flag1, flag2, flag3
!
!
      do iedge = 1, numedge
        inod1 = ie_edge(iedge,1)
        inod2 = ie_edge(iedge,2)
        jst = istack_nod_refine_edge(iedge-1) + 1
        jed = istack_nod_refine_edge(iedge)
!
        flag1 = xx(inod1,1)*xx(inod2,1)
        flag2 = xx(inod1,2)*xx(inod2,2)
        flag3 = xx(inod1,3)*xx(inod2,3)
!
        do jnum = jst, jed
!
          if ( flag1.le.0 .and. flag2.le.0 .and. flag3.le.0) then
            sph_refine_edge(jnum,1)                                     &
     &        = sqrt( x_refine_edge(jnum,1)*x_refine_edge(jnum,1)       &
     &              + x_refine_edge(jnum,2)*x_refine_edge(jnum,2)       &
     &              + x_refine_edge(jnum,3)*x_refine_edge(jnum,3) )
          else
            xi_nega = one - xi_refine_edge(jnum,1)
            xi_posi = one + xi_refine_edge(jnum,1)
            an1 = half * xi_nega
            an2 = half * xi_posi
!
            sph_refine_edge(jnum,1) =  an1*radius(inod1)                &
     &                               + an2*radius(inod2)
          end if
!
        end do
      end do
!
      end subroutine cal_r_on_edge_4_refine
!
!  ---------------------------------------------------------------------
!
      subroutine cal_r_on_surf_4_refine
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
!
        end do
      end do
!
      end subroutine cal_r_on_surf_4_refine
!
!  ---------------------------------------------------------------------
!
      subroutine cal_r_on_ele_4_refine
!
      integer(kind = kint) :: iele, jst, jed, jnum
!
!
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
          sph_refine_ele(jnum,1) =  an1*radius(inod1)                   &
     &                             + an2*radius(inod2)                  &
     &                             + an3*radius(inod3)                  &
     &                             + an4*radius(inod4)                  &
     &                             + an5*radius(inod5)                  &
     &                             + an6*radius(inod6)                  &
     &                             + an7*radius(inod7)                  &
     &                             + an8*radius(inod8)
!
        end do
      end do
!
      end subroutine cal_r_on_ele_4_refine
!
!  ---------------------------------------------------------------------
!
      end module cal_r_4_refine
