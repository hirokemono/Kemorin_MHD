!cal_refined_nod_near_pole.f90
!      module cal_refined_nod_near_pole
!
!     Written by H. Matsui on Oct., 2007
!
!!      subroutine s_cal_refined_nod_near_pole                          &
!!     &         (numnod, numsurf, numedge, ie_surf, ie_edge, longitude)
!
      module cal_refined_nod_near_pole
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_refined_nod_near_pole                            &
     &         (numnod, numsurf, numedge, ie_surf, ie_edge, longitude)
!
      use m_constants
      use m_geometry_constants
      use cal_sph_4_refine
!
      integer(kind=kint), intent(in) :: numnod, numsurf, numedge
      integer(kind=kint), intent(in) :: ie_surf(numsurf,num_linear_sf)
      integer(kind=kint), intent(in)                                    &
     &                   :: ie_edge(numedge,num_linear_edge)
      real(kind = kreal), intent(in) :: longitude(numnod)
!
      integer(kind = kint) :: isurf, iedge
      integer(kind = kint) :: inod, inod1, inod2, k1
      real(kind = kreal) :: diff, phi_min, phi_max
      real(kind = kreal) :: pi
!
!
      pi = four * atan (one)
!
      do iedge = 1, numedge
        inod1 = ie_edge(iedge,1)
        inod2 = ie_edge(iedge,2)
!
        diff = abs( longitude(inod1) - longitude(inod2) )
!
        if( diff .ge. pi ) then
          call cal_sph_xing_med_edge_refine(numnod, numedge,            &
     &        ie_edge, longitude, iedge)
        end if
      end do
!
      do isurf = 1, numsurf
        phi_max = 0.0d0
        phi_min = two*pi
        do k1 = 1, 4
          inod = ie_surf(isurf,k1)
          phi_max = max(phi_max,longitude(inod))
          phi_min = min(phi_min,longitude(inod))
        end do
        diff = abs(phi_max - phi_min)
!
        if( diff .ge. pi ) then
          call cal_sph_xing_med_surf_refine(numnod, numsurf,            &
     &        ie_surf, longitude, isurf, phi_max)
        end if
      end do
!
!
!      do iele = 1, numele
!        phi_max = 0.0d0
!        phi_min = two*pi
!        do k1 = 1, 8
!          inod = ie(iele,k1)
!          phi_max = max(phi_max,longitude(inod))
!          phi_min = min(phi_min,longitude(inod))
!        end do
!        diff = abs(phi_max - phi_min)
!
!        if( diff .ge. pi ) then
!          call cal_xyz_on_1ele_4_refine(numnod, numele, ie, xx, iele)
!        end if
!      end do
!
      end subroutine s_cal_refined_nod_near_pole
!
!  ---------------------------------------------------------------------
!
      end module cal_refined_nod_near_pole
