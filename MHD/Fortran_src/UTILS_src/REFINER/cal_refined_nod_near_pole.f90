!cal_refined_nod_near_pole.f90
!      module cal_refined_nod_near_pole
!
      module cal_refined_nod_near_pole
!
!     Written by H. Matsui on Oct., 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_cal_refined_nod_near_pole
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_refined_nod_near_pole
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use cal_xyz_4_refine
      use cal_sph_4_refine
!
      integer(kind = kint) :: iele, isurf, iedge
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
          call cal_sph_xing_med_edge_refine(iedge)
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
          call cal_sph_xing_med_surf_refine(isurf, phi_max)
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
!          call cal_xyz_on_1ele_4_refine(iele, phi_min, phi_max)
!        end if
!      end do
!
      end subroutine s_cal_refined_nod_near_pole
!
!  ---------------------------------------------------------------------
!
      end module cal_refined_nod_near_pole
