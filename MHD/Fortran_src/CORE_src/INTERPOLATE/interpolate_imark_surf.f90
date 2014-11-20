!
!     module interpolate_imark_surf
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_imark_surf4(np_smp, numnod, numele, ie, &
!     &          imark_org, istack_smp, num_points, iele_gauss,         &
!     &          isurf_gauss, imark)
!      subroutine s_interpolate_imark_surf8(np_smp, numnod, numele, ie, &
!     &          imark_org, istack_smp, num_points, iele_gauss,         &
!     &          isurf_gauss, imark)
!      subroutine s_interpolate_imark_surf9(np_smp, numnod, numele, ie, &
!     &          imark_org, istack_smp, num_points, iele_gauss,         &
!     &          isurf_gauss, imark)
!
      module interpolate_imark_surf
!
      use m_precision
      use m_constants
!
      use m_geometry_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_imark_surf4(np_smp, numnod, numele, ie,  &
     &          imark_org, istack_smp, num_points, iele_gauss,          &
     &          isurf_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,8)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: isurf_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, isf, k1, ig
!
!
!$omp parallel do private(ist,ied,ig,iele,isf,k1,i1)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
!
          iele = iele_gauss(ig)
          isf =  mod(isurf_gauss(ig),icent)
!
          k1 = node_on_sf_4(1,isf)
          i1 = ie(iele,k1)
!
          imark(ig)   = imark_org(i1 )
!
       end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_imark_surf4
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_imark_surf8(np_smp, numnod, numele, ie,  &
     &          imark_org, istack_smp, num_points, iele_gauss,          &
     &          isurf_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,20)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: isurf_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1
      integer (kind = kint) :: isf, k1, ig
!
!
!$omp parallel do private(ist,ied,ig,iele,isf, k1, i1)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
!
          iele = iele_gauss(ig)
          isf =  mod(isurf_gauss(ig),icent)
!
          k1 = node_on_sf_8(1,isf)
          i1 = ie(iele,k1)
!
          imark(ig)   = imark_org(i1 )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_imark_surf8
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_imark_surf9(np_smp, numnod, numele, ie,  &
     &          imark_org, istack_smp, num_points, iele_gauss,          &
     &          isurf_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,27)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: isurf_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i9
      integer (kind = kint) :: isf, k9
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist,ied,ig,iele,isf,k9,i9)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
!
          iele = iele_gauss(ig)
          isf =  mod(isurf_gauss(ig),icent)
!
          k9 = node_on_sf_9(9,isf)
          i9 = ie(iele,k9)
!
          imark(ig) = imark_org(i9 )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_imark_surf9
!
! ----------------------------------------------------------------------
!
      end module interpolate_imark_surf
