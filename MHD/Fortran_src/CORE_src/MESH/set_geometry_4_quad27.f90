!set_geometry_4_quad27.f90
!      module set_geometry_4_quad27
!
!      Written by H. Matsui on May, 2006
!
!      subroutine set_position_on_surf(numnod, numsurf, numele,         &
!     &          ie, ie_surf, xx, nnod_27, xx27)
!
      module set_geometry_4_quad27
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_position_on_surf(numnod, numsurf, numele,          &
     &          xx, x_ele, x_surf, nnod_27, xx27)
!
      integer(kind = kint), intent(in) :: numnod, numsurf, numele
      integer(kind = kint), intent(in) :: nnod_27
      real(kind=kreal), intent(in) :: xx(numnod,3)
      real(kind=kreal), intent(in) :: x_surf(numsurf,3)
      real(kind=kreal), intent(in) :: x_ele(numele,3)
!
      real(kind=kreal), intent(inout) :: xx27(nnod_27,3)
!
      integer(kind = kint) :: inod, iele, isurf
!
!
!$omp parallel do
        do inod = 1, numnod
          xx27(inod,1) = xx(inod,1)
          xx27(inod,2) = xx(inod,2)
          xx27(inod,3) = xx(inod,3)
        end do
!$omp end parallel do
!
!$omp parallel do
      do isurf = 1, numsurf
        inod = numnod + isurf
        xx27(inod,1) = x_surf(isurf,1)
        xx27(inod,2) = x_surf(isurf,2)
        xx27(inod,3) = x_surf(isurf,3)
      end do
!$omp end parallel do
!
!$omp parallel do
      do iele = 1, numele
        inod = numnod + numsurf + iele
        xx27(inod,1) = x_ele(iele,1)
        xx27(inod,2) = x_ele(iele,2)
        xx27(inod,3) = x_ele(iele,3)
      end do
!$omp end parallel do
!
      end subroutine set_position_on_surf
!
!  ---------------------------------------------------------------------
!
      end module set_geometry_4_quad27
