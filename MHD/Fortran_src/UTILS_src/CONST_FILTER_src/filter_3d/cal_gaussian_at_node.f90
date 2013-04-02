!
!      module cal_gaussian_at_node
!
      module cal_gaussian_at_node
!
!      Written by H. Matsui on Nov., 2006
!
      use m_precision
!
      implicit none
!
!      subroutine s_cal_gaussian_at_node(delta, xx, yy, zz, x0, y0, z0, &
!     &          dx2_nod, dy2_nod, dz2_nod, g)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_gaussian_at_node(delta, xx, yy, zz, x0, y0, z0,  &
     &          dx2_nod, dy2_nod, dz2_nod, g)
!
      real(kind = kreal), intent(in) :: delta
      real(kind = kreal), intent(in) :: xx, yy, zz, x0, y0, z0
      real(kind = kreal), intent(in) :: dx2_nod, dy2_nod, dz2_nod
      real(kind = kreal), intent(inout) :: g
      real(kind = kreal) :: pi, power
!
      real(kind = kreal), parameter :: one = 1.0d0,   four = 4.0d0
      real(kind = kreal), parameter :: eight = 8.0d0, six = 6.0d0
!
      pi = four * atan(one)
!
      power = - ( (xx-x0)**2 / (four*delta**2 * dx2_nod) )              &
              - ( (yy-y0)**2 / (four*delta**2 * dy2_nod) )              &
              - ( (zz-z0)**2 / (four*delta**2 * dz2_nod) )
!
      g = sqrt( six / pi ) * ( six / pi ) * exp ( power )               &
     &   / ( eight*delta*delta*delta*dx2_nod*dy2_nod*dz2_nod )
!
      end subroutine s_cal_gaussian_at_node
!
! -----------------------------------------------------------------------
!
      end module cal_gaussian_at_node
