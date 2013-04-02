!set_filter_moments.f90
!      module set_filter_moments
!
!      Written by H. Matsui in 2004
!
!     subroutine filter_moment_gaussian(n_order, n_point, r, g, x)
!        moments for gaussian filter
!
!     subroutine filter_moment_linear(n_order, n_point, r, g, x)
!        moments for linear filter
!
!     subroutine filter_moment_tophat(n_order, n_point, r, g, x)
!        moments for tophat filter
!
!     g: moments at gauss points
!     x: position of gauss points
!     r: filter width
!     n_order: number of order for moments
!     n_points: number of gauss points
!
      module set_filter_moments
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
!
      subroutine filter_moment_gaussian(n_order, n_point, r, g, x)
!
      integer (kind = kint), intent(in) :: n_point, n_order
      real(kind=kreal), intent(in) :: r
      real(kind=kreal), intent(in) :: x(n_point)
!
      real(kind=kreal), intent(inout) :: g(n_order+1, n_point)
!
      integer (kind = kint) :: i, j, k
      real(kind=kreal) :: pi
!
!
      pi = four * atan(one)
!
      do k = 1, n_point
        do i = 1, n_order+1
          g(i,k) = x(k)**(i-ione) * ( sqrt(six/pi) / (two*r) )          &
     &            * exp( -six * x(k)**2 / (four*r**2) )
        end do
      end do
!
      end subroutine filter_moment_gaussian
!
! -----------------------------------------------------------------------!
!
      subroutine filter_moment_linear(n_order, n_point, r, g, x)
!
      integer (kind = kint), intent(in) :: n_point, n_order
      real(kind=kreal), intent(in) :: r
      real(kind=kreal), intent(in) :: x(n_point)
!
      real(kind=kreal), intent(inout) :: g(n_order+1, n_point)
!
      integer (kind = kint) :: i, j, k
!
!
      do k = 1, n_point
        if ( x(k) .le. -two*r ) then
          do i = 1, n_order+1
            g(i,k) = zero
          end do
        else if ( x(k) .lt. zero ) then
          do i = 1, n_order+1
            g(i,k) = x(k)**(i-ione) * ( one + x(k)/(two*r) ) / (two*r)
          end do
        else if ( x(k) .lt. two*r ) then
          do i = 1, n_order+1
            g(i,k) = x(k)**(i-ione) * ( one - x(k)/(two*r) ) / (two*r)
          end do
        else
          do i = 1, n_order+1
            g(i,k) = zero
          end do
        end if
      end do
!
      end subroutine filter_moment_linear
!
! -----------------------------------------------------------------------!
!
      subroutine filter_moment_tophat(n_order, n_point, r, g, x)
!
      integer (kind = kint), intent(in) :: n_point, n_order
      real(kind=kreal), intent(in) :: r
      real(kind=kreal), intent(in) :: x(n_point)
!
      real(kind=kreal), intent(inout) :: g(n_order+1, n_point)
!
      integer (kind = kint) :: i, j, k
!
!
      do k = 1, n_point
        if ( x(k) .le. -two*r ) then
          do i = 1, n_order+1
            g(i,k) = zero
          end do
        else if ( x(k) .lt. two*r ) then
          do i = 1, n_order+1
            g(i,k) = x(k)**(i-ione) / (four*r)
          end do
        else
          do i = 1, n_order+1
            g(i,k) = zero
          end do
        end if
      end do
!
      end subroutine filter_moment_tophat
!
! -----------------------------------------------------------------------!
      end module set_filter_moments
