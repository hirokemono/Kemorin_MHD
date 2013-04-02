!
!      module m_gauss_integration
!
!      Written by H. Matsui on Aug., 2006
!
!
!      subroutine allocate_work_4_integration
!      subroutine deallocate_work_4_integration
!      subroutine set_points_4_integration(xst, xed)
!         set gauss points in integration area xst < x < xed
!      subroutine set_points_4_elevation
!         set gauss points in elevation   pi > theta > 0
!      subroutine gaussian_integration(x)
!         take numerical integration
!                 x: solution
!
      module m_gauss_integration
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: num_inte
!        number of functions to integrate
      real(kind = kreal) :: coef_gauss
!        coefficient due to changing integration area
      real(kind = kreal), dimension(:), allocatable :: x_point
!        gauss points in integration area
      real(kind = kreal), dimension(:,:), allocatable :: f_point
!        function values on integration points
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_work_4_integration
!
      use m_gauss_points
!
!
      allocate( x_point(n_point) )
      allocate( f_point(num_inte,n_point) )
!
      x_point = 0.0d0
      f_point = 0.0d0
!
      end subroutine allocate_work_4_integration
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_work_4_integration
!
      deallocate( x_point )
      deallocate( f_point )
!
      end subroutine deallocate_work_4_integration
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_points_4_integration(xst, xed)
!
      use m_gauss_points
!
!
      real(kind = kreal), intent(in) :: xst, xed
!
      integer (kind = kint) :: i
      real(kind = kreal), parameter :: half = 0.5d0
!
!
      do i = 1, n_point
        x_point(i) = half*( xst + xed + (xed-xst) * w_point(i) )
      end do
      coef_gauss = half *(xed - xst)
!
      end subroutine set_points_4_integration
!
! -----------------------------------------------------------------------
!
      subroutine set_points_4_elevation
!
      use m_gauss_points
!
      integer (kind = kint) :: i
      real(kind = kreal) :: pi
      real(kind = kreal), parameter :: four = 4.0d0, one = 1.0d0
      real(kind = kreal), parameter :: half = 0.5d0
!
!
      pi = four * atan(one)
!
      do i = 1, n_point
        x_point(i) = w_colat(i)
      end do
      coef_gauss = - half * pi
!
      end subroutine set_points_4_elevation
!
! -----------------------------------------------------------------------
!
      subroutine gaussian_integration(x)
!
      use m_gauss_points
!
!
      real(kind = kreal), intent(inout) :: x(num_inte)
!
      integer (kind = kint) :: i, j
!
      x = 0.0d0
!
!$omp parallel do
      do j = 1, num_inte
        do i = 1, n_point
          x(j) = x(j) + w_coefs(i) * f_point(j,i)
        end do
      end do
!$omp end parallel do
!
      do j = 1, num_inte
        x(j) = coef_gauss * x(j)
      end do
!
      end subroutine gaussian_integration
!
! -----------------------------------------------------------------------
!
      end module m_gauss_integration
