!>@file   int_gaussian_moments.f90
!!        module int_gaussian_moments
!!
!! @author H. Matsui
!! @date   Programmed in 2006
!!
!
!> @brief Evaluate moments of Gaussian filter
!!
!!@verbatim
!!      subroutine int_gaussian_moment_infty(n_level, f_mom, f_width)
!!      subroutine int_gaussian_moment_w_range(n_level, f_mom, f_width, &
!!     &          zst, zed)
!!@endverbatim
!!
!
!!@n @param        n_level upper limit of order of moments
!!@n @param        f_mom moments of filter
!!@n @param        f_width  filter width
!!@n @param        zst, zed  start end end position for integration
!
      module int_gaussian_moments
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
! -----------------------------------------------------------------------
!
      subroutine int_gaussian_moment_infty(n_level, f_mom, f_width)
!
      integer(kind = kint), intent(in) :: n_level
      real(kind = kreal), intent(in) :: f_width
      real(kind = kreal), intent(inout) :: f_mom(0:n_level)
!
      integer(kind = kint) :: i
!
!
      f_mom(0) = one
      if (n_level.eq.0) return
      f_mom(1) = zero
      if (n_level.eq.1) return
!
      do i = 2, n_level
        f_mom(i) = (f_width**2/three) * dble(i-1)*f_mom(i-2)
      end do
!
      end subroutine int_gaussian_moment_infty
!
! -----------------------------------------------------------------------
!
      subroutine int_gaussian_moment_w_range(n_level, f_mom, f_width,   &
     &          zst, zed)
!
      use m_constants
      use m_gauss_points
      use m_gauss_integration
      use set_filter_moments
!
      integer(kind = kint), intent(in) :: n_level
      real(kind = kreal), intent(in) :: f_width
      real(kind = kreal), intent(in) :: zst, zed
!
      real(kind = kreal), intent(inout) :: f_mom(0:n_level)
!
      real(kind = kreal) :: filter(0:n_level,2), z(2)
!
      integer(kind= kint) :: kf, num_gauss
!
!
      z(1) = zst
      z(2) = zed
      call filter_moment_gaussian(n_level, itwo, f_width,               &
     &    filter(0,1), z)
!
!
      num_gauss = 200
      num_inte = 2
      call allocate_gauss_points(num_gauss)
      call allocate_work_4_integration
!
      call construct_gauss_coefs
!
      call set_points_4_integration(zst, zed)
      call filter_moment_gaussian(ione, n_point, f_width,               &
     &       f_point, x_point)
      call gaussian_integration( f_mom(0) )
!
      do kf = 2, n_level
        f_mom(kf) = (f_width**2/three) * ( dble(kf-1) * f_mom(kf-2)     &
     &             - filter(kf-1,2) + filter(kf-1,1) )
      end do
!
      call deallocate_work_4_integration
      call deallocate_gauss_points
!
      end subroutine int_gaussian_moment_w_range
!
! -----------------------------------------------------------------------
!
      end module int_gaussian_moments
