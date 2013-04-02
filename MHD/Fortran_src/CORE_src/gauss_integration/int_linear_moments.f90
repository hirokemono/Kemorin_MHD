!int_linear_moments.f90
!
!      module int_linear_moments
!
!      Written by H. Matsui on Aug., 2006
!
!      subroutine int_linear_moment_infty(n_level, f_mom, f_width)
!      subroutine int_linear_moment_w_range(n_level, f_mom, f_width,    &
!     &          zst, zed)
!
!        n_level: upper limit of order of moments
!        f_mom: moments of filter
!        f_width:  filter width
!        zst, zed:  start end end position for integration
!
      module int_linear_moments
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine int_linear_moment_infty(n_level, f_mom, f_width)
!
      integer(kind = kint), intent(in) :: n_level
      real(kind = kreal), intent(in) :: f_width
      real(kind = kreal), intent(inout) :: f_mom(0:n_level)
!
      integer(kind = kint) :: i
      real(kind = kreal), parameter :: zero = 0.0d0, one = 1.0d0
      real (kind = kreal), parameter :: two = 2.0d0
!
!
      do i = 0, n_level
        f_mom(i) = ( ((two*f_width)**(i+1) - (-two*f_width)**(i+1) )    &
     &              / dble(i+1)                                         &
     &             - ((two*f_width)**(i+2) + (-two*f_width)**(i+2) )    &
     &              / (dble(i+2)*two*f_width) )                         &
     &            /(two*f_width)
      end do
!
      end subroutine int_linear_moment_infty
!
! -----------------------------------------------------------------------
!
      subroutine int_linear_moment_w_range(n_level, f_mom, f_width,     &
     &          zst, zed)
!
      integer(kind = kint), intent(in) :: n_level
      real(kind = kreal), intent(in) :: f_width
      real(kind = kreal), intent(in) :: zst, zed
!
      real(kind = kreal), intent(inout) :: f_mom(0:n_level)
!
      integer(kind = kint) :: i
      real(kind = kreal), parameter :: zero = 0.0d0, one = 1.0d0
      real(kind = kreal), parameter :: two = 2.0d0
!
!
      if (zed .lt. zero) then
!
        do i = 0, n_level
          f_mom(i)                                                      &
     &       = ( (zed**(i+1) - zst**(i+1)) / dble(i+1)                  &
     &         + (zed**(i+2) - zst**(i+2)) / (dble(i+2)*two*f_width) )  &
     &        / (two*f_width)
        end do
!
      else if (zst .gt. zero) then
!
        do i = 0, n_level
          f_mom(i)                                                      &
     &       = ( (zed**(i+1) - zst**(i+1)) / dble(i+1)                  &
     &         - (zed**(i+2) - zst**(i+2)) / (dble(i+2)*two*f_width) )  &
     &        / (two*f_width)
        end do
!
      else
!
        do i = 0, n_level
          f_mom(i)                                                      &
     &       = ( (zed**(i+1) - zst**(i+1)) / dble(i+1)                  &
     &         - (zed**(i+2) + zst**(i+2)) / (dble(i+2)*two*f_width) )  &
     &        / (two*f_width)
!
        end do
      end if
!
      end subroutine int_linear_moment_w_range
!
! -----------------------------------------------------------------------
!
      end module int_linear_moments
