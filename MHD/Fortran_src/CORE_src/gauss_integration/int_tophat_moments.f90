!int_tophat_moments.f90
!
!      module int_tophat_moments
!
!      Written by H. Matsui on Aug., 2006
!
!      subroutine int_tophat_moment_infty(n_level, f_mom, f_width)
!      subroutine int_tophat_moment_w_range(n_level, f_mom, f_width,    &
!     &          zst, zed)
!
!        n_level: upper limit of order of moments
!        f_mom: moments of filter
!        f_width:  filter width
!        zst, zed:  start end end position for integration
!
      module int_tophat_moments
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
      subroutine int_tophat_moment_infty(n_level, f_mom, f_width)
!
      integer(kind = kint), intent(in) :: n_level
      real(kind = kreal), intent(in) :: f_width
      real(kind = kreal), intent(inout) :: f_mom(0:n_level)
!
      integer(kind = kint) :: i
      real (kind = kreal), parameter :: two = 2.0d0, four = 4.0d0
!
!
      do i = 0, n_level
        f_mom(i) = ( (two*f_width)**(i+1) - (-two*f_width)**(i+1) )     &
     &            / (four*f_width*(i+1))
      end do
!
      end subroutine int_tophat_moment_infty
!
! -----------------------------------------------------------------------
!
      subroutine int_tophat_moment_w_range(n_level, f_mom, f_width,     &
     &          zst, zed)
!
      integer(kind = kint), intent(in) :: n_level
      real(kind = kreal), intent(in) :: f_width
      real(kind = kreal), intent(in) :: zst, zed
!
      real(kind = kreal), intent(inout) :: f_mom(0:n_level)
!
      integer(kind = kint) :: i
      real (kind = kreal), parameter :: two = 2.0d0, four = 4.0d0
!
!
      do i = 0, n_level
        f_mom(i) = (zed**(i+1) - zst**(i+1))                            &
     &            / (four*f_width*dble(i+1) )
      end do
!
      end subroutine int_tophat_moment_w_range
!
! -----------------------------------------------------------------------
!
      end module int_tophat_moments
