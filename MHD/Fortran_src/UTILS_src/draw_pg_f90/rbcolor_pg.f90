!
!      module rbcolor_pg
!
!*************************************************
!*
!*  subroutine for set contor map
!*                      for Rainbow colors
!*
!*************************************************
!
!      subroutine set_pg_color_index(num_color, idx_color,              &
!     &          xmax, xmin, scalar)
!      subroutine set_colormap_pg(icolor_mode, num_color)
!
      module rbcolor_pg
!
      use m_precision
      use m_constants
      use set_rgb_colors
!
      implicit none
!
      private :: color_rb1, color_gray1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!*
      subroutine set_pg_color_index(num_color, xmax, xmin, scalar)
!*
      integer(kind = kint), intent(in) :: num_color
      real(kind = kreal), intent(in) :: xmax, xmin, scalar
!
      integer :: idx_color
      real(kind = kreal) :: scalar_normed
!
!
      call normalize_by_linear(xmin, xmax, scalar, scalar_normed)
!
      idx_color = scalar_normed * (num_color-5) + 4
      if(idx_color .gt. num_color) idx_color = num_color
      if(idx_color .lt. 5) idx_color = 5
!
      call pgsci(idx_color)
!
      end subroutine set_pg_color_index
!
! ----------------------------------------------------------------------
!
      subroutine set_colormap_pg(icolor_mode, num_color)
!
      integer, intent(in) :: icolor_mode, num_color
!
      call pgscr(itwo,   1.0, 0.0, 0.0)
      call pgscr(ithree, 0.5, 0.5, 0.5)
      call pgscr(ifour,  1.0, 1.0, 1.0)
!
      if (icolor_mode .eq. ione) then
        call color_rb1(num_color)
      else if (icolor_mode .eq. izero) then
        call color_gray1(num_color)
      end if
!
      end subroutine set_colormap_pg
!
! ----------------------------------------------------------------------
!*
      subroutine color_rb1(num_color)
!*
      integer, intent(in) :: num_color
!
      integer :: inum, idx_color, nd
      real(kind = kreal) :: scalar_normed
      real(kind = kreal) ::  drgb(3)
      real rgb(3)
!
!
      do inum = 1, num_color-5
        idx_color = inum + 4
        scalar_normed = (dble(inum) - half) / dble(num_color-5)
!
        call color_rainbow(scalar_normed, drgb(1), drgb(2), drgb(3))
!
        rgb(1:3) = drgb(1:3)
        do nd = 1, 3
          if(rgb(nd) .lt.0.0) rgb(nd) = 0.0
          if(rgb(nd) .gt.1.0) rgb(nd) = 1.0
        end do
!
        call pgscr(idx_color, rgb(1), rgb(2), rgb(3))
      end do
!
      end subroutine color_rb1
!
! ----------------------------------------------------------------------
!
      subroutine color_gray1(num_color)
!
      integer, intent(in) :: num_color
!
      integer :: inum, idx_color, nd
      real(kind = kreal) :: scalar_normed
      real(kind = kreal) ::  drgb(3)
      real rgb(3)
!
!
      do inum = 1, num_color-5
        idx_color = inum + 4
        scalar_normed = (dble(inum) - half) / dble(num_color-5)
!
        call color_sym_grayscale(scalar_normed,                         &
     &      drgb(1), drgb(2), drgb(3))
!
        rgb(1:3) = drgb(1:3)
        do nd = 1, 3
          if(rgb(nd) .lt.0.0) rgb(nd) = 0.0
          if(rgb(nd) .gt.1.0) rgb(nd) = 1.0
        end do
!
        call pgscr( idx_color, rgb(1), rgb(2), rgb(3))
      end do
!
      end subroutine color_gray1
!
! ----------------------------------------------------------------------
!
      end module rbcolor_pg
