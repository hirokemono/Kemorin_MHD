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
      private :: color_rb1
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
      idx_color = int(scalar_normed * (num_color-5)) + 4
      if(idx_color .gt. num_color) idx_color = int(num_color)
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
      integer(kind = kint), intent(in) :: icolor_mode, num_color
!
!
      call pgscr(itwo,   1.0, 0.0, 0.0)
      call pgscr(ithree, 0.5, 0.5, 0.5)
      call pgscr(ifour,  1.0, 1.0, 1.0)
!
      call color_rb1(icolor_mode, num_color)
!
      end subroutine set_colormap_pg
!
! ----------------------------------------------------------------------
!*
      subroutine color_rb1(icolor_mode, num_color)
!*
      use set_color_4_pvr
!
      integer(kind = kint), intent(in) :: icolor_mode, num_color
!
      integer(kind = kint) :: inum, nd
      real(kind = kreal) :: scalar_normed
      real(kind = kreal) ::  drgb(3)
      integer :: idx_color
      real rgb(3)
!
!
      do inum = 1, num_color-5
        idx_color = int(inum) + 4
        scalar_normed = (dble(inum) - half) / dble(num_color-5)
!
        call normvalue_to_rgb(icolor_mode, scalar_normed, drgb)
!
        rgb(1:3) = real(drgb(1:3))
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
      end module rbcolor_pg
