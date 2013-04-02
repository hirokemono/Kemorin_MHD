!
!      module draw_mapframe_pg
!
!      subroutine mapframe
!      subroutine vecmap_frame
!
      module draw_mapframe_pg
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!*
      subroutine mapframe
!*
      use map_projection_sph
!
      real :: xf(181), yf(181)
!*
      real(kind = kreal) :: x_grid, y_grid
      real(kind = kreal) :: dy, sin_t, cos_t
      real(kind = kreal) :: dx
      real(kind = kreal) :: pi
!*
      integer(kind = kint) :: ii, jj
!
!*
!*  -----  write meridional reame lines ---------------
!*
      call pgsci(ione)
!
      pi = four*atan(one)
!
      do jj = 1 ,7
        dx = pi * dble(jj-1) / 3.0d0
!*
        do ii = 1, 91
          dy = pi * dble(ii-1) / 90.0d0
          sin_t = sin(dy)
          cos_t = cos(dy)
!*
          call aitoff(sin_t, cos_t, dx, x_grid, y_grid)
          xf(ii) = real( x_grid )
          yf(ii) = real( y_grid )
        end do
!*
        call pgline(91,xf,yf)
      end do
!*
!*  ----------   write equational frame lines -----------
!*
      do jj = 1 ,5
        dy = pi * dble(jj) / 6.0d0
        sin_t = sin(dy)
        cos_t = cos(dy)
!*
        do ii = 1, 181
          dx = pi * dble(ii-1) / 90.0d0
!*
          call aitoff(sin_t, cos_t, dx, x_grid, y_grid)
          xf(ii) = real( x_grid )
          yf(ii) = real( y_grid )
        end do
!*
        call pgline(181,xf,yf)
!*
      end do
!*
      end subroutine mapframe
!
! ----------------------------------------------------------------------
!*
      subroutine vecmap_frame
!*
      real :: xf(2), yf(2)
      integer(kind = kint) :: jj
!*
!*  -----  write meridional reame lines ---------------
!*
      yf(1) = -real(one)
      yf(2) =  real(one)
      do jj = 1 ,7
        xf(1) = real(jj-4) / 1.5e0
        xf(2) = real(jj-4) / 1.5e0
       call pgline(itwo,xf,yf)
      end do
!*
!*  ----------   write equational frame lines -----------
!*
      xf(1) = - real(two)
      xf(2) =   real(two)
      do jj = 1 ,7
        yf(1) = dble(jj-4) / 3.0d0
        yf(2) = dble(jj-4) / 3.0d0
        call pgline(itwo,xf,yf)
      end do
!*
      end subroutine vecmap_frame
!
! ----------------------------------------------------------------------
!
      end module draw_mapframe_pg
