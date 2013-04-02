!
!      module draw_colorbar_pg
!
!      subroutine draw_colorbar(idisp_mode, icolor_mode, num_color_pg,  &
!     &          nline, xmin, xmax, xc, xframe, yframe)
!      subroutine draw_axis_zplane(frame)
!      subroutine draw_axis_xplane(frame)
!      subroutine draw_axis_yplane(frame)
!
      module draw_colorbar_pg
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
!
      subroutine draw_colorbar(idisp_mode, icolor_mode, num_color_pg,   &
     &          nline, xmin, xmax, xc, xframe, yframe)
!
      use rbcolor_pg
!
      integer(kind = kint), intent(in) :: idisp_mode, icolor_mode
      integer(kind = kint), intent(in) :: num_color_pg
      integer(kind = kint), intent(in) :: nline
      real(kind = kreal), intent(in) :: xmin, xmax, xframe, yframe
      real(kind = kreal), intent(in) :: xc(nline)
!
      real(kind = kreal) :: rcolor
      integer(kind = kint) :: ii, ic, ic2
      character(len=16) :: cxmin, cxmax
!
      real :: bx1, bx2, by1, by2
      real :: wbt_x = 1.0e0, wbt_y = -1.0e0
      real, parameter :: wid_barx = 0.08e0, wid_bary = 0.7e0
      integer(kind = kint), parameter :: nbar_level = 64
      character(len=3), parameter :: cxzero = '0.0'
!
!   draw box
!
      wbt_x =  real(xframe)
      wbt_y = -real(yframe)
!
!
      if (idisp_mode .eq. itwo .or. idisp_mode .eq. ithree ) then
!
        bx1 = wbt_x
        bx2 = wbt_x + wid_barx
        by2 = wbt_y
        by1 = wbt_y + wid_bary / real(nbar_level)
!
        call pgsfs(ione)
        do ii = 0, nbar_level
!*
          rcolor = xmin + dble(ii)*(xmax-xmin)/ dble(nbar_level)
          if (icolor_mode .eq. ione) then
            call set_pg_color_index(num_color_pg, xmax, xmin, rcolor)
          else if (icolor_mode .eq. 0) then
            if (rcolor .lt. zero) then
              call pgsci(ithree)
            else
              call pgsci(ifour)
            end if
          end if
!
          call pgbbuf
          call pgrect(bx1,bx2,by1,by2)
          call pgebuf
          by1 = by1 + wid_bary / real(nbar_level)
          by2 = by2 + wid_bary / real(nbar_level)
!*
        end do
      else if (idisp_mode .eq. ifour) then
!
        bx1 = wbt_x
        bx2 = wbt_x + wid_barx
        by2 = wbt_y
        by1 = wbt_y + wid_bary / real(nbar_level)
!
        call pgsfs(ione)
        do ii = 0, nbar_level
!*
          rcolor = xmin + dble(ii)*(xmax-xmin) / dble(nbar_level)
          call set_pg_color_index(num_color_pg, xmax, xmin, rcolor)
!
          if (rcolor .lt. rzero) then
            call pgshs(45.0, 0.5, 0.0)
            call pgsfs(ifour)
          else
            call pgsfs(ione)
          end if
!
          call pgbbuf
          call pgrect(bx1,bx2,by1,by2)
          call pgebuf
          by1 = by1 + wid_bary / real(nbar_level)
          by2 = by2 + wid_bary / real(nbar_level)
!*
        end do
        call pgshs(45.0, 1.0, 0.0)
        call pgsfs(ione)
      else
        bx1 = wbt_x
        bx2 = wbt_x + wid_barx
        by1 = wbt_y + wid_bary
        by2 = wbt_y
        call pgbbuf
        call pgsfs(itwo)
        call pgsci(ione)
        call pgrect(bx1,bx2,by1,by2)
        call pgebuf
!
      end if
!
!   draw color lines
!
      bx1 = wbt_x
      bx2 = wbt_x + wid_barx
      by1 = wbt_y + wid_bary
!*
      if (idisp_mode.eq.ione .or. idisp_mode.eq.ithree) then
        call pgbbuf
!
        do ic = 1, nline
          ic2 = int((ic-1)*14/nline) + itwo
!
          if (idisp_mode .eq. ithree ) then
            call pgsci(ione)
          else
            if(icolor_mode.eq.ione) then
              call set_pg_color_index(num_color_pg, xmax, xmin, xc(ic))
            else
              call pgsci(ione)
            end if
          end if
!
              if (xc(ic) .lt. rzero ) then
                call pgsls(itwo)
              else
                call pgsls(ione)
              end if
!
          by1 = wbt_y + wid_bary * real(xc(ic)-xmin) / real(xmax-xmin)
          call pgmove(bx1,by1)
          call pgdraw(bx2,by1)
!*
        end do
        call pgebuf
      end if
!
!
      call pgbbuf
      call pgsch(rone)
      call pgslw(ione)
      call pgsci(ione)
      call pgsls(ione)
      write(cxmin,'(1pe10.2)') xmin
      write(cxmax,'(1pe10.2)') xmax
      call pgtext( (wbt_x+0.03), (wbt_y-0.08), cxmin)
      call pgtext( (wbt_x+0.03), (wbt_y+0.72), cxmax)
!*
!   draw zero marker
!
      if ( xmin*xmax .le. rzero ) then
        call pgsci(ione)
        call pgslw(itwo)
!
        bx2 = wbt_x + 0.16
        by1 = wbt_y - wid_bary*xmin / (xmax-xmin)
        call pgmove(bx1,by1)
        call pgdraw(bx2,by1)
        call pgtext( (wbt_x+0.16), (by1+0.02), cxzero)
        call pgslw(ione)
      end if
!
      call pgebuf
!
      end subroutine draw_colorbar
!
! ----------------------------------------------------------------------
!
      subroutine draw_axis_zplane(frame)
!
      real(kind = kreal), intent(in) :: frame
      real :: xaxis
!
!
      xaxis = 0.9 * real(frame)
      call pgsch(rone)
      call pgsci(ione)
      call pgarro(rzero, rzero, xaxis, rzero)
      call pgarro(rzero, rzero, rzero, xaxis)
      call pgtext( (xaxis+0.05), -0.1,'\\frx')
      call pgtext( 0.1, (xaxis+0.05), '\\fry')
!
      end subroutine draw_axis_zplane
!
! ----------------------------------------------------------------------
!
      subroutine draw_axis_xplane(frame)
!
      real(kind = kreal), intent(in) :: frame
      real :: xaxis
!
!
      xaxis = 0.9 * real(frame)
      call pgsch(rone)
      call pgsci(ione)
      call pgarro(rzero, rzero, xaxis, rzero)
      call pgarro(rzero, rzero, rzero, xaxis)
      call pgtext( (xaxis+0.05), -0.1,'\\fry')
      call pgtext( 0.1, (xaxis+0.05), '\\frz')
!
      end subroutine draw_axis_xplane
!
! ----------------------------------------------------------------------
!
      subroutine draw_axis_yplane(frame)
!
      real(kind = kreal), intent(in) :: frame
      real :: xaxis
!
!
      xaxis = 0.9 * real(frame)
      call pgsch(rone)
      call pgsci(ione)
      call pgarro(rzero, rzero, xaxis, rzero)
      call pgarro(rzero, rzero, rzero, xaxis)
      call pgtext( (xaxis+0.05), -0.1,'\\frx')
      call pgtext( 0.1, (xaxis+0.05), '\\frz')
!
      end subroutine draw_axis_yplane
!
! ----------------------------------------------------------------------
!
      end module draw_colorbar_pg
