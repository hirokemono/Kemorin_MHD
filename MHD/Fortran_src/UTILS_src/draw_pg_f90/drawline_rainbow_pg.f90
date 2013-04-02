!drawline_rainbow_pg.f90
!     module drawline_rainbow_pg
!
      module drawline_rainbow_pg
!
      use m_precision
!
      use m_constants
!
      implicit none
!
!      subroutine drawline_rb1(num_color_pg, icolor_mode, nnod, nele,   &
!     &           xg, ie, scalar, nline, xc, xmin, xmax)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine drawline_rb1(num_color_pg, icolor_mode, nnod, nele,    &
     &           xg, ie, scalar, nline, xc, xmin, xmax)
!
      use rbcolor_pg
!
      integer(kind = kint), intent(in) :: num_color_pg, icolor_mode
      integer(kind = kint), intent(in) :: nnod, nele, nline
      integer(kind = kint), intent(in) :: ie(3,nele)
      real(kind = kreal), intent(in) :: xg(3,nnod)
      real(kind = kreal), intent(in) :: scalar(nnod)
      real(kind = kreal), intent(in) :: xc(nline)
      real(kind = kreal), intent(in) :: xmax, xmin
!
      real xline(2), yline(2)
      real(kind = kreal) :: rsig1, rsig2
      integer(kind = kint) :: inod1, inod2, inod3, iflag, kline
      integer(kind = kint) :: ic, iele, k1, k2
!
!
      call pgsfs(1)
!
      if (icolor_mode .ne. ione) call pgsci(1)
!
      do ic = 1, nline
!
!    set line color or style
!
        if (icolor_mode .eq. ione) then
          call set_pg_color_index(num_color_pg, xmax, xmin, xc(ic))
        end if
!
          if (xc(ic) .lt. 0.0 ) then
            call pgsls(2)
          else
            call pgsls(1)
          end if
!
!
!    find lines
!
        do iele = 1, nele
!
          iflag = 0
          kline = 0
!
          inod1 = ie(1,iele)
          inod2 = ie(2,iele)
          inod3 = ie(3,iele)
!
          if (   scalar(inod1) .eq. scalar(inod2)                       &
     &     .and. scalar(inod1) .eq. scalar(inod3) ) then
            iflag = 0
          else
!
            do k1 = 1, 3
              k2 = mod(k1,3) + 1
              inod1 = ie(k1,iele)
              inod2 = ie(k2,iele)
!
              rsig1 = (scalar(inod1)-xc(ic))*(scalar(inod2)-xc(ic))
              rsig2 = abs( scalar(inod2)-scalar(inod1) )
!
              if ( rsig2 .eq. 0.0 .and. rsig1 .eq. 0.0) then
                iflag = 1
                xline(1) = real( xg(1,inod1) )
                xline(2) = real( xg(1,inod2) )
                yline(1) = real( xg(2,inod1) )
                yline(2) = real( xg(2,inod2) )
!
              else if ( rsig1 .le. 0.0 .and. kline .lt. 2) then
                iflag = 1
                kline = kline + 1
                xline(kline)                                            &
     &               = real ( ( xg(1,inod1)*abs(scalar(inod2)-xc(ic))   &
     &                        + xg(1,inod2)*abs(scalar(inod1)-xc(ic)) ) &
     &                      / abs(scalar(inod2)-scalar(inod1)) )
                yline(kline)                                            &
     &               = real ( ( xg(2,inod1)*abs(scalar(inod2)-xc(ic))   &
     &                        + xg(2,inod2)*abs(scalar(inod1)-xc(ic)) ) &
     &                        / abs(scalar(inod2)-scalar(inod1)) )
              end if
            end do
!
          end if
!
          if (iflag .eq. 1) then
!            call pgqci(icolor)
!            write(*,*) 'current culor ID', icolor
!            call pgqcr(icolor,cr,cg,cb)
!            write(*,*) 'line color', cr,cg,cb
            call pgmove(xline(1),yline(1))
            call pgdraw(xline(2),yline(2))
          end if
!
        end do
      end do
!
      end subroutine drawline_rb1
!
! ----------------------------------------------------------------------
!
      end module drawline_rainbow_pg
