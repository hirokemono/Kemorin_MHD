!drawline_zero_pg.f90
!     module drawline_zero_pg
!
      module drawline_zero_pg
!
      use m_precision
      use m_constants
!
      implicit none
!
!      subroutine drawline_zero(nnod, nele, xg, ie, scalar)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine drawline_zero(nnod, nele, xg, ie, scalar)
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: ie(3,nele)
      real(kind = kreal), intent(in) :: xg(3,nnod)
      real(kind = kreal), intent(in) :: scalar(nnod)
!
      real(kind = kreal) ::rsig1, rsig2
      real xline(2), yline(2)
      integer(kind = kint) :: inod1, inod2, inod3, iflag, kline
      integer(kind = kint) :: iele, k1, k2
!
!
      call pgsci(1)
      call pgslw(2)
      call pgsfs(1)
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
        if (     scalar(inod1) .eq. scalar(inod2)                       &
     &     .and. scalar(inod1) .eq. scalar(inod3) ) then
          iflag = 0
        else
!
          do k1 = 1, 3
            k2 = mod(k1,ithree) + 1
            inod1 = ie(k1,iele)
            inod2 = ie(k2,iele)
!
            rsig1 =      scalar(inod1)*scalar(inod2)
            rsig2 = abs( scalar(inod2)-scalar(inod1) )
!
            if ( rsig2 .eq. 0.0 .and. rsig1 .eq. 0.0) then
              iflag = 1
              xline(1) = real(xg(1,inod1))
              xline(2) = real(xg(1,inod2))
              yline(1) = real(xg(2,inod1))
              yline(2) = real(xg(2,inod2))
!
            else if ( rsig1 .le. 0.0 .and. kline .lt. 2) then
              iflag = 1
              kline = kline + 1
              xline(kline) = real( ( xg(1,inod1)*abs(scalar(inod2))     &
     &                             + xg(1,inod2)*abs(scalar(inod1)) )   &
     &                      / rsig2 )
              yline(kline) = real( ( xg(2,inod1)*abs(scalar(inod2))     &
     &                             + xg(2,inod2)*abs(scalar(inod1)) )   &
     &                      / rsig2 )
            end if
          end do
!
        end if
!
        if (iflag .eq. 1) then
          call pgmove(xline(1),yline(1))
          call pgdraw(xline(2),yline(2))
        end if
!
      end do
!
      call pgslw(1)
!
      end subroutine drawline_zero
!
! ----------------------------------------------------------------------
!
      end module drawline_zero_pg
