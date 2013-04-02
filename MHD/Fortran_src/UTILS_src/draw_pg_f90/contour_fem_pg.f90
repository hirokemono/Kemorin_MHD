!
!      module contour_fem_pg
!
!*************************************************
!*
!*  subroutine for draw vector map
!*
!*************************************************
!      subroutine draw_vc (nnod_pg, inod_st, inod_ed, inod_int, xg,     &
!     &          vect_pg, norm_size, fv)
!*
!      subroutine fill_tri_pg(idisp_mode, icolor_mode, num_color_pg,    &
!     &          nnod_pg, nele_pg, xg, ie, scalar, xmax, xmin)
!      subroutine drawline_pg(idisp_mode, icolor_mode, num_color_pg,    &
!     &          nnod_pg, nele_pg, xg, ie, scalar, ncl, xc, xmax, xmin)
!
!      subroutine cal_drawcont_minmax(nnod_pg, cont_pg, xmin, xmax)
!      subroutine cal_drawvec_maxlength(nnod_pg, vect_pg, maxlen)
!
      module contour_fem_pg
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
      subroutine draw_vc (nnod_pg, inod_st, inod_ed, inod_int, xg,      &
     &          vect_pg, norm_size, fv)
!
      integer(kind = kint), intent(in) :: nnod_pg
      integer(kind = kint), intent(in) :: inod_st, inod_ed, inod_int
      real(kind = kreal), intent(in) :: norm_size
      real(kind = kreal), intent(in) :: xg(3,nnod_pg)
      real(kind = kreal), intent(in) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(in) :: fv(2,2)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: norm_vect
      real :: r_fvc(2), vx_pg(2)
      character(len=16) :: cxnorm
!
!
      norm_vect = sqrt(fv(1,2)**2 + fv(2,2)**2)
!
      call pgbbuf
      call pgsls(ione)
      call pgslw(ione)
      call pgsch(0.1)
      call pgsci(itwo)
!*
!*  --------  draw arrow of reference -------------
!*
      r_fvc(1:2) = real( fv(1:2,1) )
      vx_pg(1:2) = real( fv(1:2,1) + fv(1:2,2))
      call pgarro(r_fvc(1), r_fvc(2), vx_pg(1), vx_pg(2) )
!*
!*  --------  draw arrow data -------------
!*
      do inod = inod_st ,inod_ed ,inod_int
        r_fvc(1:2) = real(  xg(1:2,inod) )
        vx_pg(1:2) = real( xg(1:2,inod) + vect_pg(1:2,inod)             &
     &                             * norm_vect / norm_size )
        call pgarro(r_fvc(1), r_fvc(2), vx_pg(1), vx_pg(2))
      end do
!*
      call pgslw(ione)
      call pgsci(ione)
      call pgsch(1.0)
!
      vx_pg(1) = real( fv(1,1) + half*fv(1,2) )
      vx_pg(2) = real( fv(2,1) )
      write(cxnorm,'(1pe10.2)') norm_size
      call pgtext( vx_pg(1), vx_pg(2), cxnorm)
!
      call pgebuf
!*
      end subroutine draw_vc
!
! ----------------------------------------------------------------------
!
      subroutine fill_tri_pg(idisp_mode, icolor_mode, num_color_pg,     &
     &          nnod_pg, nele_pg, xg, ie, scalar, xmax, xmin)
!
      use rbcolor_pg
!
      integer(kind = kint), intent(in) :: idisp_mode, icolor_mode
      integer(kind = kint), intent(in) :: num_color_pg
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie(3,nele_pg)
      real(kind = kreal), intent(in) :: xg(3,nnod_pg)
      real(kind = kreal), intent(in) :: scalar(nnod_pg)
      real(kind = kreal), intent(in) :: xmax, xmin
!
      integer(kind = kint) :: inod, iele, k1
      real(kind = kreal) :: s_patch
      real :: x_tri(3), y_tri(3)
!
!
      if ( idisp_mode .ge. itwo ) then
        call pgbbuf
        call pgsfs(ione)
!
        do iele = 1, nele_pg
          s_patch = zero
!
          do k1 = 1, 3
            inod = ie(k1,iele)
            x_tri(k1) = real( xg(1,inod) )
            y_tri(k1) = real( xg(2,inod) )
            s_patch = s_patch + scalar(inod)
          end do
          s_patch = s_patch / three
!
          if (icolor_mode .eq. ione) then
            call set_pg_color_index(num_color_pg, xmax, xmin, s_patch)
          else if (icolor_mode .eq. izero) then
            if (s_patch .lt. zero) then
              call pgsci(ithree)
            else
              call pgsci(ifour)
            end if
          end if
!
          call pgpoly(3, x_tri, y_tri)
        end do
!
        call pgebuf
      end if
!
      end subroutine fill_tri_pg
!
! ----------------------------------------------------------------------
!
      subroutine drawline_pg(idisp_mode, icolor_mode, num_color_pg,     &
     &          nnod_pg, nele_pg, xg, ie, scalar, ncl, xc, xmax, xmin)
!
      use drawline_rainbow_pg
      use drawline_zero_pg
!
      integer(kind = kint), intent(in) :: idisp_mode, icolor_mode
      integer(kind = kint), intent(in) :: num_color_pg
      integer(kind = kint), intent(in) ::ncl
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie(3,nele_pg)
      real(kind = kreal), intent(in) :: xg(3,nnod_pg)
      real(kind = kreal), intent(in) :: scalar(nnod_pg)
      real(kind = kreal), intent(in) :: xc(ncl)
      real(kind = kreal), intent(in) :: xmax, xmin
!
      integer(kind = kint) :: iflag_cline
      integer(kind = kint), parameter :: ifift = 15
!
!
!
      call pgbbuf
      if ( mod(idisp_mode,itwo) .eq. ione ) then
!
        call pgsci(ifift)
!
        if ( idisp_mode.eq.ione .and. icolor_mode.eq.ione) then
          iflag_cline = ione
        else if ( idisp_mode.eq.ione .and. icolor_mode.eq.izero) then
          iflag_cline = itwo
        else if ( idisp_mode.eq.three ) then
          iflag_cline = three
        end if
!
        call drawline_rb1(num_color_pg, iflag_cline, nnod_pg, nele_pg,  &
     &       xg, ie, scalar, ncl, xc, xmin, xmax)
!
      end if
!
      call drawline_zero(nnod_pg, nele_pg, xg, ie, scalar)
      call pgebuf
!
      end subroutine drawline_pg
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_drawcont_minmax(nnod_pg, cont_pg, xmin, xmax)
!
      integer(kind = kint), intent(in) :: nnod_pg
      real(kind = kreal), intent(in) :: cont_pg(nnod_pg)
      real(kind = kreal), intent(inout) :: xmin, xmax
!
      integer(kind = kint) :: inod
!
!
      xmax = cont_pg(1)
      xmin = cont_pg(1)
      do inod = 2, nnod_pg
        xmax = max( xmax,cont_pg(inod) )
        xmin = min( xmin,cont_pg(inod) )
      end do
!
      end subroutine cal_drawcont_minmax
!
! -----------------------------------------------------------------------
!
      subroutine cal_drawvec_maxlength(nnod_pg, vect_pg, maxlen)
!
      integer(kind = kint), intent(in) :: nnod_pg
      real(kind = kreal), intent(in) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: maxlen
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: xlen
!
!
      maxlen = 0.0
      do inod = 1, nnod_pg
        xlen = sqrt( vect_pg(1,inod)*vect_pg(1,inod)                    &
     &               + vect_pg(2,inod)*vect_pg(2,inod) )
        maxlen = max(maxlen,xlen)
      end do
!
      end subroutine cal_drawvec_maxlength
!
! -----------------------------------------------------------------------
!
      end module contour_fem_pg
