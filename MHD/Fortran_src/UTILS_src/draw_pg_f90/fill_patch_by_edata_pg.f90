!fill_patch_by_edata_pg.f90
!      module fill_patch_by_edata_pg
!
!      subroutine fill_tri_ele_pg(icolor_mode, num_color_pg, nnod_pg,   &
!     &           nele_pg, xg, ie, s_patch, xmax, xmin)
!      subroutine fill_quad_ele_pg(idisp_mode, num_color_pg, nnod_pg,   &
!     &           nele_pg, xg, ie, s_patch, xmax, xmin)
!
      module fill_patch_by_edata_pg
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer, parameter :: ifift = 15
      private :: ifift
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine fill_tri_ele_pg(icolor_mode, num_color_pg, nnod_pg,    &
     &           nele_pg, xg, ie, s_patch, xmax, xmin)
!
      use rbcolor_pg
!
      integer(kind = kint), intent(in) :: icolor_mode, num_color_pg
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie(3,nele_pg)
      real(kind = kreal), intent(in) :: xg(3,nnod_pg)
      real(kind = kreal), intent(in) :: s_patch(nele_pg)
      real(kind = kreal), intent(in) :: xmax, xmin
!
      integer :: inod, iele, i1, i2, i3
      real :: x_tri(3), y_tri(3)
!
!
      call pgbbuf
      call pgsfs(ione)
!
      do iele = 1, nele_pg
!
        if (icolor_mode .eq. ione) then
          call set_pg_color_index(num_color_pg,                         &
     &        xmax, xmin, s_patch(iele))
        else if (icolor_mode .eq. izero) then
          if (s_patch(iele) .lt. zero) then
            call pgsci(ithree)
          else
            call pgsci(ifour)
          end if
        end if
!
        i1 = ie(1,iele)
        i2 = ie(2,iele)
        i3 = ie(3,iele)
        x_tri(1) = real( xg(1,i1) )
        x_tri(2) = real( xg(1,i2) )
        x_tri(3) = real( xg(1,i3) )
        y_tri(1) = real( xg(2,i1) )
        y_tri(2) = real( xg(2,i2) )
        y_tri(3) = real( xg(2,i3) )
        call pgpoly(3, x_tri, y_tri)
      end do
!
      call pgebuf
!
      end subroutine fill_tri_ele_pg
!
! ----------------------------------------------------------------------
!
      subroutine fill_quad_ele_pg(idisp_mode, num_color_pg, nnod_pg,    &
     &           nele_pg, xg, ie, s_patch, xmax, xmin)
!
      use rbcolor_pg
!
      integer(kind = kint), intent(in) :: idisp_mode, num_color_pg
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie(4,nele_pg)
      real(kind = kreal), intent(in) :: xg(3,nnod_pg)
      real(kind = kreal), intent(in) :: s_patch(nele_pg)
      real(kind = kreal), intent(in) :: xmax, xmin
!
      integer :: inod, iele, i1, i2, i3, i4
      real :: x_tri(4), y_tri(4)
!
!
      call pgbbuf
      call pgsfs(ione)
!
      do iele = 1, nele_pg
!
        call set_pg_color_index(num_color_pg, xmax, xmin,               &
     &      s_patch(iele))
!
        if(idisp_mode .eq. ifour) then
          if (s_patch(iele) .lt. zero) then
            call pgshs(45.0, 0.5, 0.0)
            call pgsfs(ifour)
          else
            call pgsfs(ione)
          end if
        end if
!
        i1 = ie(1,iele)
        i2 = ie(2,iele)
        i3 = ie(3,iele)
        i4 = ie(4,iele)
        x_tri(1) = real( xg(1,i1) )
        x_tri(2) = real( xg(1,i2) )
        x_tri(3) = real( xg(1,i3) )
        x_tri(4) = real( xg(1,i4) )
        y_tri(1) = real( xg(2,i1) )
        y_tri(2) = real( xg(2,i2) )
        y_tri(3) = real( xg(2,i3) )
        y_tri(4) = real( xg(2,i4) )
        call pgpoly(4, x_tri, y_tri)
!
      end do
      call pgshs(45.0, 1.0, 0.0)
      call pgsfs(ione)
!
      call pgebuf
!
      end subroutine fill_quad_ele_pg
!
! ----------------------------------------------------------------------
!
      end module fill_patch_by_edata_pg
