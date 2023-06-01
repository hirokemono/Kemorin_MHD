
!      module map_contour_fem_pg
!
!*************************************************
!*
!*  subroutine for draw vector map
!*
!*************************************************
!      subroutine set_map_vector(nnod_pg, ncomp_pg, ifld_pg, vector,    &
!     &          vect_pg, vert_pg)
!
!      subroutine draw_vc_map(nnod_pg, inod_st, inod_ed, inod_int,      &
!     &          xx_psf, vect_pg, norm_size)
!*
!      subroutine fill_tri_map(idisp_mode, icolor_mode, num_color_pg,   &
!     &          nnod_pg, nele_pg, xx_psf, ie_psf, scalar_psf,          &
!     &          xmax, xmin)
!      subroutine drawline_map_fem(idisp_mode, icolor_mode,             &
!     &          num_color_pg, nnod_pg, nele_pg, xx_psf, ie_psf,        &
!     &          scalar_psf, ncl, xc, xmax, xmin)
!
      module map_contour_fem_pg
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
      subroutine set_map_vector(nnod_pg, ncomp_pg, ifld_pg, vector,     &
     &          vect_pg, vert_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg
      integer(kind = kint), intent(in) :: ncomp_pg, ifld_pg
      real(kind = kreal), intent(in) :: vector(nnod_pg,ncomp_pg)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
!
      vect_pg(1,1:nnod_pg) =  vector(1:nnod_pg,ifld_pg+2)
      vect_pg(2,1:nnod_pg) = -vector(1:nnod_pg,ifld_pg+1)
      vert_pg(1:nnod_pg) =    vector(1:nnod_pg,ifld_pg  )
!
      end subroutine set_map_vector
!
! ----------------------------------------------------------------------
!
      subroutine draw_vc_map(nnod_pg, inod_st, inod_ed, inod_int,       &
     &          xx_psf, vect_pg, norm_size)
!
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: nnod_pg
      integer(kind = kint), intent(in) :: inod_st, inod_ed, inod_int
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: norm_size
      real(kind = kreal), intent(in) :: vect_pg(2,nnod_pg)
!
      real(kind = kreal) :: pi, xx(3), rtp_psf(3), s_psf(1)
      real(kind = kreal) :: ar_psf(1), as_psf(1)
      integer(kind = kint) :: inod
      real :: rfv(2,2)
      real :: vx_pg(2), xx_pg(2), norm_vect
      character(len=16) :: cxnorm
!
      pi = four*atan(one)
      rfv(1,1) =  2.03
      rfv(2,1) = -1.0
      rfv(1,2) =  0.3 / 2.0
      rfv(2,2) =  0.4 / 2.0
!
!
      norm_vect = real(sqrt( dble(rfv(1,2))**2 + dble(rfv(2,2))**2 ))
!
      call pgbbuf
      call pgsls(ione)
      call pgsci(itwo)
      call pgslw(ione)
      call pgsch(0.3)
      call pgsah(1, 30.0, 0.2)
!*
!*  --------  draw arrow of reference -------------
!*
      vx_pg(1:2) = rfv(1:2,1) + rfv(1:2,2)
      call pgarro(rfv(1,1), rfv(2,1), vx_pg(1), vx_pg(2) )
!*
!*  --------  draw arrow data -------------
!*
      do inod = inod_st ,inod_ed ,inod_int
        xx(1:3) = xx_psf(inod,1:3)
        call position_2_sph(ione, xx(1), rtp_psf(1), rtp_psf(2),        &
     &      rtp_psf(3), ar_psf(1), s_psf(1), as_psf(1) )
!
        xx_pg(1) =  real(two * (rtp_psf(3) / pi - one))
        xx_pg(2) =  real(two * (half - rtp_psf(2) / pi))
        vx_pg(1:2) = xx_pg(1:2) + real( vect_pg(1:2,inod)               &
     &                           * norm_vect / norm_size )
!
        call pgarro( xx_pg(1), xx_pg(2), vx_pg(1), vx_pg(2) )
      end do
!*
      call pgslw(ione)
      call pgsci(ione)
      call pgsch(1.0)
!
      vx_pg(1) = rfv(1,1)
      vx_pg(2) = rfv(2,1)
      write(cxnorm,'(1pe10.2)') norm_size
      call pgtext( vx_pg(1), vx_pg(2), cxnorm)
!
      call pgebuf
!*
      end subroutine draw_vc_map
!
! ----------------------------------------------------------------------
!
      subroutine fill_tri_map(idisp_mode, icolor_mode, num_color_pg,    &
     &          nnod_pg, nele_pg, xx_psf, ie_psf, scalar_psf,           &
     &          xmax, xmin, map_e)
!
      use rbcolor_pg
      use t_map_patch_from_1patch
      use map_patch_from_1patch
!
      integer(kind = kint), intent(in) :: idisp_mode, icolor_mode
      integer(kind = kint), intent(in) :: num_color_pg
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie_psf(nele_pg,3)
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: scalar_psf(nnod_pg)
      real(kind = kreal), intent(in) :: xmax, xmin
!
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, k1, i
      real(kind = kreal) :: s_patch
!
      real :: x_tri(3), y_tri(3)
!
!
      if ( idisp_mode .ge. itwo ) then
        call pgbbuf
        call pgsfs(ione)
!
        do iele = 1, nele_pg
!
          call s_set_map_patch_from_1patch(iele, nnod_pg, nele_pg,      &
     &        xx_psf, ie_psf, ione, scalar_psf(1), map_e%n_map_patch,   &
     &        map_e%x_map_patch, map_e%d_map_patch)
          call set_sph_position_4_map_patch(map_e%n_map_patch,          &
     &        map_e%x_map_patch, map_e%rtp_map_patch)
          call patch_to_aitoff(map_e%n_map_patch, map_e%rtp_map_patch,  &
     &                         map_e%xy_map)
!
          s_patch = zero
          do i = 1, map_e%n_map_patch
            do k1 = 1, 3
              x_tri(k1) = real( map_e%xy_map(1,k1,i) )
              y_tri(k1) = real( map_e%xy_map(2,k1,i) )
              s_patch = s_patch + map_e%d_map_patch(k1,1,i)
            end do
            s_patch = s_patch / three
!
            if (icolor_mode .eq. ione) then
              call set_pg_color_index(num_color_pg, xmax, xmin,         &
     &            s_patch)
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
        end do
!
        call pgebuf
      end if
!
      end subroutine fill_tri_map
!
! ----------------------------------------------------------------------
!
      subroutine drawline_map_fem(idisp_mode, icolor_mode,              &
     &          num_color_pg, nnod_pg, nele_pg, xx_psf, ie_psf,         &
     &          scalar_psf, ncl, xc, xmax, xmin, map_e)
!
      use t_map_patch_from_1patch
      use drawline_rainbow_pg
      use drawline_zero_pg
      use map_patch_from_1patch
!
      integer(kind = kint), intent(in) :: idisp_mode, icolor_mode
      integer(kind = kint), intent(in) :: num_color_pg
      integer(kind = kint), intent(in) :: ncl
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie_psf(nele_pg,3)
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: scalar_psf(nnod_pg)
      real(kind = kreal), intent(in) :: xc(ncl)
      real(kind = kreal), intent(in) :: xmax, xmin
!
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint), parameter :: ie(3,1) = reshape(             &
     &                                     (/1, 2, 3/), shape=(/3,1/) )
!
      integer(kind = kint) :: i, k1, iele
      integer(kind = kint) :: iflag_cline
      real(kind = kreal) :: xg(3,3), sc3(3)
!
!
      call pgbbuf
      if ( mod(idisp_mode,itwo) .eq. ione ) then
        if ( idisp_mode.eq.ione .and. icolor_mode.eq.ione) then
          iflag_cline = ione
        else if ( idisp_mode.eq.ione .and. icolor_mode.eq.izero) then
          iflag_cline = itwo
        else if ( idisp_mode .eq. ithree ) then
          iflag_cline = ithree
        end if
!
        do iele = 1, nele_pg
!
          call s_set_map_patch_from_1patch(iele, nnod_pg, nele_pg,      &
     &        xx_psf, ie_psf, ione, scalar_psf(1), map_e%n_map_patch,   &
     &        map_e%x_map_patch, map_e%d_map_patch)
          call set_sph_position_4_map_patch(map_e%n_map_patch,          &
     &        map_e%x_map_patch, map_e%rtp_map_patch)
          call patch_to_aitoff(map_e%n_map_patch, map_e%rtp_map_patch,  &
     &                         map_e%xy_map)
!
          do i = 1, map_e%n_map_patch
            do k1 = 1, 3
              xg(1,k1) = real( map_e%xy_map(1,k1,i) )
              xg(2,k1) = real( map_e%xy_map(2,k1,i) )
              xg(3,k1) = zero
              sc3(k1) =  map_e%d_map_patch(k1,1,i)
            end do
            call drawline_rb1(num_color_pg, iflag_cline, ithree, ione,  &
     &           xg, ie, sc3(1), ncl, xc, xmin, xmax)
            call drawline_zero(ithree, ione, xg, ie, sc3)
          end do
        end do
!
      end if
      call pgebuf
!
      end subroutine drawline_map_fem
!
! ----------------------------------------------------------------------
!
      end module map_contour_fem_pg
