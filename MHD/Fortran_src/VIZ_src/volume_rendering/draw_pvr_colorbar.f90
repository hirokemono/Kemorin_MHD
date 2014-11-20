!
!      module  draw_pvr_colorbar
!
      module  draw_pvr_colorbar
!
      use m_precision
!
      use m_constants
      use m_control_params_4_pvr
!
      implicit none
!
      integer(kind = kint), parameter, private :: BAR_WIDTH = iten
!
      private :: gen_colormark
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_colorbar(i_pvr, num_pixel, rgba_gl)
!
      integer(kind = kint), intent(in) :: i_pvr, num_pixel
      real(kind = kreal), intent(inout)  :: rgba_gl(4,num_pixel)
!
      integer(kind = kint) :: iext_colorbar
!
      if( iflag_pvr_colorbar(i_pvr) .eq. 1) then
        iext_colorbar = 15 + 8 * 9 * iscale_font(i_pvr)
        iext_colorbar = iext_colorbar + ithree                          &
     &                  - mod((iext_colorbar-ione),ifour) 
!        if(cbar_range(2,i_pvr) .le. cbar_range(1,i_pvr)) then
!          cbar_range(1:2,i_pvr) = d_minmax_pvr(1:2,i_pvr)
!        end if
!
        call s_draw_pvr_colorbar(i_pvr, iflag_pvr_colorbar(i_pvr),      &
     &        iflag_pvr_cbar_nums(i_pvr), iflag_pvr_zero_mark(i_pvr),   &
     &        iscale_font(i_pvr), ntick_pvr_colorbar(i_pvr),            &
     &        cbar_range(1,i_pvr), n_pvr_pixel(1,i_pvr),                &
     &        iext_colorbar, num_pixel, rgba_gl)
      end if
!
      end subroutine set_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      subroutine s_draw_pvr_colorbar(i_pvr, color_bar_style,            &
     &         iflag_cbar_numeric, iflag_zero_mark, iscale,             &
     &         num_of_scale, c_minmax, npix_img, isleeve_bar,           &
     &         ntot_pix, dimage)
!
      use draw_pvr_colorbar_nums
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: color_bar_style
      integer(kind = kint), intent(in) :: iflag_cbar_numeric
      integer(kind = kint), intent(in) :: iflag_zero_mark
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      integer(kind = kint), intent(in) :: num_of_scale
      integer(kind = kint), intent(in) :: iscale
!
!
      if (color_bar_style .gt. 0) then
        call gen_colormark(i_pvr, iscale, c_minmax, npix_img,           &
     &        isleeve_bar, ntot_pix, dimage)
!
        if(iflag_cbar_numeric .gt. 0) then
          call gen_cbar_label(iscale, num_of_scale, c_minmax,           &
     &       npix_img, isleeve_bar, ntot_pix, dimage)
!
          if(iflag_zero_mark .gt. 0) then
            call gen_zero_label(iscale, c_minmax, npix_img,             &
     &          isleeve_bar, ntot_pix, dimage)
          end if
        end if
      end if
!
      end  subroutine s_draw_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      subroutine gen_colormark(i_pvr, iscale, c_minmax,                 &
     &          npix_img, isleeve_bar, ntot_pix, dimage)
!
      use set_color_4_pvr
      use set_rgba_4_each_pixel
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: iscale
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: anb_opacity, opa_current
      real(kind = kreal) :: value, color(3)
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: ist_dmap, ist_dopt, num_of_features
      integer(kind = kint) :: ist, jst, ied, jed
!
!
      ist = npix_img(1) - isleeve_bar
      jst = (npix_img(2) - 20) / 10 + 10 - 6*iscale
      jed = (npix_img(2) - 20) / 10*5 + jst
      ied = ist + BAR_WIDTH
!
      do j = jst, jed
        k = j*npix_img(1) + ist
        dimage(1:4,k) = one
        k = j*npix_img(1) + ied + 1
        dimage(1:4,k) = one
      end do
!
      do i = ist-1, ied
        j = jst
        k = jst*npix_img(1) + i + 1
        dimage(1:4,k) = one
        k = jed*npix_img(1) + i + 1
        dimage(1:4,k) = one
      end do
!
      ist_dmap =  istack_pvr_datamap_pnt(i_pvr-1)
      ist_dopt =  istack_opacity_pnt(i_pvr-1)
      num_of_features = num_opacity_pnt(i_pvr)
      anb_opacity = pvr_opacity_param(1,ist_dopt+num_of_features)
      do j = jst, jed
        value = c_minmax(1) + (c_minmax(2)-c_minmax(1))                 &
     &                         * dble(j-jst) / dble(jed-jst)
!
        call compute_opacity(id_pvr_color(3,i_pvr), anb_opacity,        &
     &    num_opacity_pnt(i_pvr), pvr_opacity_param(1,ist_dopt+1),      &
     &    value, opa_current)
        opa_current = opa_current / pvr_max_opacity(i_pvr)
!
      call value_to_rgb(id_pvr_color(2,i_pvr), id_pvr_color(1,i_pvr),   &
     &    num_pvr_datamap_pnt(i_pvr), pvr_datamap_param(1,ist_dmap+1),  &
     &    value, color)
!
        do i = ist, ied-1
          k = j*npix_img(1) + i + 1
          dimage(1:3,k) = color(1:3) * opa_current
          dimage(4,k) = one
        end do
      end do
!
      end subroutine gen_colormark
!
!  ---------------------------------------------------------------------
!
      end module  draw_pvr_colorbar
