!
!      module  draw_pvr_colorbar_nums
!
!      subroutine gen_cbar_label(iscale, color_bar_style,               &
!     &       color_mapping_style, interval_point, interval_mapping_num,&
!     &       num_of_scale, d_minmax, npix_img, isleeve_bar, ntot_pix,  &
!     &       dimage)
!      subroutine gen_zero_label(iscale, color_bar_style,               &
!     &       color_mapping_style, interval_point, interval_mapping_num,&
!     &       d_minmax, npix_img, isleeve_bar, ntot_pix, dimage)
!!      subroutine set_one_label(char1, iscale, ist_px, ist_py,        &
!!     &          npix_img, ntot_pix, dimage)
!
      module  draw_pvr_colorbar_nums
!
      use m_precision
!
      use m_constants
      use set_color_4_pvr
!
      implicit none
!
      integer(kind = kint), parameter, private :: BAR_WIDTH = iten
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gen_cbar_label(iscale, num_of_scale, c_minmax,         &
     &       npix_img, isleeve_bar, ntot_pix, dimage)
!
      integer(kind = kint), intent(in) :: num_of_scale
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: iscale, isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: value
      real(kind = kreal) :: rhgt
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: ist, jst, ied, jed
      integer(kind = kint) :: start_px(2)
      character(len=9) :: numeric
!
      ist = npix_img(1) - isleeve_bar
      jst = (npix_img(2) - itwo*iten) / iten + iten
      jed = (npix_img(2) - itwo*iten) / iten*ifive + jst
      ied = ist + BAR_WIDTH
!
      do k = 1, num_of_scale
        value = (c_minmax(2)-c_minmax(1))                             &
     &           * dble(k-1) / dble(num_of_scale-1) + c_minmax(1)
!
        rhgt = dble(jed-jst) * dble(k-1) / dble(num_of_scale-1)
        start_px(1) = ist + BAR_WIDTH + ithree
        start_px(2) = jst - 12 * iscale / 2                             &
     &                    + int(rhgt, KIND(start_px(1)))
!
        write(numeric,'(1pe9.2)') value
        call  set_numeric_labels(numeric, iscale, start_px,             &
     &        npix_img, isleeve_bar, ntot_pix, dimage)
!
        do i = ist, ied + 4
          j = (start_px(2) * npix_img(1)) + i + 1
          dimage(1:4,j) = one
        end do
      end do
!
      end subroutine gen_cbar_label
!
!  ---------------------------------------------------------------------
!
      subroutine gen_zero_label(iscale, c_minmax, npix_img,             &
     &          isleeve_bar, ntot_pix, dimage)
!
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: iscale, isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: zero_rgb
      real(kind = kreal) :: rhgt
      integer(kind = kint) :: i, k
      integer(kind = kint) :: ist, jst, ied, jed
      integer(kind = kint) :: start_px(2)
      character(len=9) :: numeric
!
      ist = npix_img(1) - isleeve_bar
      jst = (npix_img(2) - itwo*iten) / iten + iten
      jed = (npix_img(2) - itwo*iten) / iten*ifive + jst
      ied = ist + BAR_WIDTH
!
      zero_rgb = (zero - c_minmax(1)) / (c_minmax(2) - c_minmax(1))
!
      rhgt = zero_rgb * dble(jed-jst) + dble(jst)
      start_px(1) = ist + BAR_WIDTH + ithree
      start_px(2) = int(rhgt, KIND(rhgt))
!
      write(numeric,'(1pe9.2)') zero
      call set_numeric_labels(numeric, iscale, start_px,                &
     &         npix_img, isleeve_bar, ntot_pix, dimage)
!
      do i = ist, ied + 4
        k = (start_px(2) * npix_img(1)) + i + 1
        dimage(1:4,k) = one
      end do
!
      end subroutine gen_zero_label
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_numeric_labels(numeric, iscale, start_px,          &
     &          npix_img, isleeve_bar, ntot_pix, dimage)
!
      use pvr_font_texture
!
      character(len=9), intent(in) :: numeric
      integer(kind = kint), intent(in) :: iscale
      integer(kind = kint), intent(in) :: start_px(2)
      integer(kind = kint), intent(in) :: ntot_pix, isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      integer(kind = kint) :: i, j, k, m, ic, jc, ist_px, ist_py
      integer(kind = kint) :: ist, jst, ied, jed
      integer(kind = kint) :: i_font(8,12)
      character(len=1) :: char1
!
      ist = npix_img(1) - isleeve_bar
      jst = (npix_img(2) - itwo*iten) / iten + iten
      jed = (npix_img(2) - itwo*iten) / iten*ifive + jst
      ied = ist + BAR_WIDTH
!
!
      ist_px = start_px(1)
      ist_py = start_px(2)
      do m = 1, 9
        write(char1,'(a1)') numeric(m:m)
        call set_one_label(char1, iscale, ist_px, ist_py,          &
     &      npix_img, ntot_pix, dimage)
        ist_px = ist_px + 8 * iscale
      end do
!
      end subroutine set_numeric_labels
!
!  ---------------------------------------------------------------------
!
      subroutine set_one_label(char1, iscale, ist_px, ist_py,          &
     &          npix_img, ntot_pix, dimage)
!
      use pvr_font_texture
!
      character(len=1), intent(in) :: char1
      integer(kind = kint), intent(in) :: ist_px, ist_py
!
      integer(kind = kint), intent(in) :: iscale
      integer(kind = kint), intent(in) :: ntot_pix
      integer(kind = kint), intent(in) :: npix_img(2)
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      integer(kind = kint) :: i, j, k, ic, jc
      integer(kind = kint) :: i_font(8,12)
!
!
      call gen_font8_12(char1, i_font)
      do i = 1, 8*iscale
        do j = 1, 12*iscale
          k = ( (ist_py+j-1)*npix_img(1)+ist_px + i)
          ic =  (i-1) / iscale + 1
          jc = 12 - (j-1) / iscale
          dimage(1:3,k) = dimage(1:3,k) + i_font(ic,jc)               &
     &                     * (one - two*dimage(1:3,k))
          dimage(4,k) = one
        end do
      end do
!
      end subroutine set_one_label
!
!  ---------------------------------------------------------------------
!
      end module draw_pvr_colorbar_nums
