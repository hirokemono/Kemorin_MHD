!
      program reverse_aitoff_test
!
      use m_precision
      use m_constants
      use convert_real_rgb_2_bite
      use calypso_png_file_IO
      use aitoff
!
      implicit none
!
      character(len = kchara), parameter :: img_head = 'test_map'
      integer(kind = kint), parameter :: npix_x = 800
      integer(kind = kint), parameter :: npix_y = 600
      integer(kind = kint), parameter :: npix = npix_x*npix_y
      real(kind = kreal), parameter :: xmax =  3.2
      real(kind = kreal), parameter :: ymax =  2.4
      real(kind = kreal), parameter :: xmin = -xmax
      real(kind = kreal), parameter :: ymin = -ymax
!
      real(kind = kreal), allocatable :: rgba(:,:)
      character(len = 1), allocatable :: cimage(:,:)
!
      integer(kind= kint) :: i_pix, j_pix, inod
      real(kind = kreal) :: x_pix, y_pix, pi
      real(kind = kreal) :: rev_colat, rev_long
      real(kind = kreal) :: xyz(3)
!
      pi = four*atan(one)
!
      allocate(rgba(4,npix))
      allocate(cimage(3,npix))
!
      rgba(1:4,1:npix) = 0.0d0
      call cvt_double_rgba_to_char_rgb(npix, rgba, cimage)
!
      do j_pix = 1, npix_y
        y_pix = ymin + (ymax - ymin) * dble(j_pix-1) / dble(npix_y)
        do i_pix = 1, npix_x
          inod = i_pix + (j_pix-1) * npix_x
          x_pix = xmin + (xmax - xmin) * dble(i_pix-1) / dble(npix_x)
          call reverse_aitoff(x_pix, y_pix, rev_colat, rev_long)
          if(rev_colat .lt. 0.0d0) cycle
!
          xyz(1) = half * sin(rev_colat) * cos(rev_long)
          xyz(2) = half * sin(rev_colat) * sin(rev_long)
          xyz(3) = half * cos(rev_colat)
!          rgba(1,inod) = (rev_long + pi) *half / pi
!          rgba(2,inod) =  rev_colat / pi
          rgba(1,inod) = xyz(1) + half
          rgba(2,inod) = xyz(2) + half
          rgba(3,inod) = xyz(3) + half
          rgba(4,inod) = one
        end do
      end do
!
      call cvt_double_rgba_to_char_rgb(npix, rgba, cimage)
      call calypso_write_png                                          &
     &   (img_head, ithree, npix_x, npix_y, cimage(1,1))
      stop
!
      end program reverse_aitoff_test
