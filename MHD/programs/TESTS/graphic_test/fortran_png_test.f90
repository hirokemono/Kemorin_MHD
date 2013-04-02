!
      module png_test_from_f
!
!  ---------------------------------------------------------------------
      contains
!  ---------------------------------------------------------------------
!
      subroutine  s_png_test_from_f
!
      use convert_real_rgb_2_bite
!
      integer(kind = kint) :: num_pixel_x, num_pixel_y, num_pixel_xy
      real(kind = kreal), allocatable :: rgba_real_gl(:,:)
      character(len = 1), allocatable :: rgb_chara_gl(:,:)
      character(len = 1), allocatable :: rgba_chara_gl(:,:)
      character(len=1024) ::  fname_img_v
!
!
      num_pixel_x = 240
      num_pixel_y = 120
      num_pixel_xy = num_pixel_x*num_pixel_y
      allocate(rgb_chara_gl(3,num_pixel_xy))
      allocate(rgba_chara_gl(4,num_pixel_xy))
      allocate(rgba_real_gl(4,num_pixel_xy))
!
!
      call set_rgba_image_1(num_pixel_x, num_pixel_y, rgba_real_gl)
!
      call cvt_double_rgba_to_char_rgba(num_pixel_xy, rgba_real_gl,     &
     &    rgba_chara_gl)
      write(fname_img_v, '(a,a1)') 'rgba_test_1', CHAR(0)
      call write_png_rgba_c(fname_img_v, num_pixel_x,                   &
     &    num_pixel_y, rgba_chara_gl(1,1))
!
      call cvt_double_rgba_to_char_rgb(num_pixel_xy, rgba_real_gl,      &
     &    rgb_chara_gl)
      write(fname_img_v, '(a,a1)') 'rgb_test_1', CHAR(0)
      call write_png_rgb_c(fname_img_v, num_pixel_x, num_pixel_y,       &
     &    rgb_chara_gl(1,1))
!
!
      call set_rgba_image_2(num_pixel_x, num_pixel_y, rgba_real_gl)
!
      call cvt_double_rgba_to_char_rgba(num_pixel_xy, rgba_real_gl,     &
     &    rgba_chara_gl)
      write(fname_img_v, '(a,a1)') 'rgba_test_2', CHAR(0)
      call write_png_rgba_c(fname_img_v, num_pixel_x,                   &
     &    num_pixel_y, rgba_chara_gl(1,1))
!
      call cvt_double_rgba_to_char_rgb(num_pixel_xy, rgba_real_gl,      &
     &    rgb_chara_gl)
      write(fname_img_v, '(a,a1)') 'rgb_test_2', CHAR(0)
      call write_png_rgb_c(fname_img_v, num_pixel_x, num_pixel_y,       &
     &    rgb_chara_gl(1,1))
!
      end subroutine s_png_test_from_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_rgba_image_1(num_x, num_y, rgba)
!
      integer, intent(in) :: num_x, num_y
      real(kind = 8), intent(inout) :: rgba(4,num_x*num_y)
      integer :: j1, j2, i, j, k
!
!
      j1 = num_y/3
      j2 = 2*num_y/3
!
      do j = 1, j1
        do i = 1, num_x
          k = i + (j-1)*num_x
          rgba(1,k) = dble(i-1) / dble(num_x)
          rgba(2,k) = 0
          rgba(3,k) = 0
          rgba(4,k) = 1.0
        end do
      end do
!
!
      do j = j1+1, j2
        do i = 1, num_x
          k = i + (j-1)*num_x
          rgba(1,k) = 0
          rgba(2,k) = dble(num_x-i) / dble(num_x)
          rgba(3,k) = 0
          rgba(4,k) = 0.5
        end do
      end do
!
      do j = j2+1, num_y
        do i = 1, num_x
          k = i + (j-1)*num_x
          rgba(1,k) = 0
          rgba(2,k) = 0
          rgba(3,k) = dble(i-1) / dble(num_x)
          rgba(4,k) = dble(num_x-i) / dble(num_x)
        end do
      end do
!
      end subroutine set_rgba_image_1
!
!  ---------------------------------------------------------------------
!
      subroutine set_rgba_image_2(num_x, num_y, rgba)
!
      integer, intent(in) :: num_x, num_y
      real(kind = 8), intent(inout) :: rgba(4,num_x*num_y)
      integer :: j1, j2, i, j, k
!
!
      j1 = num_y/3
      j2 = 2*num_y/3
!
      do j = 1, j1
        do i = 1, num_x
          k = i + (j-1)*num_x
          rgba(1,k) = dble(i-1) / dble(num_x)
          rgba(2,k) = 1.0
          rgba(3,k) = 1.0
          rgba(4,k) = 1.0
        end do
      end do
!
!
      do j = j1+1, j2
        do i = 1, num_x
          k = i + (j-1)*num_x
          rgba(1,k) = 1.0
          rgba(2,k) = dble(num_x-i) / dble(num_x)
          rgba(3,k) = 1.0
          rgba(4,k) = dble(num_x-1) / dble(num_x)
        end do
      end do
!
      do j = j2+1, num_y
        do i = 1, num_x
          k = i + (j-1)*num_x
          rgba(1,k) = 1.0
          rgba(2,k) = 1.0
          rgba(3,k) = dble(i-1) / dble(255)
          rgba(4,k) = 0.5
        end do
      end do
!
      end subroutine set_rgba_image_2
!  ---------------------------------------------------------------------
      end module png_test_from_f
!
!
      program fortran_png_test
!
      use png_test_from_f
      call s_png_test_from_f
!
      end program fortran_png_test
!
!  ---------------------------------------------------------------------
