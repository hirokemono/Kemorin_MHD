!>@file  t_pvr_image_array.f90
!!       module t_pvr_image_array
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Structures for PVR Image data
!!
!!@verbatim
!!      subroutine alloc_pvr_image_array_type(n_pvr_pixel, pvr_rgb)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!
!!      subroutine dealloc_pvr_image_array_type(pvr_rgb)
!!
!!      subroutine store_left_eye_image(pvr_rgb)
!!      subroutine add_left_eye_image(pvr_rgb)
!!@endverbatim
!
      module t_pvr_image_array
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      implicit  none
!
!>  Structure for PVR images
      type pvr_image_type
!>        File prefix for image file
        character(len = kchara) :: pvr_prefix
!
!>        Transparent image flag
        integer(kind = kint) :: id_pvr_transparent = 0
!>        File format for image file
        integer(kind = kint) :: id_pvr_file_type = 0
!>        Monitoring mode flag
        integer(kind = kint) :: iflag_monitoring = 0
!
!>        MPI rank to putput each PVR image
        integer(kind = kint) :: irank_image_file = 0
!>        MPI rank for each PVR composttion arnge
        integer(kind = kint) :: irank_end_composit = 0
!>        Number of MPI rank to composit image
        integer(kind = kint) :: npe_img_composit =  0
!
!>        Number of pixels in each direction
        integer(kind = kint) :: num_pixels(2)
!>        Number of pixels (same value in all processes)
        integer(kind = kint) :: num_pixel_xy
!>        Number of pixels in each process
        integer(kind = kint) :: num_pixel_actual
!
!>    Global real image data
        real(kind = kreal), allocatable :: rgba_real_gl(:,:)
!
!>    RGB byte image data
        character(len = 1), allocatable :: rgb_chara_gl(:,:)
!>    RGBA byte image data
        character(len = 1), allocatable :: rgba_chara_gl(:,:)
!
!>    Local real image data
        real(kind = kreal), allocatable :: rgba_real_lc(:,:)
!>    RGB byte image data
        character(len = 1), allocatable :: rgb_chara_lc(:,:)
!
!>    Global real image data for left eye
        real(kind = kreal), allocatable :: rgba_left_gl(:,:)
!>    Global real image data for right eye
        real(kind = kreal), allocatable :: rgba_right_gl(:,:)
      end type pvr_image_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_image_array_type(n_pvr_pixel, pvr_rgb)
!
      use t_control_params_4_pvr
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      pvr_rgb%num_pixels(1:2) = n_pvr_pixel(1:2)
      pvr_rgb%num_pixel_xy = n_pvr_pixel(1) * n_pvr_pixel(2)
!
!
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        pvr_rgb%num_pixel_actual = pvr_rgb%num_pixel_xy
      else
        pvr_rgb%num_pixel_actual = 1
      end if
!
      allocate(pvr_rgb%rgb_chara_gl(3,pvr_rgb%num_pixel_actual))
      allocate(pvr_rgb%rgba_chara_gl(4,pvr_rgb%num_pixel_actual))
!
      allocate(pvr_rgb%rgba_left_gl(4,pvr_rgb%num_pixel_actual))
      allocate(pvr_rgb%rgba_right_gl(4,pvr_rgb%num_pixel_actual))
!
      allocate(pvr_rgb%rgba_real_gl(4,pvr_rgb%num_pixel_actual))
!
      pvr_rgb%rgba_real_gl =  0.0d0
      pvr_rgb%rgba_left_gl =  0.0d0
      pvr_rgb%rgba_right_gl = 0.0d0
!
      allocate(pvr_rgb%rgba_real_lc(4,pvr_rgb%num_pixel_xy))
      allocate(pvr_rgb%rgb_chara_lc(3,pvr_rgb%num_pixel_xy))
      pvr_rgb%rgba_real_lc = 0.0d0
!
      end subroutine alloc_pvr_image_array_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_image_array_type(pvr_rgb)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      deallocate(pvr_rgb%rgb_chara_gl, pvr_rgb%rgba_chara_gl)
      deallocate(pvr_rgb%rgba_left_gl, pvr_rgb%rgba_right_gl)
      deallocate(pvr_rgb%rgba_real_gl)
!
      deallocate(pvr_rgb%rgba_real_lc, pvr_rgb%rgb_chara_lc)
!
      end subroutine dealloc_pvr_image_array_type
!
!  ---------------------------------------------------------------------
!
      subroutine store_left_eye_image(pvr_rgb)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!$omp parallel workshare
      pvr_rgb%rgba_left_gl(1,:) = pvr_rgb%rgba_real_gl(1,:)
!      pvr_rgb%rgba_left_gl(2,:) = pvr_rgb%rgba_real_gl(2,:)
!      pvr_rgb%rgba_left_gl(3,:) = pvr_rgb%rgba_real_gl(3,:)
      pvr_rgb%rgba_left_gl(4,:) = pvr_rgb%rgba_real_gl(4,:)
!$omp end parallel workshare
!
      end subroutine store_left_eye_image
!
!  ---------------------------------------------------------------------
!
      subroutine add_left_eye_image(pvr_rgb)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!$omp parallel workshare
        pvr_rgb%rgba_real_gl(1,:) =  pvr_rgb%rgba_left_gl(1,:)
!        pvr_rgb%rgba_real_gl(2,:) =  pvr_rgb%rgba_left_gl(2,:)
!        pvr_rgb%rgba_real_gl(3,:) =  pvr_rgb%rgba_left_gl(3,:)
        pvr_rgb%rgba_real_gl(4,:) =  pvr_rgb%rgba_real_gl(4,:)          &
     &                             + pvr_rgb%rgba_left_gl(4,:)
!$omp end parallel workshare
!
      end subroutine add_left_eye_image
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_array
