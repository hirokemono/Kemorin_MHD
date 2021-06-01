!>@file  t_pvr_image_array.f90
!!       module t_pvr_image_array
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Structures for PVR Image data
!!
!!@verbatim
!!      subroutine alloc_pvr_image_array(n_pvr_pixel, pvr_rgb)
!!      subroutine dealloc_pvr_image_array(pvr_rgb)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!
!!      subroutine store_left_eye_image(pvr_rgb)
!!      subroutine add_left_eye_image(pvr_rgb)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!
!!      subroutine copy_pvr_image_file_param(org_pvr_rgb, rot_pvr_rgb)
!!        type(pvr_image_type), intent(in) :: org_pvr_rgb
!!        type(pvr_image_type), intent(inout) :: rot_pvr_rgb
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
!>    Global real image data for left eye
        real(kind = kreal), allocatable :: rgba_left_gl(:,:)
      end type pvr_image_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_image_array(n_pvr_pixel, pvr_rgb)
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
      allocate(pvr_rgb%rgba_left_gl(4,pvr_rgb%num_pixel_actual))
!
      allocate(pvr_rgb%rgba_real_gl(4,pvr_rgb%num_pixel_actual))
!
!$omp parallel workshare
      pvr_rgb%rgba_real_gl(1:4,1:pvr_rgb%num_pixel_actual) =  0.0d0
      pvr_rgb%rgba_left_gl(1:4,1:pvr_rgb%num_pixel_actual) =  0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_pvr_image_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_image_array(pvr_rgb)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      deallocate(pvr_rgb%rgba_left_gl)
      deallocate(pvr_rgb%rgba_real_gl)
!
      end subroutine dealloc_pvr_image_array
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
!  ---------------------------------------------------------------------
!
      subroutine copy_pvr_image_file_param(org_pvr_rgb, rot_pvr_rgb)
!
      type(pvr_image_type), intent(in) :: org_pvr_rgb
      type(pvr_image_type), intent(inout) :: rot_pvr_rgb
!
!
      rot_pvr_rgb%iflag_monitoring =   org_pvr_rgb%iflag_monitoring
      rot_pvr_rgb%id_pvr_file_type =   org_pvr_rgb%id_pvr_file_type
      rot_pvr_rgb%id_pvr_transparent = org_pvr_rgb%id_pvr_transparent
      rot_pvr_rgb%pvr_prefix =         org_pvr_rgb%pvr_prefix
!
      end subroutine copy_pvr_image_file_param
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_array
