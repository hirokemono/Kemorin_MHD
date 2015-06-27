!>@file  t_pvr_image_array.f90
!!       module t_pvr_image_array
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Structures for PVR Image data
!!
!!@verbatim
!!      subroutine alloc_pvr_image_array_type(n_pvr_pixel, pvr_img)
!!      subroutine dealloc_pvr_image_array_type(pvr_img)
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
!>    Number of pixels
        integer(kind = kint) :: num_pixel_xy
!>    Number of pixels in each direction
        integer(kind = kint) :: num_pixels(2)
!
!>    Stackes for subimages in each process for blending
        integer(kind = kint), pointer :: istack_image(:)
!>    Number of local pixels for blending
        integer(kind = kint) :: npixel_local
!
!>    Global real image data
        real(kind = kreal), pointer :: rgba_real_gl(:,:)
!>    Global real image data for left eye
        real(kind = kreal), pointer :: rgba_left_gl(:,:)
!>    Global real image data for right eye
        real(kind = kreal), pointer :: rgba_right_gl(:,:)
!
!>    RGB byte image data
        character(len = 1), pointer :: rgb_chara_gl(:,:)
!>    RGBA byte image data
        character(len = 1), pointer :: rgba_chara_gl(:,:)
!
!>    Local real image data
        real(kind = kreal), pointer :: rgba_lc(:,:)
!>    RGB byte image data
        character(len = 1), pointer :: rgb_chara_lc(:,:)
!
!>    Interger flag if image is exist
        integer(kind = kint), pointer :: iflag_mapped(:)
!>    Local depth of image
        real(kind = kreal), pointer :: depth_lc(:)
!
!>    Segmented real image data to be blended
        real(kind = kreal), pointer :: rgba_part(:,:,:)
!>    Segmented real image data after blended
        real(kind = kreal), pointer :: rgba_real_part(:,:)
!
!>    Order of image data with respect to distance
        integer(kind = kint), pointer :: ip_farther(:)
!>    average of depth of image
        real(kind = kreal), pointer :: ave_depth_gl(:)
      end type pvr_image_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_image_array_type(n_pvr_pixel, pvr_img)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      type(pvr_image_type), intent(inout) :: pvr_img
!
      integer(kind = kint) :: max_smp
!
!
      pvr_img%num_pixels(1:2) = n_pvr_pixel(1:2)
      pvr_img%num_pixel_xy = n_pvr_pixel(1)*n_pvr_pixel(2)
!
!
      if(my_rank .eq. 0) then
        allocate(pvr_img%rgb_chara_gl(3,pvr_img%num_pixel_xy))
        allocate(pvr_img%rgba_chara_gl(4,pvr_img%num_pixel_xy))
!
        allocate(pvr_img%rgba_left_gl(4,pvr_img%num_pixel_xy))
        allocate(pvr_img%rgba_right_gl(4,pvr_img%num_pixel_xy))
!
        allocate(pvr_img%rgba_real_gl(4,pvr_img%num_pixel_xy))
!
      else
        allocate(pvr_img%rgb_chara_gl(3,1))
        allocate(pvr_img%rgba_chara_gl(4,1))
!
        allocate(pvr_img%rgba_left_gl(4,1))
        allocate(pvr_img%rgba_right_gl(4,1))
!
        allocate(pvr_img%rgba_real_gl(4,1))
      end if
      pvr_img%rgba_real_gl =  0.0d0
      pvr_img%rgba_left_gl =  0.0d0
      pvr_img%rgba_right_gl = 0.0d0
!
      allocate(pvr_img%rgba_lc(4,pvr_img%num_pixel_xy))
      allocate(pvr_img%rgb_chara_lc(3,pvr_img%num_pixel_xy))
      pvr_img%rgba_lc = 0.0d0
!
      allocate(pvr_img%iflag_mapped(pvr_img%num_pixel_xy))
      allocate(pvr_img%depth_lc(pvr_img%num_pixel_xy))
      pvr_img%iflag_mapped = 0
      pvr_img%depth_lc = 0.0d0
!
      allocate(pvr_img%istack_image(0:nprocs))
!
      call count_number_4_smp(nprocs, ione, pvr_img%num_pixel_xy,       &
     &    pvr_img%istack_image, max_smp)
      pvr_img%npixel_local = pvr_img%istack_image(my_rank+1)            &
     &                      - pvr_img%istack_image(my_rank)
!
      allocate(pvr_img%rgba_part(4,pvr_img%npixel_local,nprocs))
      allocate(pvr_img%rgba_real_part(4,pvr_img%npixel_local))
!
      allocate(pvr_img%ave_depth_gl(nprocs))
      allocate(pvr_img%ip_farther(nprocs))
      pvr_img%ave_depth_gl = 0.0d0
      pvr_img%ip_farther = -1
!
      end subroutine alloc_pvr_image_array_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_image_array_type(pvr_img)
!
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      deallocate(pvr_img%rgb_chara_gl, pvr_img%rgba_chara_gl)
      deallocate(pvr_img%rgba_left_gl, pvr_img%rgba_right_gl)
      deallocate(pvr_img%rgba_real_gl)
!
      deallocate(pvr_img%ave_depth_gl, pvr_img%ip_farther)
      deallocate(pvr_img%rgba_real_part, pvr_img%rgba_part)
      deallocate(pvr_img%istack_image)
      deallocate(pvr_img%rgba_lc, pvr_img%rgb_chara_lc)
      deallocate(pvr_img%depth_lc, pvr_img%iflag_mapped)
!
      end subroutine dealloc_pvr_image_array_type
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_array
