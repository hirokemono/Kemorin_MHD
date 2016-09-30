!>@file  t_pvr_image_array.f90
!!       module t_pvr_image_array
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Structures for PVR Image data
!!
!!@verbatim
!!      subroutine alloc_pvr_image_array_type(view, pvr_rgb)
!!        type(pvr_view_parameter), intent(in) :: view
!!        type(pvr_segmented_img), intent(inout) :: pvr_img
!!      subroutine dealloc_pvr_image_array_type(pvr_img)
!!      subroutine alloc_pvr_subimage_flags(num_pixel_xy, pvr_img)
!!@endverbatim
!
      module t_pvr_image_array
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use PVR_image_transfer
!
      implicit  none
!
!>  Structure for PVR images
      type pvr_image_type
        integer(kind = kint) :: num_pixel_xy
!>    Number of pixels in each direction
        integer(kind = kint) :: num_pixels(2)
!
!>    Global real image data
        real(kind = kreal), pointer :: rgba_real_gl(:,:)
!
!>    RGB byte image data
        character(len = 1), pointer :: rgb_chara_gl(:,:)
!>    RGBA byte image data
        character(len = 1), pointer :: rgba_chara_gl(:,:)
!
!>    Local real image data
        real(kind = kreal), pointer :: rgba_real_lc(:,:)
!>    RGB byte image data
        character(len = 1), pointer :: rgb_chara_lc(:,:)
!
!>    Global real image data for left eye
        real(kind = kreal), pointer :: rgba_left_gl(:,:)
!>    Global real image data for right eye
        real(kind = kreal), pointer :: rgba_right_gl(:,:)
      end type pvr_image_type
!
!>  Structure for PVR images
      type pvr_segmented_img
        type(PVR_MPI_FLAGS) :: COMM
!
!>    Number of pixels
        integer(kind = kint) :: num_pixel_xy
!>    Order of image data with respect to distance
        integer(kind = kint), pointer :: iflag_img_pe(:)
!>    Interger flag if image is exist
        integer(kind = kint), pointer :: iflag_mapped(:)
!
!
!>    Number of overlapped domain (requered number of image)
        integer(kind = kint) :: num_overlap
!>    Number of overlapped domain (requered number of image)
        integer(kind = kint) :: ntot_overlap
!>    Number of overlapped domain (requered number of image)
        integer(kind = kint), pointer :: istack_overlap(:)
!
!
!>    Order of image data with respect to distance
        integer(kind = kint), pointer :: ip_closer(:,:)
!>    Segmented real image data to be blended
        real(kind = kreal), pointer :: depth_part(:,:)
!>    Segmented real image data to be blended
        real(kind = kreal), pointer :: rgba_part(:,:,:)
!>    Segmented real image data to be blended
        real(kind = kreal), pointer :: rgba_whole(:,:)
!
!>    Segmented real image data to be blended
        real(kind = kreal), pointer :: depth_recv(:)
!>    Segmented real image data to be blended
        real(kind = kreal), pointer :: rgba_recv(:,:)
!
!>    Number of pixels with image
        integer(kind = kint) :: npixel_img
!>    Number of distoributed pixels with image
        integer(kind = kint) :: npixel_img_local
!>    Stack of distoributed pixels with image
        integer(kind = kint), pointer :: istack_pixel(:)
!>    list of position of pixel for reduced data
        integer(kind = kint), pointer :: ipixel_small(:)
!>    Order of image data with respect to distance
        integer(kind = kint), pointer :: iflag_img_lc(:,:)
!>    Local depth of image excluding overlap
        real(kind = kreal), pointer :: depth_lc(:,:)
!>    Local real image data excluding overlap
        real(kind = kreal), pointer :: rgba_lc(:,:,:)
!>    Segmented real image data to be blended
        real(kind = kreal), pointer :: rgba_rank0(:,:)
      end type pvr_segmented_img
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_image_array_type(view, pvr_rgb)
!
      use t_control_params_4_pvr
!
      type(pvr_view_parameter), intent(in) :: view
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      pvr_rgb%num_pixels(1:2) = view%n_pvr_pixel(1:2)
      pvr_rgb%num_pixel_xy = view%n_pvr_pixel(1)*view%n_pvr_pixel(2)
!
!
      if(my_rank .eq. 0) then
        allocate(pvr_rgb%rgb_chara_gl(3,pvr_rgb%num_pixel_xy))
        allocate(pvr_rgb%rgba_chara_gl(4,pvr_rgb%num_pixel_xy))
!
        allocate(pvr_rgb%rgba_left_gl(4,pvr_rgb%num_pixel_xy))
        allocate(pvr_rgb%rgba_right_gl(4,pvr_rgb%num_pixel_xy))
!
        allocate(pvr_rgb%rgba_real_gl(4,pvr_rgb%num_pixel_xy))
!
      else
        allocate(pvr_rgb%rgb_chara_gl(3,1))
        allocate(pvr_rgb%rgba_chara_gl(4,1))
!
        allocate(pvr_rgb%rgba_left_gl(4,1))
        allocate(pvr_rgb%rgba_right_gl(4,1))
!
        allocate(pvr_rgb%rgba_real_gl(4,1))
      end if
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
!
      subroutine alloc_pvr_subimage_flags(num_pixel_xy, pvr_img)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_segmented_img), intent(inout) :: pvr_img
!
!
      call alloc_pvr_image_comm_status(pvr_img%COMM)
!
      pvr_img%num_pixel_xy = num_pixel_xy
      allocate(pvr_img%iflag_img_pe(pvr_img%num_pixel_xy))
      allocate(pvr_img%iflag_mapped(pvr_img%num_pixel_xy))
      pvr_img%iflag_img_pe = 0
      pvr_img%iflag_mapped = 0
!
      end subroutine alloc_pvr_subimage_flags
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_local_subimage(pvr_img)
!
      type(pvr_segmented_img), intent(inout) :: pvr_img
!
      integer(kind = kint) :: npix, nolp
!
!
      allocate(pvr_img%istack_pixel(0:nprocs))
      allocate(pvr_img%istack_overlap(0:nprocs))
      pvr_img%istack_pixel =   0
      pvr_img%istack_overlap = 0
!
      allocate(pvr_img%ipixel_small(pvr_img%npixel_img))
      pvr_img%ipixel_small = 0
!
      npix = pvr_img%npixel_img
      nolp = pvr_img%num_overlap
!
      allocate(pvr_img%iflag_img_lc(nolp,npix))
      allocate(pvr_img%depth_lc(nolp,npix))
      allocate(pvr_img%rgba_lc(4,nolp,npix))
      allocate(pvr_img%rgba_rank0(4,npix))
!
      pvr_img%iflag_img_lc = 0
      pvr_img%rgba_lc =   0.0d0
      pvr_img%depth_lc =  0.0d0
      pvr_img%rgba_rank0 = 0.0d0
!
      end subroutine alloc_pvr_local_subimage
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_subimage_array(pvr_img)
!
      type(pvr_segmented_img), intent(inout) :: pvr_img
!
      integer(kind = kint) :: npix, nolp
!
!
      nolp = pvr_img%ntot_overlap
      npix = pvr_img%npixel_img_local
!
      allocate(pvr_img%ip_closer(nolp,npix))
!
      allocate(pvr_img%depth_part(nolp,npix))
      allocate(pvr_img%depth_recv(nolp*npix))
!
      allocate(pvr_img%rgba_recv(4,nolp*npix))
      allocate(pvr_img%rgba_part(4,nolp,npix))
      allocate(pvr_img%rgba_whole(4,npix))
!
      pvr_img%ip_closer =  -1
      pvr_img%depth_part = 0.0d0
      pvr_img%rgba_part =  0.0d0
      pvr_img%rgba_whole =  0.0d0
!
      pvr_img%depth_recv =  0.0d0
      pvr_img%rgba_recv =   0.0d0
!
      end subroutine alloc_pvr_subimage_array
!
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
      subroutine dealloc_pvr_local_subimage(pvr_img)
!
      type(pvr_segmented_img), intent(inout) :: pvr_img
!
!
      call dealloc_pvr_image_comm_status(pvr_img%COMM)
!
      deallocate(pvr_img%depth_part, pvr_img%ip_closer)
      deallocate(pvr_img%depth_recv, pvr_img%rgba_recv)
      deallocate(pvr_img%rgba_part, pvr_img%rgba_whole)
      deallocate(pvr_img%depth_lc, pvr_img%rgba_lc, pvr_img%rgba_rank0)
      deallocate(pvr_img%iflag_img_lc)
      deallocate(pvr_img%istack_overlap)
      deallocate(pvr_img%istack_pixel, pvr_img%ipixel_small)
      deallocate(pvr_img%iflag_mapped, pvr_img%iflag_img_pe)
!
      end subroutine dealloc_pvr_local_subimage
!
!  ---------------------------------------------------------------------
!
      subroutine store_left_eye_image(pvr_rgb)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      if(my_rank .ne. 0) return
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
      if(my_rank .ne. 0) return
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
