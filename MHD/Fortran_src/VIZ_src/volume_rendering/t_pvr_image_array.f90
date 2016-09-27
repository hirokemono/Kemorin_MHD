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
        real(kind = kreal), pointer :: old_rgba_lc(:,:)
!>    RGB byte image data
        character(len = 1), pointer :: rgb_chara_lc(:,:)
!
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
!
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
      allocate(pvr_img%old_rgba_lc(4,pvr_img%num_pixel_xy))
      allocate(pvr_img%rgb_chara_lc(3,pvr_img%num_pixel_xy))
      pvr_img%old_rgba_lc = 0.0d0
!
      allocate(pvr_img%iflag_img_pe(pvr_img%num_pixel_xy))
      allocate(pvr_img%iflag_mapped(pvr_img%num_pixel_xy))
      pvr_img%iflag_img_pe = 0
      pvr_img%iflag_mapped = 0
!
      end subroutine alloc_pvr_image_array_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_local_subimage(pvr_img)
!
      type(pvr_image_type), intent(inout) :: pvr_img
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
!
!
      end subroutine alloc_pvr_local_subimage
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_subimage_array(pvr_img)
!
      type(pvr_image_type), intent(inout) :: pvr_img
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
      subroutine dealloc_pvr_image_array_type(pvr_img)
!
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      deallocate(pvr_img%rgb_chara_gl, pvr_img%rgba_chara_gl)
      deallocate(pvr_img%rgba_left_gl, pvr_img%rgba_right_gl)
      deallocate(pvr_img%rgba_real_gl)
!
      deallocate(pvr_img%old_rgba_lc, pvr_img%rgb_chara_lc)
      deallocate(pvr_img%iflag_mapped)
      deallocate(pvr_img%iflag_img_pe)
!
      end subroutine dealloc_pvr_image_array_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_local_subimage(pvr_img)
!
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      deallocate(pvr_img%depth_part, pvr_img%ip_closer)
      deallocate(pvr_img%depth_recv, pvr_img%rgba_recv)
      deallocate(pvr_img%rgba_part, pvr_img%rgba_whole)
      deallocate(pvr_img%depth_lc, pvr_img%rgba_lc, pvr_img%rgba_rank0)
      deallocate(pvr_img%iflag_img_lc)
      deallocate(pvr_img%istack_overlap)
      deallocate(pvr_img%istack_pixel, pvr_img%ipixel_small)
!
      end subroutine dealloc_pvr_local_subimage
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_array
