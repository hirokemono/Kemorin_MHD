!>@file   t_rotation_pvr_images.f90
!!@brief  module t_rotation_pvr_images
!!
!!@date  Programmed by H.Matsui in May, 2021
!
!>@brief Structure to output rotation images
!!
!!@verbatim
!!      subroutine init_rot_pvr_image_arrays                            &
!!     &         (view_param, pvr_rgb, rot_imgs)
!!      subroutine dealloc_rot_pvr_image_arrays(view_param, rot_imgs)
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        type(pvr_image_type), intent(in) :: pvr_rgb(2)
!!        type(rotation_pvr_images), intent(inout) :: rot_imgs
!!@endverbatim
!
      module t_rotation_pvr_images
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use t_pvr_image_array
      use t_control_params_4_pvr
!
      implicit  none
!
!
!>      Structure of PVR images for rotation
      type rotation_pvr_images
!>        Structure of each PVR image in rotation
        type(pvr_image_type), allocatable :: rot_pvr_rgb(:)
      end type rotation_pvr_images
!
      private :: alloc_rot_pvr_image_arrays
      private :: set_rank_to_write_rot_images
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_rot_pvr_image_arrays                              &
     &         (view_param, pvr_rgb, rot_imgs)
!
      type(pvr_view_parameter), intent(in) :: view_param
!
      type(pvr_image_type), intent(in) :: pvr_rgb
      type(rotation_pvr_images), intent(inout) :: rot_imgs
!
      integer(kind = kint) :: i_rot
!
!
      call alloc_rot_pvr_image_arrays                                   &
     &   (view_param%istart_rot, view_param%iend_rot, rot_imgs)
!
      call set_rank_to_write_rot_images(pvr_rgb,                        &
     &    view_param%istart_rot, view_param%iend_rot,                   &
     &    rot_imgs%rot_pvr_rgb)
      do i_rot = view_param%istart_rot, view_param%iend_rot
        call alloc_pvr_image_array                                      &
     &     (pvr_rgb%num_pixels, rot_imgs%rot_pvr_rgb(i_rot))
      end do
!
      if(my_rank .gt. 0) return
      do i_rot = view_param%istart_rot, view_param%iend_rot
        write(*,*) i_rot, 'rot_pvr_rgb%irank_image_file', &
     &                  rot_pvr_rgb%irank_image_file(i_rot), &
     &                  rot_pvr_rgb%irank_end_composit(i_rot), &
     &                  rot_pvr_rgb%npe_img_composit(i_rot)
      end do
!
      end subroutine init_rot_pvr_image_arrays
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_rot_pvr_image_arrays(view_param, rot_imgs)
!
      type(pvr_view_parameter), intent(in) :: view_param
      type(rotation_pvr_images), intent(inout) :: rot_imgs
!
      integer(kind = kint) :: i_rot
!
!
      do i_rot = view_param%istart_rot, view_param%iend_rot
        call dealloc_pvr_image_array(rot_imgs%rot_pvr_rgb(i_rot))
      end do
      deallocate(rot_imgs%rot_pvr_rgb)
!
      end subroutine dealloc_rot_pvr_image_arrays
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_rot_pvr_image_arrays(ist_rot, ied_rot, rot_imgs)
!
      integer(kind = kint), intent(in) :: ist_rot, ied_rot
      type(rotation_pvr_images), intent(inout) :: rot_imgs
!
!
      allocate(rot_imgs%rot_pvr_rgb(ist_rot:ied_rot))
!
      end subroutine alloc_rot_pvr_image_arrays
!
!  ---------------------------------------------------------------------
!
      subroutine set_rank_to_write_rot_images                           &
     &         (org_pvr_rgb, ist_rot, ied_rot, rot_pvr_rgb)
!
      integer(kind = kint), intent(in) :: ist_rot, ied_rot
      type(pvr_image_type), intent(in) :: org_pvr_rgb
      type(pvr_image_type), intent(inout)                               &
     &                     :: rot_pvr_rgb(ist_rot:ied_rot)
!
      integer(kind = kint) :: i_rot, num_rot
!
!
      num_rot = ied_rot - ist_rot + 1
!$omp parallel do
      do i_rot = ist_rot, ied_rot
        call copy_pvr_image_file_param(org_pvr_rgb, rot_pvr_rgb(i_rot))
!
        rot_pvr_rgb(i_rot)%irank_image_file                             &
     &          = int(dble(nprocs) * dble(i_rot-1) / dble(num_rot))
      end do
!$omp end parallel do
!$omp parallel do
      do i_rot = ist_rot, ied_rot - 1
        if(rot_pvr_rgb(i_rot+1)%irank_image_file                        &
     &      .eq. rot_pvr_rgb(i_rot)%irank_image_file) then
          rot_pvr_rgb(i_rot)%irank_end_composit                         &
     &          = rot_pvr_rgb(i_rot)%irank_image_file
          rot_pvr_rgb(i_rot)%npe_img_composit =   1
        else
          rot_pvr_rgb(i_rot)%irank_end_composit                         &
     &          = rot_pvr_rgb(i_rot+1)%irank_image_file - 1
          rot_pvr_rgb(i_rot)%npe_img_composit                           &
     &        = rot_pvr_rgb(i_rot+1)%irank_image_file                   &
     &         - rot_pvr_rgb(i_rot)%irank_image_file
        end if
      end do
!$omp end parallel do
!
      rot_pvr_rgb(ied_rot)%irank_end_composit = nprocs - 1
      rot_pvr_rgb(ied_rot)%npe_img_composit                             &
     &        = rot_pvr_rgb(ied_rot+1)%irank_end_composit               &
     &         - rot_pvr_rgb(ied_rot)%irank_image_file + 1
!
      end subroutine set_rank_to_write_rot_images
!
!  ---------------------------------------------------------------------
!
      end module t_rotation_pvr_images
