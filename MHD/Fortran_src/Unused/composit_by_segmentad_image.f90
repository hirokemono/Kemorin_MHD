!>@file  composit_by_segmentad_image.f90
!!       module composit_by_segmentad_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine s_composit_by_segmentad_image                        &
!!     &         (istep_pvr, iflag_PVR_time, ist_elapsed_PVR,           &
!!     &          pvr_start, pvr_stencil, pvr_img, pvr_rgb)
!!        type(pvr_ray_start_type), intent(in) :: pvr_start
!!        type(pvr_stencil_buffer), intent(in) :: pvr_stencil
!!        type(pvr_segmented_img), intent(inout) :: pvr_img
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module composit_by_segmentad_image
!
      use m_precision
      use m_work_time
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
      private :: compare_image_composition
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_composit_by_segmentad_image                          &
     &         (istep_pvr, iflag_PVR_time, ist_elapsed_PVR,             &
     &          pvr_start, pvr_stencil, pvr_img, pvr_rgb)
!
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use t_pvr_stencil_buffer
      use composite_pvr_images
      use PVR_image_transfer
!
      integer(kind = kint), intent(in) :: istep_pvr
      logical, intent(in) :: iflag_PVR_time
      integer(kind = kint), intent(in) :: ist_elapsed_PVR
!
      type(pvr_ray_start_type), intent(in) :: pvr_start
      type(pvr_stencil_buffer), intent(in) :: pvr_stencil
      type(pvr_segmented_img), intent(inout) :: pvr_img
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      real(kind = kreal), allocatable :: rgba_gl(:,:)
      integer(kind = kint) :: i, j, k, ipix
!
!
      allocate(rgba_gl(4,pvr_rgb%num_pixel_actual))
!$omp parallel workshare
      rgba_gl(1:4,1:pvr_rgb%num_pixel_actual)                           &
     &          = pvr_rgb%rgba_real_gl(1:4,1:pvr_rgb%num_pixel_actual)
!$omp end parallel workshare

!
      if(iflag_debug .gt. 0) write(*,*) 'copy_segmented_image'
      call copy_segmented_image(pvr_start%num_pvr_ray,                  &
     &    pvr_start%id_pixel_start, pvr_start%rgba_ray,                 &
     &    pvr_img%num_overlap, pvr_rgb%num_pixel_xy,                    &
     &    pvr_img%npixel_img, pvr_img%iflag_img_pe,                     &
     &    pvr_img%iflag_mapped, pvr_img%rgba_lc)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+3)
!
!       Outut semented image
      if(i_debug .gt. 0) then
        do i = 1, pvr_img%num_overlap
          j = pvr_img%istack_overlap(my_rank) + i
          do k = 1, pvr_img%npixel_img
            ipix = pvr_img%ipixel_small(k)
            pvr_rgb%rgba_real_lc(1:4,ipix) = pvr_img%rgba_lc(1:4,i,k)
          end do
          call sel_write_pvr_local_img(j, istep_pvr, pvr_rgb)
        end do
      end if
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+4)
      if(iflag_debug .gt. 0) write(*,*) 'distribute_segmented_images'
      call distribute_segmented_images                                  &
     &   (pvr_img%num_overlap, pvr_img%istack_overlap,                  &
     &    pvr_img%ntot_overlap, pvr_img%npixel_img,                     &
     &    pvr_img%istack_pixel, pvr_img%npixel_img_local,               &
     &    pvr_img%rgba_lc, pvr_img%rgba_recv, pvr_img%rgba_part,        &
     &    pvr_img%COMM)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'blend_image_over_segments'
      call blend_image_over_segments                                    &
     &   (pvr_img%ntot_overlap, pvr_img%npixel_img_local,               &
     &    pvr_img%ip_closer, pvr_img%rgba_part, pvr_img%rgba_whole)
!
      if(iflag_debug .gt. 0) write(*,*) 'collect_segmented_images'
      call collect_segmented_images(pvr_rgb%irank_image_file,           &
     &    pvr_img%npixel_img_local, pvr_img%istack_pixel,               &
     &    pvr_img%npixel_img, pvr_rgb%num_pixel_xy,                     &
     &    pvr_img%ipixel_small, pvr_img%rgba_whole,                     &
     &    pvr_img%rgba_rank0, pvr_rgb%rgba_real_gl, pvr_img%COMM)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+4)
!
      call compare_image_composition                                    &
     &   (pvr_stencil, pvr_img, pvr_rgb, rgba_gl)
      deallocate(rgba_gl)
!
      end subroutine s_composit_by_segmentad_image
!
!  ---------------------------------------------------------------------
!
      subroutine compare_image_composition                              &
     &         (pvr_stencil, pvr_img, pvr_rgb, rgba_gl)
!
      use t_pvr_image_array
      use t_pvr_stencil_buffer
      use composite_pvr_images
      use composite_pvr_images
!
      type(pvr_stencil_buffer), intent(in) :: pvr_stencil
      type(pvr_segmented_img), intent(in) :: pvr_img
      type(pvr_image_type), intent(in) :: pvr_rgb
      real(kind = kreal), intent(in)                                    &
     &           :: rgba_gl(4,pvr_rgb%num_pixel_actual)
!
      integer(kind = kint) :: num_fail
      integer(kind = kint), allocatable :: ilist_fail(:), ilist_tmp(:)
      integer(kind = kint) :: i, iflag
!
!
      write(my_rank+50,*) 'pixel check', my_rank, size(rgba_gl,2),      &
     &          size(pvr_rgb%rgba_real_gl,2), pvr_rgb%num_pixel_xy
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        num_fail = 0
        allocate(ilist_fail(num_fail))
        do i = 1, pvr_rgb%num_pixel_xy
          iflag = 0
          if(rgba_gl(1,i) .ne. pvr_rgb%rgba_real_gl(1,i)) iflag = 1
          if(rgba_gl(2,i) .ne. pvr_rgb%rgba_real_gl(2,i)) iflag = 1
          if(rgba_gl(3,i) .ne. pvr_rgb%rgba_real_gl(3,i)) iflag = 1
          if(rgba_gl(4,i) .ne. pvr_rgb%rgba_real_gl(4,i))  iflag = 1
          if(iflag .gt. 0) then
            write(my_rank+50,*) 'rgba_gl', i, rgba_gl(1:4,i)
            write(my_rank+50,*) 'rgba_real_gl', i,                      &
     &                         pvr_rgb%rgba_real_gl(1:4,i)
!
            allocate(ilist_tmp(num_fail))
            ilist_tmp(1:num_fail) = ilist_fail(1:num_fail)
            deallocate(ilist_fail)
            num_fail = num_fail + 1
            allocate(ilist_fail(num_fail))
            ilist_fail(1:num_fail-1) = ilist_tmp(1:num_fail-1)
            ilist_fail(num_fail) = i
            deallocate(ilist_tmp)
          end if
        end do
      end if
!
      call mpi_bcast(num_fail, 1, CALYPSO_INTEGER,                      &
     &        int(pvr_rgb%irank_image_file), CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. pvr_rgb%irank_image_file)                         &
     &      allocate(ilist_fail(num_fail))
      call mpi_bcast(ilist_fail, num_fail, CALYPSO_INTEGER,             &
     &    int(pvr_rgb%irank_image_file), CALYPSO_COMM, ierr_MPI)
!
      do i = 1, num_fail
        call check_rendering_image                                      &
     &     (50+my_rank, ilist_fail(i), pvr_stencil%img_stack,           &
     &      pvr_stencil%npixel_recved, pvr_stencil%rgba_subdomain,      &
     &      pvr_stencil%npixel_stacked, pvr_stencil%rgba_composit)

        call check_image_over_segments(my_rank+50, ilist_fail(i),       &
     &      pvr_img%ntot_overlap, pvr_img%npixel_img_local,             &
     &      pvr_img%istack_pixel, pvr_rgb%num_pixel_xy,                 &
     &      pvr_img%iflag_img_pe, pvr_img%ip_closer, pvr_img%rgba_part)
      end do
      deallocate(ilist_fail)
!
      end subroutine compare_image_composition
!
!  ---------------------------------------------------------------------
!
      end module composit_by_segmentad_image
