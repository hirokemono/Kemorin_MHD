!>@file   t_pvr_image_stack_table.f90
!!@brief  module t_pvr_image_stack_table
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Work structure to make stencil buffer
!!
!!@verbatim
!!      subroutine alloc_pvr_image_stack_table(ntot_import, img_stack)
!!      subroutine alloc_depth_pixel_composit                           &
!!     &         (num_pvr_ray, ntot_recv_composit, img_stack)
!!      subroutine alloc_pvr_ipixel_4_composit(num_pixel_xy, img_stack)
!!        type(pvr_image_stack_table), intent(inout) :: img_stack
!!
!!      subroutine dealloc_pvr_image_stack_table(img_stack)
!!      subroutine dealloc_depth_pixel_composit(img_stack)
!!      subroutine dealloc_pvr_ipixel_4_composit(img_stack)
!!        type(pvr_image_stack_table), intent(inout) :: img_stack
!!
!!      subroutine composit_rendering_image                             &
!!     &         (img_stack, npixel_recved, rgba_subdomain,             &
!!     &          npixel_stacked, rgba_composit)
!!      subroutine check_rendering_image(id_file, iref,                 &
!!     &          img_stack, npixel_recved, rgba_subdomain,             &
!!     &          npixel_stacked, rgba_composit)
!!        type(pvr_image_stack_table), intent(in) :: img_stack
!!
!!      subroutine set_global_pixel_4_composit                          &
!!     &         (stencil_wk, npixel_4_composit, num_pixel_xy,          &
!!     &          ipixel_4_composit, item_4_composit)
!!      subroutine count_parallel_stencil_buffer                        &
!!     &         (stencil_wk, npixel_4_composit)
!!        type(stencil_buffer_work), intent(in) :: stencil_wk
!!      subroutine set_image_stacking_list                              &
!!     &       (num_pixel_xy, item_4_composit,                          &
!!     &          ntot_recv_pixel_composit, npixel_4_composit,          &
!!     &          ipix_4_composit, depth_pixel_composit,                &
!!     &          istack_composition, idx_recv_pixel_composit)
!!@endverbatim
!!
      module t_pvr_image_stack_table
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_calypso_comm_table
      use t_stencil_buffer_work
!
      implicit  none
!
      type pvr_image_stack_table
        integer(kind = kint) :: npixel_4_composit
        integer(kind = kint), allocatable :: istack_composition(:)
!
        integer(kind = kint), allocatable :: idx_recv_pixel_composit(:)
!
        integer(kind = kint), allocatable :: ipix_4_composit(:)
        real(kind = kreal), allocatable :: depth_pixel_composit(:)
        real(kind = kreal), allocatable :: depth_pvr_ray_start(:)
!
        integer(kind = kint), allocatable :: ipixel_4_composit(:)
        integer(kind = kint), allocatable :: item_4_composit(:)
      end type pvr_image_stack_table
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_image_stack_table(ntot_import, img_stack)
!
      integer(kind = kint), intent(in) :: ntot_import
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
      integer(kind = kint) :: n_image
!
      n_image = img_stack%npixel_4_composit
      allocate(img_stack%idx_recv_pixel_composit(ntot_import))
      allocate(img_stack%istack_composition(0:n_image))
!
      img_stack%istack_composition(0:n_image) = 0
      if(ntot_import .gt. 0) img_stack%idx_recv_pixel_composit = 0
!
      end subroutine alloc_pvr_image_stack_table
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_depth_pixel_composit                             &
     &         (num_pvr_ray, ntot_recv_composit, img_stack)
!
      integer(kind = kint), intent(in) :: ntot_recv_composit
      integer(kind = kint), intent(in) :: num_pvr_ray
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
!
      allocate(img_stack%ipix_4_composit(ntot_recv_composit))
      allocate(img_stack%depth_pixel_composit(ntot_recv_composit))
      allocate(img_stack%depth_pvr_ray_start(num_pvr_ray))
!
      if(ntot_recv_composit .gt. 0) then
!$omp parallel workshare
        img_stack%ipix_4_composit(1:ntot_recv_composit) = 0
        img_stack%depth_pixel_composit(1:ntot_recv_composit) = 0.0d0
!$omp end parallel workshare
      end if
!
      if(num_pvr_ray .gt. 0) then
!$omp parallel workshare
        img_stack%depth_pvr_ray_start(1:num_pvr_ray) = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_depth_pixel_composit
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_ipixel_4_composit(num_pixel_xy, img_stack)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
      integer(kind = kint) :: n_composit
!
      n_composit = img_stack%npixel_4_composit
      allocate(img_stack%ipixel_4_composit(n_composit))
      allocate(img_stack%item_4_composit(num_pixel_xy))
!
      if(n_composit .gt. 0) then
!$omp parallel workshare
        img_stack%ipixel_4_composit(1:n_composit) = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_pvr_ipixel_4_composit
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_image_stack_table(img_stack)
!
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
!
      deallocate(img_stack%idx_recv_pixel_composit)
      deallocate(img_stack%istack_composition)
!
      end subroutine dealloc_pvr_image_stack_table
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_depth_pixel_composit(img_stack)
!
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
!
      deallocate(img_stack%ipix_4_composit)
      deallocate(img_stack%depth_pixel_composit)
      deallocate(img_stack%depth_pvr_ray_start)
!
      end subroutine dealloc_depth_pixel_composit
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_ipixel_4_composit(img_stack)
!
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
      deallocate(img_stack%ipixel_4_composit)
      deallocate(img_stack%item_4_composit)
!
      end subroutine dealloc_pvr_ipixel_4_composit
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine composit_rendering_image                               &
     &         (img_stack, npixel_recved, rgba_subdomain,               &
     &          npixel_stacked, rgba_composit)
!
      use set_rgba_4_each_pixel
!
      type(pvr_image_stack_table), intent(in) :: img_stack
      integer(kind = kint), intent(in) :: npixel_recved, npixel_stacked
      real(kind = kreal), intent(in) :: rgba_subdomain(4,npixel_recved)
!
      real(kind = kreal), intent(inout)                                 &
     &      :: rgba_composit(4,npixel_stacked)
!
      integer(kind = kint) :: inum, ipix, ist, ied, irecv
!
!
!$omp parallel do private(ipix,ist,ied,inum,irecv)
      do ipix = 1, img_stack%npixel_4_composit
        ist = img_stack%istack_composition(ipix-1)
        ied = img_stack%istack_composition(ipix)
        do inum = ist+1, ied
          irecv = img_stack%idx_recv_pixel_composit(inum)
!
          call composite_alpha_blending(rgba_subdomain(1,irecv),        &
     &        rgba_composit(1,ipix))
        end do
      end do
!$omp end parallel do
!
      end subroutine composit_rendering_image
!
!  ---------------------------------------------------------------------
!
      subroutine check_rendering_image(id_file, iref,                   &
     &          img_stack, npixel_recved, rgba_subdomain,               &
     &          npixel_stacked, rgba_composit)
!
      use set_rgba_4_each_pixel
!
      integer(kind = kint), intent(in) :: id_file, iref
      type(pvr_image_stack_table), intent(in) :: img_stack
      integer(kind = kint), intent(in) :: npixel_recved, npixel_stacked
      real(kind = kreal), intent(in) :: rgba_subdomain(4,npixel_recved)
!
      real(kind = kreal), intent(in) :: rgba_composit(4,npixel_stacked)
!
      integer(kind = kint) :: inum, ipix, ist, ied, irecv
      real(kind = kreal) :: rgb_test(4)
      integer :: i
!
      rgb_test(1:4) = 0.0d0
!
      do ipix = 1, img_stack%npixel_4_composit
        ist = img_stack%istack_composition(ipix-1)
        ied = img_stack%istack_composition(ipix)
        do inum = ist+1, ied
          irecv = img_stack%idx_recv_pixel_composit(inum)
          if(img_stack%ipixel_4_composit(ipix) .eq. iref) then
            write(id_file,*) 'rgba_subdomain', iref, inum, irecv, &
     &              rgba_subdomain(1:4,irecv),  &
     &              img_stack%depth_pixel_composit(irecv)
!
            call composite_alpha_blending(rgba_subdomain(1,irecv),      &
     &          rgb_test(1))
          end if
        end do
      end do
!
      do i = 1, npixel_stacked
        if(img_stack%ipixel_4_composit(i) .eq. iref) then
          write(id_file,*) 'rgb_test', iref, i, rgb_test(1:4)
          write(id_file,*) 'rgba_composit', iref, i, rgba_composit(1:4,i)
        end if
      end do
!
      end subroutine check_rendering_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_global_pixel_4_composit                            &
     &         (stencil_wk, npixel_4_composit, num_pixel_xy,            &
     &          ipixel_4_composit, item_4_composit)
!
      type(stencil_buffer_work), intent(in) :: stencil_wk
      integer(kind = kint), intent(in) :: npixel_4_composit
      integer(kind = kint), intent(in) :: num_pixel_xy
!
      integer(kind = kint), intent(inout)                               &
     &      :: ipixel_4_composit(npixel_4_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: item_4_composit(num_pixel_xy)
!
      integer(kind = kint) :: ip, inum, ipix, ist
!
!
!$omp parallel workshare
      item_4_composit(1:num_pixel_xy) = 0
!$omp end parallel workshare
!
      do ip = 1, nprocs
        ist = stencil_wk%istack_recv_image(ip-1)
        ipix = stencil_wk%item_recv_image(ist+1)
        if(stencil_wk%irank_4_composit(ipix) .eq. my_rank) then
          do inum = 1, npixel_4_composit
            ipix = stencil_wk%item_recv_image(ist+inum)
            ipixel_4_composit(inum) = ipix
            item_4_composit(ipix) = inum
          end do
          exit
        end if
      end do
!
      end subroutine set_global_pixel_4_composit
!
!  ---------------------------------------------------------------------
!
      subroutine count_parallel_stencil_buffer                          &
     &         (stencil_wk, npixel_4_composit)
!
      type(stencil_buffer_work), intent(in) :: stencil_wk
      integer(kind = kint), intent(inout) :: npixel_4_composit
!
      integer(kind = kint) :: ip, ist, ipix
!
!
      npixel_4_composit = 0
      do ip = 1, nprocs
        ist = stencil_wk%istack_recv_image(ip-1)
        ipix = stencil_wk%item_recv_image(ist+1)
        if(stencil_wk%irank_4_composit(ipix) .eq. my_rank) then
          npixel_4_composit = stencil_wk%istack_recv_image(ip)          &
     &                       - stencil_wk%istack_recv_image(ip-1)
          exit
        end if
      end do
!
      end subroutine count_parallel_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_image_stacking_list                                &
     &         (num_pixel_xy, item_4_composit,                          &
     &          ntot_recv_pixel_composit, npixel_4_composit,            &
     &          ipix_4_composit, depth_pixel_composit,                  &
     &          istack_composition, idx_recv_pixel_composit)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: item_4_composit(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: ntot_recv_pixel_composit
      integer(kind = kint), intent(in)                                  &
     &      :: ipix_4_composit(ntot_recv_pixel_composit)
      real(kind = kreal), intent(in)                                    &
     &      :: depth_pixel_composit(ntot_recv_pixel_composit)
      integer(kind = kint), intent(in) :: npixel_4_composit
!
      integer(kind = kint), intent(inout)                               &
     &      :: istack_composition(0:npixel_4_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: idx_recv_pixel_composit(ntot_recv_pixel_composit)
!
      integer(kind = kint), allocatable :: itmp_recv_pixel_composit(:)
      real(kind = kreal), allocatable :: rwork_recv_pixel_composit(:)
!
      integer(kind = kint) :: inum, ipix, ist, ied, num, icou
!
!
      allocate(itmp_recv_pixel_composit(ntot_recv_pixel_composit))
      allocate(rwork_recv_pixel_composit(ntot_recv_pixel_composit))
!
!$omp parallel do
      do inum = 1, ntot_recv_pixel_composit
        ipix = ipix_4_composit(inum)
        itmp_recv_pixel_composit(inum) = item_4_composit(ipix)
        idx_recv_pixel_composit(inum) = inum
      end do
!$omp end parallel do
!
      call quicksort_w_index                                            &
     &   (ntot_recv_pixel_composit, itmp_recv_pixel_composit,           &
     &    ione, ntot_recv_pixel_composit, idx_recv_pixel_composit)
!
      do inum = 1, ntot_recv_pixel_composit
        icou = idx_recv_pixel_composit(inum)
        rwork_recv_pixel_composit(inum) = depth_pixel_composit(icou)
      end do
!
!$omp parallel workshare
      istack_composition(0:npixel_4_composit) = 0
!$omp end parallel workshare
      do inum = 1, ntot_recv_pixel_composit
        ipix = itmp_recv_pixel_composit(inum)
        istack_composition(ipix) = istack_composition(ipix) + 1
      end do
      do ipix = 1, npixel_4_composit
        istack_composition(ipix) = istack_composition(ipix-1)           &
     &                            + istack_composition(ipix)
      end do
!
!$omp parallel do private(ipix,ist,ied,num)
      do ipix = 1, npixel_4_composit
        ist = istack_composition(ipix-1)
        ied = istack_composition(ipix)
        num = ied - ist
        if(num .gt. 1) then
          call quicksort_real_w_index                                   &
     &       (num, rwork_recv_pixel_composit(ist+1),                    &
     &        ione, num, idx_recv_pixel_composit(ist+1))
        end if
      end do
!$omp end parallel do
!
      deallocate(itmp_recv_pixel_composit, rwork_recv_pixel_composit)
!
      end subroutine set_image_stacking_list
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_stack_table
