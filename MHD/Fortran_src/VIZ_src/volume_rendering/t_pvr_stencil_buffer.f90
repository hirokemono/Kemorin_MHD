!>@file   t_pvr_stencil_buffer.f90
!!@brief  module t_pvr_stencil_buffer
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Work structure to make stencil buffer
!!
!!@verbatim
!!      subroutine const_pvr_stencil_buffer(irank_image_file,           &
!!     &          num_pixel_xy, pvr_start, pvr_stencil)
!!      subroutine collect_rendering_image(pvr_stencil,                 &
!!     &          num_pvr_ray, rgba_ray)
!!      subroutine dealloc_pvr_stencil_buffer(pvr_stencil)
!!        type(pvr_ray_start_type), intent(in) :: pvr_start
!!        type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!!@endverbatim
!!
      module t_pvr_stencil_buffer
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_calypso_comm_table
      use t_pvr_ray_startpoints
      use t_pvr_image_stack_table
      use t_stencil_buffer_work
!
      implicit  none
!
      type pvr_stencil_buffer
        type(pvr_image_stack_table) :: img_stack
        type(calypso_comm_table) :: img_output_tbl
        type(calypso_comm_table) :: img_composit_tbl
!
        integer(kind = kint) :: num_pixel_recv
        real(kind = kreal), allocatable :: rgba_gl(:,:)
        integer(kind = kint) :: npixel_recved
        real(kind = kreal), allocatable :: rgba_subdomain(:,:)
        integer(kind = kint) :: npixel_stacked
        real(kind = kreal), allocatable :: rgba_composit(:,:)
      end type pvr_stencil_buffer
!
      character(len=kchara), parameter, private                         &
     &                      :: check_fhead = 'pvr_composition_check'
      integer(kind = kint), parameter, private :: id_file = 49
!
      private :: set_pvr_stencil_buffer
      private :: alloc_pvr_stencil_buffer, reset_pvr_stencil_buffer
      private :: check_composit_communication
      private :: check_img_output_communication
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_pvr_stencil_buffer                               &
     &         (num_pixel_xy, pvr_start, pvr_stencil)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_ray_start_type), intent(in) :: pvr_start
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!
      type(stencil_buffer_work) :: stencil_wk
!
      integer :: irank_image_file
!
!
      irank_image_file = pvr_start%irank_composit_ref
!
      call const_stencil_buffer_work                                    &
     &   (irank_image_file, num_pixel_xy, pvr_start, stencil_wk)
!
      call set_pvr_stencil_buffer                                       &
     &   (irank_image_file, num_pixel_xy, pvr_start, stencil_wk,        &
     &    pvr_stencil%num_pixel_recv, pvr_stencil%img_output_tbl,       &
     &    pvr_stencil%img_composit_tbl, pvr_stencil%img_stack)
      call dealloc_stencil_buffer_work(stencil_wk)
!
      call alloc_pvr_stencil_buffer(pvr_stencil)
!
      end subroutine const_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine collect_rendering_image(pvr_stencil,                   &
     &          num_pvr_ray, rgba_ray)
!
      use calypso_SR_type
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      real(kind = kreal), intent(in) :: rgba_ray(4,num_pvr_ray)
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!
!
      call reset_pvr_stencil_buffer(pvr_stencil)
      call calypso_SR_type_N(0, ifour, pvr_stencil%img_composit_tbl,    &
     &    num_pvr_ray, pvr_stencil%npixel_recved,                       &
     &    rgba_ray(1,1), pvr_stencil%rgba_subdomain(1,1))
!
      call composit_rendering_image                                     &
     &   (pvr_stencil%img_composit_tbl, pvr_stencil%img_stack,          &
     &    pvr_stencil%rgba_subdomain, pvr_stencil%rgba_composit)
!
      call calypso_SR_type_N(0, ifour, pvr_stencil%img_output_tbl,      &
     &    pvr_stencil%npixel_stacked, pvr_stencil%num_pixel_recv,       &
     &    pvr_stencil%rgba_composit(1,1), pvr_stencil%rgba_gl(1,1))
!
      end subroutine collect_rendering_image
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_stencil_buffer(pvr_stencil)
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!
!
      deallocate(pvr_stencil%rgba_gl)
      deallocate(pvr_stencil%rgba_subdomain)
      deallocate(pvr_stencil%rgba_composit)
!
      call dealloc_pvr_image_stack_table(pvr_stencil%img_stack)
      call dealloc_calypso_comm_table(pvr_stencil%img_output_tbl)
      call dealloc_calypso_comm_table(pvr_stencil%img_composit_tbl)
!
      end subroutine dealloc_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_stencil_buffer(pvr_stencil)
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!
!
      pvr_stencil%npixel_recved                                         &
     &      = pvr_stencil%img_composit_tbl%ntot_import
      allocate(pvr_stencil%rgba_subdomain(4,pvr_stencil%npixel_recved))
!
      pvr_stencil%npixel_stacked                                        &
     &      = pvr_stencil%img_stack%npixel_4_composit
      allocate(pvr_stencil%rgba_composit(4,pvr_stencil%npixel_stacked))
!
      allocate(pvr_stencil%rgba_gl(4,pvr_stencil%num_pixel_recv))
!
      end subroutine alloc_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_stencil_buffer(pvr_stencil)
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!
!
      if(pvr_stencil%npixel_recved .gt. 0) then
!$omp parallel workshare
      pvr_stencil%rgba_subdomain(1:4,1:pvr_stencil%npixel_recved)       &
     &                                                         = 0.0d0
!$omp end parallel workshare
      end if
!
      if(pvr_stencil%npixel_stacked .gt. 0) then
!$omp parallel workshare
      pvr_stencil%rgba_composit(1:4,1:pvr_stencil%npixel_stacked)       &
     &                                                         = 0.0d0
!$omp end parallel workshare
      end if
!
      if(pvr_stencil%num_pixel_recv .gt. 0) then
!$omp parallel workshare
        pvr_stencil%rgba_gl(1:4,1:pvr_stencil%num_pixel_recv) = 0.0d0
!$omp end parallel workshare
      end if
!
!
      end subroutine reset_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_stencil_buffer                                 &
     &         (irank_image_file, num_pixel_xy, pvr_start, stencil_wk,  &
     &          num_pixel_recv, img_output_tbl, img_composit_tbl,       &
     &          img_stack)
!
      use quicksort
      use calypso_SR_type
      use const_comm_tbl_img_output
      use const_comm_tbl_img_composit
      use set_parallel_file_name
!
      integer, intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_ray_start_type), intent(in) :: pvr_start
      type(stencil_buffer_work), intent(in)  :: stencil_wk
!
      integer(kind = kint), intent(inout) :: num_pixel_recv
      type(calypso_comm_table), intent(inout) :: img_output_tbl
      type(calypso_comm_table), intent(inout) :: img_composit_tbl
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
      integer(kind = kint), allocatable :: ipixel_4_composit(:)
      integer(kind = kint), allocatable :: item_4_composit(:)
!
      integer(kind = kint), allocatable :: ipix_recv_pixel_composit(:)
      real(kind = kreal), allocatable :: depth_recv_pixel_composit(:)
!
      character(len=kchara) :: fname_tmp, file_name
!
!
      call count_parallel_stencil_buffer                                &
     &   (stencil_wk, img_stack%npixel_4_composit)
!
!
      call s_const_comm_tbl_img_output                                  &
     &   (stencil_wk, irank_image_file, num_pixel_xy,                   &
     &    img_stack%npixel_4_composit, num_pixel_recv, img_output_tbl)
!
      allocate(ipixel_4_composit(img_stack%npixel_4_composit))
      allocate(item_4_composit(num_pixel_xy))
!
      call set_global_pixel_4_composit                                  &
     &         (stencil_wk, img_stack%npixel_4_composit, num_pixel_xy,  &
     &          ipixel_4_composit, item_4_composit)
!
!
!
      call s_const_comm_tbl_img_composit                                &
     &   (num_pixel_xy, stencil_wk%irank_4_composit,                    &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    img_composit_tbl)
!
      allocate(ipix_recv_pixel_composit(img_composit_tbl%ntot_import))
      allocate(depth_recv_pixel_composit(img_composit_tbl%ntot_import))
!
      call calypso_SR_type_int(0, img_composit_tbl,                     &
     &    pvr_start%num_pvr_ray, img_composit_tbl%ntot_import,          &
     &    pvr_start%id_pixel_start, ipix_recv_pixel_composit)
!
      call calypso_SR_type_1(0, img_composit_tbl,                       &
     &    pvr_start%num_pvr_ray, img_composit_tbl%ntot_import,          &
     &    pvr_start%xx_pvr_ray_start(1,3), depth_recv_pixel_composit)
!
      call alloc_pvr_image_stack_table                                  &
     &   (img_composit_tbl%ntot_import, img_stack)
      call set_image_stacking_list(num_pixel_xy, item_4_composit,       &
     &    img_composit_tbl%ntot_import, img_stack%npixel_4_composit,    &
     &    ipix_recv_pixel_composit, depth_recv_pixel_composit,          &
     &    img_stack%istack_composition,                                 &
     &    img_stack%idx_recv_pixel_composit)
!
!
      if(i_debug .le. 0) return
      fname_tmp = add_int_suffix(my_rank, check_fhead)
      file_name = add_dat_extension(fname_tmp)
      open(id_file, file = file_name)
      call check_img_output_communication(id_file,                      &
     &    img_stack, img_output_tbl, num_pixel_xy, num_pixel_recv,      &
     &    stencil_wk%irev_recv_image, ipixel_4_composit)
!
      call check_composit_communication(id_file,                        &
     &    pvr_start, img_composit_tbl, img_stack,                       &
     &    ipix_recv_pixel_composit, depth_recv_pixel_composit)
      close(id_file)
!
      end subroutine set_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_composit_communication                           &
     &         (id_file, pvr_start, img_composit_tbl, img_stack,        &
     &          ipix_recv_pixel_composit, depth_recv_pixel_composit)
!
      integer(kind = kint), intent(in) :: id_file
      type(pvr_ray_start_type), intent(in) :: pvr_start
      type(calypso_comm_table), intent(in) :: img_composit_tbl
      type(pvr_image_stack_table), intent(in) :: img_stack
      integer(kind = kint), intent(in)                                  &
     &      :: ipix_recv_pixel_composit(img_composit_tbl%ntot_import)
      real(kind = kreal), intent(in)                                    &
     &      :: depth_recv_pixel_composit(img_composit_tbl%ntot_import)
!
      integer(kind = kint) :: inum, ipix, icou, ist, num, ip
!
!
      write(id_file,*) 'nrank_export', img_composit_tbl%nrank_export
      do ip = 1, img_composit_tbl%nrank_export
        ist = img_composit_tbl%istack_export(ip-1)
        num = img_composit_tbl%istack_export(ip) - ist
        write(id_file,*) 'img_composit_tbl%irank_export',               &
     &        ip, img_composit_tbl%irank_export(ip), ist, num
        do inum = 1, num
          icou = img_composit_tbl%item_export(ist+inum)
          write(id_file,*) inum, icou,   &
     &                pvr_start%id_pixel_start(icou), &
     &                pvr_start%xx_pvr_ray_start(icou,3)
        end do
      end do
!
      write(id_file,*) 'img_composit_tbl%ntot_import',                  &
     &       img_composit_tbl%ntot_import, img_stack%npixel_4_composit
      do ipix = 1, img_stack%npixel_4_composit
        ist = img_stack%istack_composition(ipix-1)
        num = img_stack%istack_composition(ipix) - ist
        write(id_file,*) 'idx_recv_pixel_composit', ist, num
        do inum = 1, num
          icou = img_stack%idx_recv_pixel_composit(ist+inum)
          write(id_file,*) inum, ipix, icou,                            &
     &                ipix_recv_pixel_composit(icou),                   &
     &                depth_recv_pixel_composit(icou)
        end do
      end do
      close(id_file)
!
      end subroutine check_composit_communication
!
!  ---------------------------------------------------------------------
!
      subroutine check_img_output_communication(id_file,                &
     &          img_stack, img_output_tbl, num_pixel_xy,                &
     &          num_pixel_recv, irev_recv_image, ipixel_4_composit)
!
      use calypso_SR_type
!
      integer(kind = kint), intent(in) :: id_file
      type(pvr_image_stack_table), intent(in) :: img_stack
      type(calypso_comm_table), intent(in) :: img_output_tbl
      integer(kind = kint), intent(in) :: num_pixel_xy, num_pixel_recv
      integer(kind = kint), intent(in) :: irev_recv_image(num_pixel_xy)
      integer(kind = kint), intent(in)                                  &
     &               :: ipixel_4_composit(img_stack%npixel_4_composit)
!
      integer(kind = kint), allocatable :: ipixel_check(:)
      integer(kind = kint) :: ipix
!
!
      allocate(ipixel_check(num_pixel_recv))
      call calypso_SR_type_int(0, img_output_tbl,                       &
     &    img_stack%npixel_4_composit, num_pixel_recv,                  &
     &    ipixel_4_composit, ipixel_check)
!
      write(id_file,*) 'ipixel_check', num_pixel_recv, num_pixel_xy
      do ipix = 1, num_pixel_recv
        write(id_file,*)                                                &
             ipix, ipixel_check(ipix), irev_recv_image(ipix)
      end do
      deallocate(ipixel_check)
!
      end subroutine check_img_output_communication
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_stencil_buffer
