!>@file   t_pvr_stencil_buffer.f90
!!@brief  module t_pvr_stencil_buffer
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Work structure to make stencil buffer
!!
!!@verbatim
!!      subroutine const_pvr_stencil_buffer                             &
!!     &         (irank_image_file, npe_img_composit,                   &
!!     &          num_pixel_xy, pvr_start, pvr_stencil)
!!      subroutine collect_rendering_image(pvr_start, pvr_stencil)
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
     &         (irank_image_file, npe_img_composit,                     &
     &          num_pixel_xy, pvr_start, pvr_stencil)
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: npe_img_composit
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_ray_start_type), intent(in) :: pvr_start
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!
      type(stencil_buffer_work) :: stencil_wk
!
!
!      write(*,*) 'const_stencil_buffer_work'
      call const_stencil_buffer_work                                    &
     &   (irank_image_file, npe_img_composit, num_pixel_xy, pvr_start,  &
     &    stencil_wk)
!
!      write(*,*) 'set_pvr_stencil_buffer'
      call set_pvr_stencil_buffer                                       &
     &   (irank_image_file, num_pixel_xy, pvr_start, stencil_wk,        &
     &    pvr_stencil%num_pixel_recv, pvr_stencil%img_output_tbl,       &
     &    pvr_stencil%img_composit_tbl, pvr_stencil%img_stack)
      call dealloc_stencil_buffer_work(stencil_wk)
!
!      write(*,*) 'alloc_pvr_stencil_buffer'
      call alloc_pvr_stencil_buffer(pvr_stencil)
!
      end subroutine const_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine collect_rendering_image(pvr_start,                     &
     &          num_pixel_actual, rgba_real_gl, pvr_stencil)
!
      use calypso_SR_type
!
      type(pvr_ray_start_type), intent(in) :: pvr_start
      integer(kind = kint), intent(in) :: num_pixel_actual
!
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
      real(kind = kreal), intent(inout)                                 &
     &                    :: rgba_real_gl(4,num_pixel_actual)
!
!
      call reset_pvr_stencil_buffer(pvr_stencil)
      call calypso_SR_type_N(0, ifour, pvr_stencil%img_composit_tbl,    &
     &    pvr_start%num_pvr_ray, pvr_stencil%npixel_recved,             &
     &    pvr_start%rgba_ray(1,1), pvr_stencil%rgba_subdomain(1,1))
!
      call composit_rendering_image(pvr_stencil%img_stack,              &
     &    pvr_stencil%npixel_recved, pvr_stencil%rgba_subdomain,        &
     &    pvr_stencil%npixel_stacked, pvr_stencil%rgba_composit)
!
!
      if(num_pixel_actual .gt. 0) then
!$omp parallel workshare
        rgba_real_gl(1:4,1:num_pixel_actual) = 0.0d0
!$omp end parallel workshare
      end if
!
      call calypso_SR_type_N(0, ifour, pvr_stencil%img_output_tbl,      &
     &    pvr_stencil%npixel_stacked, num_pixel_actual,                 &
     &    pvr_stencil%rgba_composit(1,1), rgba_real_gl(1,1))
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
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_ray_start_type), intent(in) :: pvr_start
      type(stencil_buffer_work), intent(in)  :: stencil_wk
!
      integer(kind = kint), intent(inout) :: num_pixel_recv
      type(calypso_comm_table), intent(inout) :: img_output_tbl
      type(calypso_comm_table), intent(inout) :: img_composit_tbl
      type(pvr_image_stack_table), intent(inout) :: img_stack
!
      character(len=kchara) :: fname_tmp, file_name
!
!
!      write(*,*) 'count_parallel_stencil_buffer'
      call count_parallel_stencil_buffer                                &
     &   (stencil_wk, img_stack%npixel_4_composit)
!
!
!      write(*,*) 's_const_comm_tbl_img_output'
      call s_const_comm_tbl_img_output                                  &
     &   (stencil_wk, irank_image_file, num_pixel_xy,                   &
     &    img_stack%npixel_4_composit, num_pixel_recv, img_output_tbl)
!
!
!      write(*,*) 'set_global_pixel_4_composit'
      call alloc_pvr_ipixel_4_composit(num_pixel_xy, img_stack)
      call set_global_pixel_4_composit                                  &
     &   (stencil_wk, img_stack%npixel_4_composit, num_pixel_xy,        &
     &    img_stack%ipixel_4_composit, img_stack%item_4_composit)
!
!
!
!      write(*,*) 's_const_comm_tbl_img_composit'
      call s_const_comm_tbl_img_composit                                &
     &   (num_pixel_xy, stencil_wk%irank_4_composit,                    &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    img_composit_tbl)
!
      call alloc_depth_pixel_composit(pvr_start%num_pvr_ray,            &
     &    img_composit_tbl%ntot_import, img_stack)
!
!$omp parallel workshare
      img_stack%depth_pvr_ray_start(1:pvr_start%num_pvr_ray)            &
     &      = - pvr_start%xx_pvr_ray_start(3,1:pvr_start%num_pvr_ray)
!$omp end parallel workshare
!
      call calypso_SR_type_int(0, img_composit_tbl,                     &
     &    pvr_start%num_pvr_ray, img_composit_tbl%ntot_import,          &
     &    pvr_start%id_pixel_start, img_stack%ipix_4_composit)
!
      call calypso_SR_type_1(0, img_composit_tbl,                       &
     &    pvr_start%num_pvr_ray, img_composit_tbl%ntot_import,          &
     &    img_stack%depth_pvr_ray_start, img_stack%depth_pixel_composit)
!
      call alloc_pvr_image_stack_table                                  &
     &   (img_composit_tbl%ntot_import, img_stack)
      call set_image_stacking_list                                      &
     &   (num_pixel_xy, img_stack%item_4_composit,                      &
     &    img_composit_tbl%ntot_import, img_stack%npixel_4_composit,    &
     &    img_stack%ipix_4_composit, img_stack%depth_pixel_composit,    &
     &    img_stack%istack_composition,                                 &
     &    img_stack%idx_recv_pixel_composit)
!
!
      if(i_debug .gt. 0) then
        fname_tmp = add_int_suffix(my_rank, check_fhead)
        file_name = add_dat_extension(fname_tmp)
        open(id_file, file = file_name)
        call check_img_output_communication(id_file,                    &
     &      img_stack, img_output_tbl, num_pixel_xy, num_pixel_recv,    &
     &      stencil_wk%irev_recv_image, img_stack%ipixel_4_composit)
!
        call check_composit_communication(id_file,                      &
     &      pvr_start, img_composit_tbl, img_stack,                     &
     &      img_stack%ipix_4_composit, img_stack%depth_pixel_composit)
        close(id_file)
      end if
!
      call dealloc_pvr_ipixel_4_composit(img_stack)
      call dealloc_depth_pixel_composit(img_stack)
!
      end subroutine set_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_composit_communication                           &
     &         (id_file, pvr_start, img_composit_tbl, img_stack,        &
     &          ipix_4_composit, depth_pixel_composit)
!
      integer(kind = kint), intent(in) :: id_file
      type(pvr_ray_start_type), intent(in) :: pvr_start
      type(calypso_comm_table), intent(in) :: img_composit_tbl
      type(pvr_image_stack_table), intent(in) :: img_stack
      integer(kind = kint), intent(in)                                  &
     &      :: ipix_4_composit(img_composit_tbl%ntot_import)
      real(kind = kreal), intent(in)                                    &
     &      :: depth_pixel_composit(img_composit_tbl%ntot_import)
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
     &                pvr_start%xx_pvr_ray_start(3,icou)
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
     &        ipix_4_composit(icou), depth_pixel_composit(icou)
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
