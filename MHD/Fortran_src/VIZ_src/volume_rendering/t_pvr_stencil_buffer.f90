!t_pvr_stencil_buffer.f90
!
!      module t_pvr_stencil_buffer
!
!      Written by H. Matsui on Aug., 2011
!
!!
      module t_pvr_stencil_buffer
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_calypso_comm_table
      use t_pvr_ray_startpoints
      use t_pvr_image_stack_table
      use t_stencil_buffer_work
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_stencil_buffer                                 &
     &         (num_pixel_xy, pvr_start)
!
      use quicksort
      use m_solver_SR
      use set_to_send_buffer
      use calypso_SR_core
      use calypso_SR
      use calypso_SR_int
      use calypso_SR_type
      use const_comm_tbl_img_output
      use const_comm_tbl_img_composit
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_ray_start_type), intent(in) :: pvr_start
!
      integer(kind = kint_gl) :: num_pvr_ray_gl
!
      integer(kind = kint_gl), allocatable :: num_ray_start_lc(:)
      integer(kind = kint_gl), allocatable :: num_ray_start_gl(:)
!
      type(stencil_buffer_work)  :: stencil_wk
!
      integer(kind = kint), allocatable :: ipixel_4_composit(:)
      integer(kind = kint), allocatable :: item_4_composit(:)
!
      integer(kind = kint) :: num_pixel_recv
      type(calypso_comm_table) :: img_output_tbl
      type(calypso_comm_table) :: img_composit_tbl
!
      type(pvr_image_stack_table) :: img_stack
      integer(kind = kint), allocatable :: ipix_recv_pixel_composit(:)
      real(kind = kreal), allocatable :: depth_recv_pixel_composit(:)
!
      integer(kind = kint), allocatable :: ipixel_check(:)
!!
      integer :: num32
      integer :: ip
      integer :: irank_image_file
      integer(kind = kint) :: inum, ipix, icou, ist, num
      integer(kind = kint_gl) :: num64
!
!
      irank_image_file = pvr_start%irank_composit_ref
!
      allocate(num_ray_start_lc(num_pixel_xy))
      if(my_rank .eq. irank_image_file) then
        allocate(num_ray_start_gl(num_pixel_xy))
      end if
!
      num64 = pvr_start%num_pvr_ray
      call MPI_REDUCE(num64, num_pvr_ray_gl, 1, CALYPSO_GLOBAL_INT,     &
     &    MPI_SUM, irank_image_file, CALYPSO_COMM, ierr_MPI)
      if(my_rank .eq. irank_image_file) write(*,*)                      &
     &      'num_pvr_ray_gl', num_pvr_ray_gl, num_pixel_xy
!
      call count_local_ray_4_each_pixel(num_pixel_xy,                   &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    num_ray_start_lc)
!
      num32 = num_pixel_xy
      call MPI_REDUCE(num_ray_start_lc, num_ray_start_gl, num32,        &
     &    CALYPSO_GLOBAL_INT, MPI_SUM, irank_image_file,                &
     &    CALYPSO_COMM, ierr_MPI)
!
      call alloc_stencil_buffer_work(num_pixel_xy, stencil_wk)
      call set_global_stencil_buffer(irank_image_file,            &
     &    num_pixel_xy, num_pvr_ray_gl, num_ray_start_gl, stencil_wk)
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
      allocate(ipixel_check(num_pixel_recv))
      call calypso_SR_type_int(0, img_output_tbl,                       &
     &    img_stack%npixel_4_composit, num_pixel_recv,                  &
     &    ipixel_4_composit, ipixel_check)
!
      write(50+my_rank,*) 'ipixel_check', num_pixel_recv, num_pixel_xy
      do ipix = 1, num_pixel_recv
        write(50+my_rank,*) ipix,                                       &
     &            ipixel_check(ipix), stencil_wk%irev_recv_image(ipix)
      end do
      deallocate(ipixel_check)
!
!
!
      call s_const_comm_tbl_img_composit                                &
     &   (num_pixel_xy, stencil_wk%irank_4_composit,                    &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    img_composit_tbl)
      call dealloc_stencil_buffer_work(stencil_wk)
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

      write(50+my_rank,*) 'img_composit_tbl%nrank_export',              &
     &                    img_composit_tbl%nrank_export
      do ip = 1, img_composit_tbl%nrank_export
        ist = img_composit_tbl%istack_export(ip-1)
        num = img_composit_tbl%istack_export(ip) - ist
        write(50+my_rank,*) 'img_composit_tbl%irank_export',            &
     &        ip, img_composit_tbl%irank_export(ip), ist, num
        do inum = 1, num
          icou = img_composit_tbl%item_export(ist+inum)
          write(50+my_rank,*) inum, icou,   &
     &                pvr_start%id_pixel_start(icou), &
     &                pvr_start%xx_pvr_ray_start(icou,3)
        end do
      end do
!
      write(50+my_rank,*) 'img_composit_tbl%ntot_import',               &
     &       img_composit_tbl%ntot_import, img_stack%npixel_4_composit
      do ipix = 1, img_stack%npixel_4_composit
        ist = img_stack%istack_composition(ipix-1)
        num = img_stack%istack_composition(ipix) - ist
        write(50+my_rank,*) 'idx_recv_pixel_composit', ist, num
        do inum = 1, num
          icou = img_stack%idx_recv_pixel_composit(ist+inum)
          write(50+my_rank,*) inum, ipix, icou,   &
     &                ipix_recv_pixel_composit(icou), &
     &                depth_recv_pixel_composit(icou)
        end do
      end do
      close(50+my_rank)
!
      return
!
      end subroutine set_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_stencil_buffer
