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
      use t_pvr_ray_startpoints
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
      integer(kind = kint_gl), allocatable :: istack_ray_start_gl(:)
      type(stencil_buffer_work)  :: stencil_wk
!
      integer(kind = kint) :: npixel_4_composit
      integer(kind = kint), allocatable :: ipixel_4_composit(:)
      integer(kind = kint), allocatable :: item_4_composit(:)
      integer(kind = kint), allocatable :: istack_composition(:)
!
      integer(kind = kint) :: num_pixel_recv
      integer(kind = kint) :: ncomm_send_pixel_output
      integer(kind = kint) :: ncomm_recv_pixel_output
      integer(kind = kint) :: iself_pixel_output
      integer(kind = kint) :: ntot_send_pixel_output
      integer(kind = kint) :: ntot_recv_pixel_output
      integer(kind = kint), allocatable :: irank_send_pixel_output(:)
      integer(kind = kint), allocatable :: istack_send_pixel_output(:)
      integer(kind = kint), allocatable :: item_send_pixel_output(:)
      integer(kind = kint), allocatable :: irank_recv_pixel_output(:)
      integer(kind = kint), allocatable :: istack_recv_pixel_output(:)
      integer(kind = kint), allocatable :: item_recv_pixel_output(:)
      integer(kind = kint), allocatable :: irev_recv_pixel_output(:)
!
      integer(kind = kint), allocatable :: num_send_pixel_tmp(:)
      integer(kind = kint), allocatable :: num_recv_pixel_tmp(:)
!
      integer(kind = kint) :: ncomm_send_pixel_composit
      integer(kind = kint) :: ncomm_recv_pixel_composit
      integer(kind = kint) :: iself_pixel_composit
      integer(kind = kint) :: ntot_send_pixel_composit
      integer(kind = kint) :: ntot_recv_pixel_composit
      integer(kind = kint), allocatable :: irank_send_pixel_composit(:)
      integer(kind = kint), allocatable :: istack_send_pixel_composit(:)
      integer(kind = kint), allocatable :: item_send_pixel_composit(:)
      integer(kind = kint), allocatable :: irank_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: istack_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: item_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: irev_recv_pixel_composit(:)
!
      integer(kind = kint), allocatable :: itmp_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: ipix_send_pixel_composit(:)
      integer(kind = kint), allocatable :: idx_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: ipix_recv_pixel_composit(:)
      real(kind = kreal), allocatable :: depth_recv_pixel_composit(:)
      real(kind = kreal), allocatable :: rwork_recv_pixel_composit(:)
!
      integer(kind = kint), allocatable :: index_pvr_start(:)
      integer(kind = kint), allocatable :: ipixel_check(:)
!!
      integer :: num32
      integer :: ip, jp, i_rank, id_rank
      integer :: irank_image_file
      integer(kind = kint) :: inum, ipix, icou, ist, ied, isrt, jst, num
      integer(kind = kint) :: icou1, icou2
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
      ncomm_send_pixel_output = 0
      if(npixel_4_composit .gt. 0) ncomm_send_pixel_output = 1
!
      allocate(irank_send_pixel_output(ncomm_send_pixel_output))
      allocate(istack_send_pixel_output(0:ncomm_send_pixel_output))

      call count_export_item_pvr_output                           &
     &         (irank_image_file, npixel_4_composit,                    &
     &          ncomm_send_pixel_output, ntot_send_pixel_output,        &
     &          irank_send_pixel_output, istack_send_pixel_output)
!
      allocate(item_send_pixel_output(ntot_send_pixel_output))
!
      call set_export_item_pvr_output                             &
     &         (ntot_send_pixel_output, item_send_pixel_output)
!
!
      call count_import_pe_pvr_output                             &
     &         (irank_image_file, stencil_wk%istack_recv_image,         &
     &          ncomm_recv_pixel_output)
      allocate(irank_recv_pixel_output(ncomm_recv_pixel_output))
      allocate(istack_recv_pixel_output(0:ncomm_recv_pixel_output))
!
      call count_import_item_pvr_output                           &
     &         (irank_image_file, stencil_wk%istack_recv_image,         &
     &    num_pixel_xy, stencil_wk%irank_4_composit, stencil_wk%item_recv_image,        &
     &          ncomm_recv_pixel_output, ntot_recv_pixel_output,        &
     &          irank_recv_pixel_output, istack_recv_pixel_output,      &
     &          iself_pixel_output, num_pixel_recv)
!
      allocate(item_recv_pixel_output(ntot_recv_pixel_output))
      allocate(irev_recv_pixel_output(num_pixel_recv))
!
      call set_import_item_pvr_output                             &
     &         (num_pixel_xy, stencil_wk%item_recv_image,                          &
     &          ntot_recv_pixel_output, num_pixel_recv,                 &
     &          item_recv_pixel_output, irev_recv_pixel_output)
!
      allocate(ipixel_4_composit(npixel_4_composit))
      allocate(item_4_composit(num_pixel_xy))
!
!$omp parallel workshare
      item_4_composit(1:num_pixel_xy) = 0
!$omp end parallel workshare
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
      allocate(ipixel_check(num_pixel_recv))
      call calypso_send_recv_int       &
     &    (0, npixel_4_composit, num_pixel_recv,    &
     &     ncomm_send_pixel_output, iself_pixel_output,        &
     &     irank_send_pixel_output, istack_send_pixel_output,      &
     &     item_send_pixel_output,      &
     &     ncomm_recv_pixel_output, iself_pixel_output,       &
     &     irank_recv_pixel_output, istack_recv_pixel_output,    &
     &     item_recv_pixel_output, irev_recv_pixel_output,      &
     &     ipixel_4_composit, ipixel_check)
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
      allocate(index_pvr_start(pvr_start%num_pvr_ray))
      call sort_index_pvr_start                                         &
     &   (pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    index_pvr_start)
!
      allocate(num_send_pixel_tmp(nprocs))
      allocate(num_recv_pixel_tmp(nprocs))
!
      call count_num_send_pixel_tmp                                     &
     &   (num_pixel_xy, stencil_wk%irank_4_composit, pvr_start%num_pvr_ray,        &
     &    pvr_start%id_pixel_start, index_pvr_start, num_send_pixel_tmp)
!
      call MPI_Alltoall(num_send_pixel_tmp, 1, CALYPSO_INTEGER,         &
     &                  num_recv_pixel_tmp, 1, CALYPSO_INTEGER,         &
     &                  CALYPSO_COMM, ierr_MPI)
!
!
      call count_comm_pe_pvr_composition                          &
     &         (num_send_pixel_tmp, num_recv_pixel_tmp,                 &
     &          ncomm_send_pixel_composit, ncomm_recv_pixel_composit)
!
      allocate(irank_send_pixel_composit(ncomm_send_pixel_composit))
      allocate(istack_send_pixel_composit(0:ncomm_send_pixel_composit))
      allocate(irank_recv_pixel_composit(ncomm_recv_pixel_composit))
      allocate(istack_recv_pixel_composit(0:ncomm_recv_pixel_composit))
!
      call count_comm_tbl_pvr_composition                         &
     &         (num_send_pixel_tmp, num_recv_pixel_tmp,                 &
     &          ncomm_send_pixel_composit, ncomm_recv_pixel_composit,   &
     &          ntot_send_pixel_composit, irank_send_pixel_composit,    &
     &          istack_send_pixel_composit, ntot_recv_pixel_composit,   &
     &          irank_recv_pixel_composit, istack_recv_pixel_composit,  &
     &          iself_pixel_composit)
!
!
      allocate(item_send_pixel_composit(ntot_send_pixel_composit))
      allocate(item_recv_pixel_composit(ntot_recv_pixel_composit))
      allocate(irev_recv_pixel_composit(ntot_recv_pixel_composit))
!
      call set_comm_tbl_pvr_composition                           &
     &         (pvr_start%num_pvr_ray, pvr_start%id_pixel_start, index_pvr_start,           &
     &          num_pixel_xy, stencil_wk%irank_4_composit,              &
     &          ncomm_send_pixel_composit, ntot_send_pixel_composit,    &
     &          irank_send_pixel_composit, istack_send_pixel_composit,  &
     &          item_send_pixel_composit, ntot_recv_pixel_composit,     &
     &          item_recv_pixel_composit, irev_recv_pixel_composit)
      call dealloc_stencil_buffer_work(stencil_wk)
!
      allocate(idx_recv_pixel_composit(ntot_recv_pixel_composit))
      allocate(ipix_recv_pixel_composit(ntot_recv_pixel_composit))
      allocate(depth_recv_pixel_composit(ntot_recv_pixel_composit))
!
      allocate(rwork_recv_pixel_composit(ntot_recv_pixel_composit))
!
      allocate(ipix_send_pixel_composit(ntot_send_pixel_composit))
      do inum = 1, ntot_send_pixel_composit
        isrt = item_send_pixel_composit(inum)
        ipix_send_pixel_composit(inum) = pvr_start%id_pixel_start(isrt)
      end do
!
      call calypso_send_recv_int       &
     &    (0, pvr_start%num_pvr_ray, ntot_recv_pixel_composit,    &
     &     ncomm_send_pixel_composit, iself_pixel_composit,        &
     &     irank_send_pixel_composit, istack_send_pixel_composit,      &
     &     item_send_pixel_composit,      &
     &     ncomm_recv_pixel_composit, iself_pixel_composit,       &
     &     irank_recv_pixel_composit, istack_recv_pixel_composit,    &
     &     item_recv_pixel_composit, irev_recv_pixel_composit,      &
     &     pvr_start%id_pixel_start, ipix_recv_pixel_composit)
!
      call calypso_send_recv       &
     &    (0, pvr_start%num_pvr_ray, ntot_recv_pixel_composit,    &
     &     ncomm_send_pixel_composit, iself_pixel_composit,        &
     &     irank_send_pixel_composit, istack_send_pixel_composit,      &
     &     item_send_pixel_composit,      &
     &     ncomm_recv_pixel_composit, iself_pixel_composit,       &
     &     irank_recv_pixel_composit, istack_recv_pixel_composit,    &
     &     item_recv_pixel_composit, irev_recv_pixel_composit,      &
     &     pvr_start%xx_pvr_ray_start(1,3), depth_recv_pixel_composit)
!
      allocate(itmp_recv_pixel_composit(ntot_recv_pixel_composit))
!$omp parallel do
      do inum = 1, ntot_recv_pixel_composit
        ipix = ipix_recv_pixel_composit(inum)
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
        rwork_recv_pixel_composit(inum) = depth_recv_pixel_composit(icou)
      end do
!
      allocate(istack_composition(0:npixel_4_composit))
      istack_composition(0:npixel_4_composit) = 0
      do inum = 1, ntot_recv_pixel_composit
        ipix = itmp_recv_pixel_composit(inum)
        istack_composition(ipix) = istack_composition(ipix) +1
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

      write(50+my_rank,*) 'ncomm_send_pixel_composit',  &
     &                    ncomm_send_pixel_composit
      do ip = 1, ncomm_send_pixel_composit
        ist = istack_send_pixel_composit(ip-1)
        num = istack_send_pixel_composit(ip) - ist
        write(50+my_rank,*) 'irank_send_pixel_composit',  &
     &        ip, irank_send_pixel_composit(ip), ist, num
        do inum = 1, num
          icou = item_send_pixel_composit(ist+inum)
          write(50+my_rank,*) inum, icou,   &
     &                pvr_start%id_pixel_start(icou), &
     &                pvr_start%xx_pvr_ray_start(icou,3)
        end do
      end do
!
      write(50+my_rank,*) 'ntot_recv_pixel_composit',  &
     &       istack_recv_pixel_composit(ncomm_recv_pixel_composit),   &
     &       npixel_4_composit
      do ipix = 1, npixel_4_composit
        ist = istack_composition(ipix-1)
        num = istack_composition(ipix) - ist
        write(50+my_rank,*) 'idx_recv_pixel_composit', ist, num
        do inum = 1, num
          icou = idx_recv_pixel_composit(ist+inum)
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
