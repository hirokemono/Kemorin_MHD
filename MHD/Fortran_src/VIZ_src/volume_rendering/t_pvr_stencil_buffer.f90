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
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_ray_start_type), intent(in) :: pvr_start
!
      integer(kind = kint_gl) :: num_pvr_ray_gl
      integer(kind = kint) :: ntot_recv_image
!
      integer(kind = kint_gl), allocatable :: num_ray_start_lc(:)
      integer(kind = kint_gl), allocatable :: num_ray_start_gl(:)
!
      integer(kind = kint), allocatable :: istack_recv_image(:)
      integer(kind = kint), allocatable :: irank_4_composit(:)
      integer(kind = kint), allocatable :: item_recv_image(:)
      integer(kind = kint), allocatable :: irev_recv_image(:)
      integer(kind = kint), allocatable :: irank_image_stack(:)
      integer(kind = kint_gl), allocatable :: istack_ray_start_gl(:)
!
      integer(kind = kint) :: npixel_4_composit
      integer(kind = kint), allocatable :: ipixel_4_composit(:)
      integer(kind = kint), allocatable :: item_4_composit(:)
      integer(kind = kint), allocatable :: istack_composition(:)
!
      integer(kind = kint), allocatable :: num_send_pixel_composit(:)
      real(kind = kreal), allocatable :: depth_send_pixel_composit(:)
!
      integer(kind = kint), allocatable :: num_send_pixel_tmp(:)
      integer(kind = kint), allocatable :: num_recv_pixel_tmp(:)
      integer(kind = kint), allocatable :: id_send_pixel_tmp(:)
!
      integer(kind = kint) :: ncomm_send_pixel_composit
      integer(kind = kint) :: ncomm_recv_pixel_composit
      integer(kind = kint) :: iself_send_pixel_composit
      integer(kind = kint) :: iself_recv_pixel_composit
      integer(kind = kint) :: ntot_send_pixel_composit
      integer(kind = kint) :: ntot_recv_pixel_composit
      integer(kind = kint), allocatable :: irank_send_pixel_composit(:)
      integer(kind = kint), allocatable :: istack_send_pixel_composit(:)
      integer(kind = kint), allocatable :: item_send_pixel_composit(:)
      integer(kind = kint), allocatable :: irank_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: istack_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: item_recv_pixel_composit(:)
!
      integer(kind = kint), allocatable :: ipix_send_pixel_composit(:)
      integer(kind = kint), allocatable :: idx_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: ipix_recv_pixel_composit(:)
      integer(kind = kint), allocatable :: iwork_recv_pixel_composit(:)
      real(kind = kreal), allocatable :: depth_recv_pixel_composit(:)
      real(kind = kreal), allocatable :: rwork_recv_pixel_composit(:)
!
      integer(kind = kint), allocatable :: index(:)
      integer(kind = kint), allocatable :: iref(:)
!
      integer :: num32
      integer :: ip, jp, i_rank, id_rank
      integer :: image_out
      integer(kind = kint) :: inum, ipix, icou, ist, ied, isrt, jst, num
      integer(kind = kint) :: icou1, icou2
      integer(kind = kint_gl) :: num64
!
!
      image_out = pvr_start%irank_composit_ref
!
      allocate(num_ray_start_lc(num_pixel_xy))
      if(my_rank .eq. image_out) then
        allocate(num_ray_start_gl(num_pixel_xy))
      end if
!
      num64 = pvr_start%num_pvr_ray
      call MPI_REDUCE(num64, num_pvr_ray_gl, 1, CALYPSO_GLOBAL_INT,     &
     &    MPI_SUM, image_out, CALYPSO_COMM, ierr_MPI)
      if(my_rank .eq. image_out) write(*,*)                             &
     &      'num_pvr_ray_gl', num_pvr_ray_gl, num_pixel_xy
!
!$omp parallel workshare
      num_ray_start_lc(1:num_pixel_xy) = 0
!$omp end parallel workshare
      do inum = 1, pvr_start%num_pvr_ray
        ipix = pvr_start%id_pixel_start(inum)
        num_ray_start_lc(ipix) = num_ray_start_lc(ipix) + 1
      end do
!
      num32 = num_pixel_xy
      call MPI_REDUCE(num_ray_start_lc, num_ray_start_gl, num32,        &
     &    CALYPSO_GLOBAL_INT, MPI_SUM, image_out,                       &
     &    CALYPSO_COMM, ierr_MPI)
!
      allocate(istack_recv_image(0:nprocs))
!
      allocate(irank_4_composit(num_pixel_xy))
      allocate(irev_recv_image(num_pixel_xy))
      allocate(item_recv_image(num_pixel_xy))
      if(my_rank .eq. image_out) then
        allocate(irank_image_stack(0:num_pixel_xy))
        allocate(istack_ray_start_gl(num_pixel_xy))
!
!$omp parallel workshare
        irank_4_composit(1:num_pixel_xy) = -1
        irank_image_stack(1:num_pixel_xy) = -1
!$omp end parallel workshare

        istack_ray_start_gl(0) = 0
        icou = 0
        do ipix = 1, num_pixel_xy
          istack_ray_start_gl(ipix) = istack_ray_start_gl(ipix-1)       &
     &                               + num_ray_start_gl(ipix)
!
          if(num_ray_start_gl(ipix) .gt. 0) then
            icou = icou + 1
            ip = int((istack_ray_start_gl(ipix) - 1)                    &
     &              * nprocs / num_pvr_ray_gl + 1)
            i_rank = int(mod(image_out+ip,nprocs))
            irank_4_composit(ipix) = i_rank
            istack_recv_image(ip) = icou
            irev_recv_image(ipix) = icou
            item_recv_image(icou) = ipix
          end if
        end do
        ntot_recv_image = istack_recv_image(nprocs)
        write(*,*) 'item_recv_image(1)', item_recv_image(1:10)
!
        do ip = 1, nprocs
          ist = istack_recv_image(ip-1) + 1
          ied = istack_recv_image(ip)
          do inum = ist, ied
            irev_recv_image(inum) = irev_recv_image(inum)               &
     &                             - istack_recv_image(ip-1)
          end do
        end do
!
!        write(50+my_rank,*) 'ipix, irank_4_composit'
!        do ipix = 1, num_pixel_xy
!          write(50+my_rank,*) ipix, irank_4_composit(ipix)
!        end do
      end if
!
      call mpi_Bcast(istack_recv_image, (nprocs+1),                     &
     &    CALYPSO_INTEGER, image_out, CALYPSO_COMM, ierr_MPI)
!
      num32 = num_pixel_xy
      call mpi_Bcast(irank_4_composit, num32,                           &
     &    CALYPSO_INTEGER, image_out, CALYPSO_COMM, ierr_MPI)
      call mpi_Bcast(irev_recv_image, num32,                            &
     &    CALYPSO_INTEGER, image_out, CALYPSO_COMM, ierr_MPI)
!
      call mpi_Bcast(ntot_recv_image, 1,                                &
     &    CALYPSO_INTEGER, image_out, CALYPSO_COMM, ierr_MPI)
      num32 = ntot_recv_image
      call mpi_Bcast(item_recv_image(1), num32,                         &
     &    CALYPSO_INTEGER, image_out, CALYPSO_COMM, ierr_MPI)
!
      npixel_4_composit = 0
      do ip = 1, nprocs
        ist = istack_recv_image(ip-1)
        ipix = item_recv_image(ist+1)
!        write(*,*) 'irank_4_composit(ipix)', irank_4_composit(ipix)
        if(irank_4_composit(ipix) .eq. my_rank) then
          npixel_4_composit = istack_recv_image(ip)                     &
     &                       - istack_recv_image(ip-1)
          exit
        end if
      end do
!      write(*,*) 'istack_recv_image', istack_recv_image
!      write(*,*) 'npixel_4_composit', npixel_4_composit
!
      allocate(ipixel_4_composit(npixel_4_composit))
      allocate(item_4_composit(num_pixel_xy))
!
!$omp parallel workshare
      item_4_composit(1:num_pixel_xy) = 0
!$omp end parallel workshare
      do ip = 1, nprocs
        ist = istack_recv_image(ip-1)
        ipix = item_recv_image(ist+1)
        if(irank_4_composit(ipix) .eq. my_rank) then
          do inum = 1, npixel_4_composit
            ipix = item_recv_image(ist+inum)
            ipixel_4_composit(inum) = ipix
            item_4_composit(ipix) = inum
          end do
          exit
        end if
      end do
!



      allocate(index(pvr_start%num_pvr_ray))
      allocate(iref(pvr_start%num_pvr_ray))
!
      do inum = 1,  pvr_start%num_pvr_ray
        index(inum) = inum
        iref(inum) = pvr_start%id_pixel_start(inum)
      end do
!
      call quicksort_w_index(pvr_start%num_pvr_ray, iref,               &
     &    ione, pvr_start%num_pvr_ray, index)
!
      allocate(num_send_pixel_tmp(nprocs))
      allocate(num_recv_pixel_tmp(nprocs))
      allocate(id_send_pixel_tmp(nprocs))
!
!$omp parallel do
      do ip = 1, nprocs
        id_send_pixel_tmp(ip) = ip - 1
      end do
!$omp end parallel do
!$omp parallel workshare
      num_send_pixel_tmp(1:nprocs) = 0
!$omp end parallel workshare
      do inum = 1, pvr_start%num_pvr_ray
        isrt = index(inum)
        ipix =  pvr_start%id_pixel_start(isrt)
        ip = irank_4_composit(ipix) + 1
        num_send_pixel_tmp(ip) = num_send_pixel_tmp(ip) + 1
      end do
!
      call MPI_Alltoall(num_send_pixel_tmp, 1, CALYPSO_INTEGER,         &
     &                  num_recv_pixel_tmp, 1, CALYPSO_INTEGER,         &
     &                  CALYPSO_COMM, ierr_MPI)
!
!
      icou1 = 0
      icou2 = 0
      do ip = 1, nprocs
        if(num_send_pixel_tmp(ip) .gt. 0) icou1 = icou1 + 1
        if(num_recv_pixel_tmp(ip) .gt. 0) icou2 = icou2 + 1
      end do
      ncomm_send_pixel_composit = icou1
      ncomm_recv_pixel_composit = icou2
      write(*,*) my_rank, 'num_send_pixel_tmp', num_send_pixel_tmp
      write(*,*) my_rank, 'num_recv_pixel_tmp', num_recv_pixel_tmp
      write(*,*) my_rank, 'ncomm_send_pixel_composit', ncomm_send_pixel_composit
      write(*,*) my_rank, 'ncomm_recv_pixel_composit', ncomm_recv_pixel_composit
!
      allocate(irank_send_pixel_composit(ncomm_send_pixel_composit))
      allocate(istack_send_pixel_composit(0:ncomm_send_pixel_composit))
      allocate(irank_recv_pixel_composit(ncomm_recv_pixel_composit))
      allocate(istack_recv_pixel_composit(0:ncomm_recv_pixel_composit))
!
      icou1 = 0
      icou2 = 0
      iself_send_pixel_composit = 0
      iself_recv_pixel_composit = 0
      istack_send_pixel_composit(icou1) = 0
      istack_recv_pixel_composit(icou2) = 0
      do ip = 1, nprocs
        i_rank = mod(my_rank+ip,nprocs)
        if(num_send_pixel_tmp(i_rank+1) .gt. 0) then
          icou1 = icou1 + 1
          irank_send_pixel_composit(icou1) = i_rank
          istack_send_pixel_composit(icou1)                             &
     &          = istack_send_pixel_composit(icou1-1)                   &
     &           + num_send_pixel_tmp(i_rank+1)
          if(i_rank .eq. my_rank) iself_send_pixel_composit = 1
        end if
        if(num_recv_pixel_tmp(i_rank+1) .gt. 0) then
          icou2 = icou2 + 1
          irank_recv_pixel_composit(icou2) = i_rank
          istack_recv_pixel_composit(icou2)                             &
     &          = istack_recv_pixel_composit(icou2-1)                   &
     &           + num_recv_pixel_tmp(i_rank+1)
          if(i_rank .eq. my_rank) iself_recv_pixel_composit = 1
        end if
      end do
!
      ntot_send_pixel_composit                                          &
     &       = istack_send_pixel_composit(ncomm_send_pixel_composit)
      ntot_recv_pixel_composit                                          &
     &       = istack_recv_pixel_composit(ncomm_recv_pixel_composit)
!
!      write(50+my_rank,*) 'irank_send_pixel_composit',  &
!     & ncomm_send_pixel_composit
!      do ip = 1, ncomm_send_pixel_composit
!        write(50+my_rank,*) ip, irank_send_pixel_composit(ip),   &
!     & istack_send_pixel_composit(ip) - istack_send_pixel_composit(ip-1)
!      end do
!      write(50+my_rank,*) 'irank_recv_pixel_composit', &
!     &    ncomm_recv_pixel_composit
!      do ip = 1, ncomm_recv_pixel_composit
!        write(50+my_rank,*) ip, irank_recv_pixel_composit(ip),   &
!     & istack_recv_pixel_composit(ip) - istack_recv_pixel_composit(ip-1)
!      end do
!      close(50+my_rank)
!
!
      allocate(item_send_pixel_composit(ntot_send_pixel_composit))
      allocate(ipix_send_pixel_composit(ntot_send_pixel_composit))
!
      allocate(idx_recv_pixel_composit(ntot_recv_pixel_composit))
      allocate(item_recv_pixel_composit(ntot_recv_pixel_composit))
      allocate(ipix_recv_pixel_composit(ntot_recv_pixel_composit))
      allocate(depth_recv_pixel_composit(ntot_recv_pixel_composit))
!
      allocate(iwork_recv_pixel_composit(ntot_recv_pixel_composit))
      allocate(rwork_recv_pixel_composit(ntot_recv_pixel_composit))
!
      icou = 0
      do 
        isrt = index(icou+1)
        ipix =  pvr_start%id_pixel_start(isrt)
        i_rank = irank_4_composit(ipix)
        do ip = 1, ncomm_send_pixel_composit
          if(irank_send_pixel_composit(ip) .eq. i_rank) then
            jst = istack_send_pixel_composit(ip-1)
            num = istack_send_pixel_composit(ip) - jst
            do inum = 1, num
              icou = icou + 1
              isrt = index(icou)
              item_send_pixel_composit(inum+jst) = isrt
              ipix_send_pixel_composit(inum+jst)                        &
     &                       = pvr_start%id_pixel_start(isrt)
            end do
            exit
          end if
        end do
        if(icou .ge. pvr_start%num_pvr_ray) then
          write(*,*) 'cout', my_rank, pvr_start%num_pvr_ray, &
     &        ntot_send_pixel_composit
          exit
        end if
      end do
!
      call resize_iwork_4_SR           &
     &   (ncomm_send_pixel_composit, ncomm_recv_pixel_composit,         &
     &    istack_send_pixel_composit(ncomm_send_pixel_composit),        &
     &    istack_recv_pixel_composit(ncomm_recv_pixel_composit))
      call set_to_send_buf_int(pvr_start%num_pvr_ray,               &
     &    istack_send_pixel_composit(ncomm_send_pixel_composit),    &
     &    item_send_pixel_composit, pvr_start%id_pixel_start, iWS)
      call calypso_send_recv_intcore                                    &
     &   (ncomm_send_pixel_composit, iself_send_pixel_composit,         &
     &    irank_send_pixel_composit, istack_send_pixel_composit,        &
     &    ncomm_recv_pixel_composit, iself_recv_pixel_composit,         &
     &    irank_recv_pixel_composit, istack_recv_pixel_composit)
!
      if(iself_send_pixel_composit .gt. 0) then
        ist = istack_send_pixel_composit(ncomm_send_pixel_composit-1)
        jst = istack_recv_pixel_composit(ncomm_recv_pixel_composit-1)
        num = istack_send_pixel_composit(ncomm_send_pixel_composit)     &
     &       - ist
        iWR(jst+1:jst+num) = iWS(ist+1:ist+num)
      end if
!
      call calypso_send_recv_fin                                     &
     &    (ncomm_send_pixel_composit, iself_send_pixel_composit)
!
!$omp parallel workshare
      ipix_recv_pixel_composit(1:ntot_recv_pixel_composit)  &
     &    = iWR(1:ntot_recv_pixel_composit)
!$omp end parallel workshare
!
      call resize_work_4_SR           &
     &   (ione, ncomm_send_pixel_composit, ncomm_recv_pixel_composit,   &
     &    istack_send_pixel_composit(ncomm_send_pixel_composit),        &
     &    istack_recv_pixel_composit(ncomm_recv_pixel_composit))
      call set_to_send_buf_1(pvr_start%num_pvr_ray,                 &
     &    istack_send_pixel_composit(ncomm_send_pixel_composit),    &
     &    item_send_pixel_composit, pvr_start%xx_pvr_ray_start(1,3), WS)
      call calypso_send_recv_core                                    &
     &   (ione, ncomm_send_pixel_composit, iself_send_pixel_composit,   &
     &    irank_send_pixel_composit, istack_send_pixel_composit,        &
     &    ncomm_recv_pixel_composit, iself_recv_pixel_composit,         &
     &    irank_recv_pixel_composit, istack_recv_pixel_composit)
!
      if(iself_send_pixel_composit .gt. 0) then
        ist = istack_send_pixel_composit(ncomm_send_pixel_composit-1)
        jst = istack_recv_pixel_composit(ncomm_recv_pixel_composit-1)
        num = istack_send_pixel_composit(ncomm_send_pixel_composit)     &
     &       - ist
        WR(jst+1:jst+num) = WS(ist+1:ist+num)
      end if
!
      call calypso_send_recv_fin                                     &
     &    (ncomm_send_pixel_composit, iself_send_pixel_composit)
!
!$omp parallel workshare
      depth_recv_pixel_composit(1:ntot_recv_pixel_composit)  &
     &    = WR(1:ntot_recv_pixel_composit)
!$omp end parallel workshare
!
!$omp parallel do
      do inum = 1, ntot_recv_pixel_composit
        ipix = ipix_recv_pixel_composit(inum)
        item_recv_pixel_composit(inum) = item_4_composit(ipix)
        idx_recv_pixel_composit(inum) = inum
        iwork_recv_pixel_composit(inum) = item_recv_pixel_composit(inum)
      end do
!$omp end parallel do
!
      call quicksort_w_index                                            &
     &   (ntot_recv_pixel_composit, iwork_recv_pixel_composit,          &
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
        ipix = iwork_recv_pixel_composit(inum)
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
!      write(50+my_rank,*) 'ncomm_recv_pixel_composit',  &
!     &                    ncomm_recv_pixel_composit
!      do ip = 1, ncomm_recv_pixel_composit
!        ist = istack_recv_pixel_composit(ip-1)
!        num = istack_recv_pixel_composit(ip) - ist
!        write(50+my_rank,*) 'irank_recv_pixel_composit',  &
!     &        ip, irank_recv_pixel_composit(ip), ist, num
!        do inum = 1, num
!          icou = item_recv_pixel_composit(ist+inum)
!          write(50+my_rank,*) inum, icou,   &
!     &                ipix_recv_pixel_composit(ist+inum), &
!     &                ipixel_4_composit(icou),   &
!     &                depth_recv_pixel_composit(ist+inum)
!        end do
!      end do
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
!
      return
!
      do inum = 1, pvr_start%num_pvr_ray
        ipix = pvr_start%id_pixel_start(inum)
        ip = irank_4_composit(ipix) + 1
        num_send_pixel_composit(ip) = num_send_pixel_composit(ip) + 1
      end do
!
      istack_send_pixel_composit(0) = 0
      do ip = 1, nprocs
        id_rank = mod(ip + my_rank,nprocs)
        irank_send_pixel_composit(ip) = id_rank
        istack_send_pixel_composit(ip)                                  &
     &             = istack_send_pixel_composit(ip-1)                   &
     &              + num_send_pixel_composit(id_rank+1)
      end do
      ntot_send_pixel_composit = istack_send_pixel_composit(nprocs)
!
      allocate(item_send_pixel_composit(ntot_send_pixel_composit))
      allocate(ipix_send_pixel_composit(ntot_send_pixel_composit))
      allocate(depth_send_pixel_composit(ntot_send_pixel_composit))
!
!$omp parallel workshare
      num_send_pixel_composit(1:nprocs) = 0
!$omp end parallel workshare
      do inum = 1, pvr_start%num_pvr_ray
        ipix = pvr_start%id_pixel_start(inum)
        ip = irank_4_composit(ipix)
        do jp = 1, nprocs
          if(irank_send_pixel_composit(jp) .eq. ip) then
            num_send_pixel_composit(ip)                                 &
     &             = num_send_pixel_composit(ip) + 1
            icou = istack_send_pixel_composit(ip-1)                     &
     &              + num_send_pixel_composit(ip)
            item_send_pixel_composit(icou) = inum
            ipix_send_pixel_composit(icou) = ipix
            depth_send_pixel_composit(icou)                             &
     &              = pvr_start%xx_pvr_ray_start(inum,3)
            exit
          end if
        end do
      end do
!
      deallocate(item_send_pixel_composit)
      deallocate(ipix_send_pixel_composit)
      deallocate(depth_send_pixel_composit)
!
      deallocate(istack_send_pixel_composit)
      deallocate(num_send_pixel_composit)
!
      deallocate(ipixel_4_composit)
!
      deallocate(irank_4_composit)
      deallocate(irev_recv_image, item_recv_image)
      deallocate(istack_recv_image)
      if(my_rank .eq. image_out)  then
        deallocate(irank_image_stack, istack_ray_start_gl)
      end if
!
      deallocate(num_ray_start_lc)
      if(my_rank .eq. image_out)  deallocate(num_ray_start_gl)
!
!
      end subroutine set_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_stencil_buffer
