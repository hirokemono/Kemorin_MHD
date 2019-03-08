!>@file  PVR_image_transfer.f90
!!       module PVR_image_transfer
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!!@date   Modified in Aug., 2016
!
!> @brief Data transfer for PVR image data
!!
!!@verbatim
!!      subroutine alloc_pvr_image_comm_status(PVR_COMM)
!!      subroutine dealloc_pvr_image_comm_status(PVR_COMM)
!!
!!      subroutine count_pixel_with_image(num_pixel_xy, npixel_img,     &
!!     &          iflag_img_pe, iflag_mapped)
!!      subroutine share_num_images_to_compose                          &
!!     &         (num_overlap, istack_overlap, ntot_overlap)
!!
!!      subroutine distribute_pixel_depth                               &
!!     &         (num_overlap, istack_overlap, ntot_overlap,            &
!!     &          npixel_img, istack_pixel, npixel_img_local,           &
!!     &          depth_lc, depth_recv, depth_part, PVR_COMM)
!!      subroutine distribute_segmented_images                          &
!!     &         (num_overlap, istack_overlap, ntot_overlap,            &
!!     &          npixel_img, istack_pixel, npixel_img_local,           &
!!     &          rgba_lc, rgba_recv, rgba_part, PVR_COMM)
!!      subroutine collect_segmented_images                             &
!!     &         (irank_tgt, npixel_img_local, istack_pixel,            &
!!     &          npixel_img, num_pixel_xy, ipixel_small, rgba_whole,   &
!!     &          rgba_rank0, rgba_real_gl, PVR_COMM)
!!@endverbatim
!
      module PVR_image_transfer
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
!>      Structure for MPI status arrays
      type PVR_MPI_FLAGS
!>         status flag for sending
        integer(kind = kint) :: nprocs_PVR
!>         status flag for sending
        integer, allocatable :: sta1(:,:)
!>         status flag for recieving
        integer, allocatable :: sta2(:,:)
!>         status flag for sending
        integer, allocatable :: req1(:  )
!>         status flag for recieving
        integer, allocatable :: req2(:  )
      end type PVR_MPI_FLAGS
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_image_comm_status(PVR_COMM)
!
      type(PVR_MPI_FLAGS), intent(inout) :: PVR_COMM
!
!
      PVR_COMM%nprocs_PVR = nprocs
!
      allocate(PVR_COMM%sta1(MPI_STATUS_SIZE,PVR_COMM%nprocs_PVR))
      allocate(PVR_COMM%req1(PVR_COMM%nprocs_PVR))
      allocate(PVR_COMM%sta2(MPI_STATUS_SIZE,PVR_COMM%nprocs_PVR))
      allocate(PVR_COMM%req2(PVR_COMM%nprocs_PVR))
!
      end subroutine alloc_pvr_image_comm_status
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_image_comm_status(PVR_COMM)
!
      type(PVR_MPI_FLAGS), intent(inout) :: PVR_COMM
!
!
      deallocate(PVR_COMM%sta1, PVR_COMM%req1)
      deallocate(PVR_COMM%sta2, PVR_COMM%req2)
!
      end subroutine dealloc_pvr_image_comm_status
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_pixel_with_image(num_pixel_xy, npixel_img,       &
     &          iflag_img_pe, iflag_mapped)
!
      use cal_minmax_and_stacks
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: num_pixel_xy
!
      integer(kind = kint), intent(inout) :: npixel_img
      integer(kind = kint), intent(inout) :: iflag_img_pe(num_pixel_xy)
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
!
      integer(kind = kint) :: ipix
!
!
      call calypso_mpi_allreduce_int                                    &
     &   (iflag_img_pe, iflag_mapped, cast_long(num_pixel_xy), MPI_MAX)
!
      npixel_img = 0
      do ipix = 1, num_pixel_xy
        iflag_img_pe(ipix) = iflag_mapped(ipix)
        npixel_img = npixel_img + iflag_img_pe(ipix)
      end do
!
      end subroutine count_pixel_with_image
!
!  ---------------------------------------------------------------------
!
      subroutine share_num_images_to_compose                            &
     &         (num_overlap, istack_overlap, ntot_overlap)
!
      integer(kind = kint), intent(in) :: num_overlap
      integer(kind = kint), intent(inout) :: ntot_overlap
      integer(kind = kint), intent(inout) :: istack_overlap(0:nprocs)
!
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(num_overlap, 1, CALYPSO_INTEGER,               &
     &                   istack_overlap(1), 1, CALYPSO_INTEGER,         &
     &                   CALYPSO_COMM, ierr_MPI)
      istack_overlap(0) = 0
      do ip = 1, nprocs
        istack_overlap(ip) =  istack_overlap(ip-1) + istack_overlap(ip)
      end do
      ntot_overlap = istack_overlap(nprocs)
!
      end subroutine share_num_images_to_compose
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine distribute_pixel_depth                                 &
     &         (num_overlap, istack_overlap, ntot_overlap,              &
     &          npixel_img, istack_pixel, npixel_img_local,             &
     &          depth_lc, depth_recv, depth_part, PVR_COMM)
!
      type(PVR_MPI_FLAGS), intent(inout) :: PVR_COMM
!
      integer(kind = kint), intent(in) :: num_overlap, npixel_img
      real(kind = kreal), intent(in)                                    &
     &                    :: depth_lc(num_overlap,npixel_img)
!
      integer(kind = kint), intent(in) :: istack_pixel(0:nprocs)
      integer(kind = kint), intent(in) :: npixel_img_local
!
      integer(kind = kint), intent(in) :: istack_overlap(0:nprocs)
      integer(kind = kint), intent(in) :: ntot_overlap
!
      real(kind = kreal), intent(inout)                                 &
     &             :: depth_recv(ntot_overlap*npixel_img_local)
      real(kind = kreal), intent(inout)                                 &
     &             :: depth_part(ntot_overlap,npixel_img_local)
!
      integer(kind = kint) :: ist, ipix, jst
      integer :: nneib_send, nneib_recv
      integer :: i_rank
      integer :: num, icou
!
!
!$omp workshare
      depth_recv(1:ntot_overlap*npixel_img_local) = -1000.0d0
!$omp end workshare
!$omp workshare
      depth_part(1:ntot_overlap,1:npixel_img_local) = -1000.0d0
!$omp end workshare
!
      call calypso_mpi_barrier
!
      nneib_send = 0
      do i_rank = 0, nprocs-1
          nneib_send = nneib_send + 1
          ist =  istack_pixel(i_rank)
          num = int((istack_pixel(i_rank+1) - istack_pixel(i_rank))     &
     &              * num_overlap)
          call MPI_ISEND(depth_lc(1,ist+1), num, CALYPSO_REAL, i_rank,  &
     &        0, CALYPSO_COMM, PVR_COMM%req1(nneib_send), ierr_MPI)
      end do
!
      nneib_recv = 0
      do i_rank = 0, nprocs-1
          nneib_recv = nneib_recv + 1
          jst = istack_overlap(i_rank) * npixel_img_local
          num =  int(npixel_img_local                                   &
     &          * (istack_overlap(i_rank+1) - istack_overlap(i_rank)))
          call MPI_IRECV(depth_recv(jst+1), num, CALYPSO_REAL, i_rank,  &
     &        0, CALYPSO_COMM, PVR_COMM%req2(nneib_recv), ierr_MPI)
      end do
!
      call MPI_WAITALL                                                  &
     &   (nneib_recv, PVR_COMM%req2, PVR_COMM%sta2, ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (nneib_send, PVR_COMM%req1, PVR_COMM%sta1, ierr_MPI)
!
!
!$omp parallel do private(ipix,i_rank,ist,num,icou,jst)
      do ipix = 1, npixel_img_local
        do i_rank = 0, nprocs-1
          ist = istack_overlap(i_rank)
          num = int(istack_overlap(i_rank+1) - istack_overlap(i_rank))
          jst = ist * npixel_img_local + (ipix-1) * num
          do icou = 1, num
            depth_part(ist+icou,ipix) = depth_recv(jst+icou)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine distribute_pixel_depth
!
!  ---------------------------------------------------------------------
!
      subroutine distribute_segmented_images                            &
     &         (num_overlap, istack_overlap, ntot_overlap,              &
     &          npixel_img, istack_pixel, npixel_img_local,             &
     &          rgba_lc, rgba_recv, rgba_part, PVR_COMM)
!
      type(PVR_MPI_FLAGS), intent(inout) :: PVR_COMM
!
      integer(kind = kint), intent(in) :: num_overlap, npixel_img
      real(kind = kreal), intent(in)                                    &
     &                    :: rgba_lc(4,num_overlap,npixel_img)
!
      integer(kind = kint), intent(in) :: istack_pixel(0:nprocs)
      integer(kind = kint), intent(in) :: npixel_img_local
!
      integer(kind = kint), intent(in) :: istack_overlap(0:nprocs)
      integer(kind = kint), intent(in) :: ntot_overlap
!
      real(kind = kreal), intent(inout)                                 &
     &             :: rgba_recv(4,ntot_overlap*npixel_img_local)
      real(kind = kreal), intent(inout)                                 &
     &             :: rgba_part(4,ntot_overlap,npixel_img_local)
!
      integer(kind = kint) :: ist, ipix, jst
      integer :: i_rank, nneib_send, nneib_recv
      integer :: num, icou
!
!
!$omp workshare
      rgba_recv(1:4,ntot_overlap*npixel_img_local) = zero
!$omp end workshare
!$omp workshare
      rgba_part(1:4,1:ntot_overlap,1:npixel_img_local) = zero
!$omp end workshare
!
      call calypso_mpi_barrier
!
      nneib_send = 0
      do i_rank = 0, nprocs-1
!        if(i_rank .eq. my_rank) cycle
          nneib_send = nneib_send + 1
          ist =  istack_pixel(i_rank)
          num = int((istack_pixel(i_rank+1) - istack_pixel(i_rank))     &
     &               * ifour * num_overlap)
          call MPI_ISEND(rgba_lc(1,1,ist+1), num, CALYPSO_REAL, i_rank, &
     &        0, CALYPSO_COMM, PVR_COMM%req1(nneib_send), ierr_MPI)
      end do
!
      nneib_recv = 0
      do i_rank = 0, nprocs-1
!        if(i_rank .eq. my_rank) cycle
          nneib_recv = nneib_recv + 1
          jst = istack_overlap(i_rank) * npixel_img_local
          num = int(ifour * npixel_img_local                            &
     &         * (istack_overlap(i_rank+1) - istack_overlap(i_rank)))
          call MPI_IRECV(rgba_recv(1,jst+1), num, CALYPSO_REAL, i_rank, &
     &        0, CALYPSO_COMM, PVR_COMM%req2(nneib_recv), ierr_MPI)
      end do
!
      call MPI_WAITALL                                                  &
     &   (nneib_recv, PVR_COMM%req2, PVR_COMM%sta2, ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (nneib_send, PVR_COMM%req1, PVR_COMM%sta1, ierr_MPI)
!
!
!$omp parallel do private(ipix,i_rank,ist,num,icou,jst)
      do ipix = 1, npixel_img_local
        do i_rank = 0, nprocs-1
          ist = istack_overlap(i_rank)
          num = istack_overlap(i_rank+1) - istack_overlap(i_rank)
          jst = ist * npixel_img_local + (ipix-1) * num
          do icou = 1, num
            rgba_part(1,ist+icou,ipix) = rgba_recv(1,jst+icou)
            rgba_part(2,ist+icou,ipix) = rgba_recv(2,jst+icou)
            rgba_part(3,ist+icou,ipix) = rgba_recv(3,jst+icou)
            rgba_part(4,ist+icou,ipix) = rgba_recv(4,jst+icou)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine distribute_segmented_images
!
!  ---------------------------------------------------------------------
!
      subroutine collect_segmented_images                               &
     &         (irank_tgt, npixel_img_local, istack_pixel,              &
     &          npixel_img, num_pixel_xy, ipixel_small, rgba_whole,     &
     &          rgba_rank0, rgba_real_gl, PVR_COMM)
!
      type(PVR_MPI_FLAGS), intent(inout) :: PVR_COMM
!
      integer(kind = kint), intent(in) :: irank_tgt
      integer(kind = kint), intent(in) :: istack_pixel(0:nprocs)
      integer(kind = kint), intent(in) :: npixel_img_local
      real(kind = kreal), intent(in) :: rgba_whole(4,npixel_img_local)
!
      integer(kind = kint), intent(in) :: npixel_img, num_pixel_xy
      integer(kind = kint), intent(inout) :: ipixel_small(npixel_img)
!
      real(kind = kreal), intent(inout) :: rgba_rank0(4,npixel_img)
      real(kind = kreal), intent(inout) :: rgba_real_gl(4,num_pixel_xy)
!
      integer(kind = kint) :: ist, ipix
      integer :: i_rank, nneib_send, nneib_recv
      integer :: num, i
!
!
      nneib_send = 0
      nneib_recv = 0
      num = int(ifour * npixel_img_local)
      if(my_rank .ne. irank_tgt) then
        nneib_send = 1
        call MPI_ISEND(rgba_whole(1,1), num, CALYPSO_REAL,              &
     &      irank_tgt, 0, CALYPSO_COMM, PVR_COMM%req1(1), ierr_MPI)
      end if
!
      if(my_rank .eq. irank_tgt) then
        do i_rank = 0, nprocs-1
          if(i_rank .eq. irank_tgt) cycle
!
          nneib_recv = nneib_recv + 1
          ist =          istack_pixel(i_rank)
          num = int((istack_pixel(i_rank+1) - istack_pixel(i_rank))     &
     &         * ifour)
          call MPI_IRECV(rgba_rank0(1,ist+1), num, CALYPSO_REAL,        &
     &        i_rank, 0, CALYPSO_COMM, PVR_COMM%req2(nneib_recv),       &
     &        ierr_MPI)
        end do
      end if
!
      call MPI_WAITALL                                                  &
     &   (nneib_recv, PVR_COMM%req2(1), PVR_COMM%sta2, ierr_MPI)
!
      if(my_rank .eq. irank_tgt) then
        ist = istack_pixel(irank_tgt)
        num = istack_pixel(irank_tgt+1) - ist
!$omp parallel do
        do i = 1, num
          rgba_rank0(1,ist+i) = rgba_whole(1,i)
          rgba_rank0(2,ist+i) = rgba_whole(2,i)
          rgba_rank0(3,ist+i) = rgba_whole(3,i)
          rgba_rank0(4,ist+i) = rgba_whole(4,i)
        end do
!$omp end parallel do
!
        rgba_real_gl(1:4,1:num_pixel_xy) = 0.0d0
!$omp parallel do private(i,ipix)
        do i = 1, npixel_img
          ipix = ipixel_small(i)
          rgba_real_gl(1,ipix) = rgba_rank0(1,i)
          rgba_real_gl(2,ipix) = rgba_rank0(2,i)
          rgba_real_gl(3,ipix) = rgba_rank0(3,i)
          rgba_real_gl(4,ipix) = rgba_rank0(4,i)
        end do
!$omp end parallel do
      end if
!
      call MPI_WAITALL                                                  &
     &   (nneib_send, PVR_COMM%req1(1), PVR_COMM%sta1, ierr_MPI)
!
      end subroutine collect_segmented_images
!
!  ---------------------------------------------------------------------
!
      end module PVR_image_transfer
