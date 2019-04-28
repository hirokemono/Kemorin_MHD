!const_comm_tbl_img_composit.f90
!
!      module const_comm_tbl_img_composit
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_const_comm_tbl_img_composit                        &
!!     &         (num_pixel_xy, irank_4_composit, num_pvr_ray,          &
!!     &          id_pixel_start, img_composit_tbl)
!!        type(calypso_comm_table), intent(inout) :: img_composit_tbl
!!
      module const_comm_tbl_img_composit
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_comm_tbl_img_composit                          &
     &         (num_pixel_xy, irank_4_composit, num_pvr_ray,            &
     &          id_pixel_start, img_composit_tbl)
!
      use t_calypso_comm_table
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in)                                  &
     &                     :: irank_4_composit(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
!
      type(calypso_comm_table), intent(inout) :: img_composit_tbl
!
      integer(kind = kint), allocatable :: index_pvr_start(:)
      integer(kind = kint), allocatable :: num_send_pixel_tmp(:)
      integer(kind = kint), allocatable :: num_recv_pixel_tmp(:)
!
!
      allocate(index_pvr_start(num_pvr_ray))
      call sort_index_pvr_start                                         &
     &   (num_pvr_ray, id_pixel_start, index_pvr_start)
!
      allocate(num_send_pixel_tmp(nprocs))
      allocate(num_recv_pixel_tmp(nprocs))
!
      call count_num_send_pixel_tmp                                     &
     &   (num_pixel_xy, irank_4_composit, num_pvr_ray,                  &
     &    id_pixel_start, index_pvr_start, num_send_pixel_tmp)
!
      call MPI_Alltoall(num_send_pixel_tmp, 1, CALYPSO_INTEGER,         &
     &                  num_recv_pixel_tmp, 1, CALYPSO_INTEGER,         &
     &                  CALYPSO_COMM, ierr_MPI)
!
!
      call count_comm_pe_pvr_composition                                &
     &   (num_send_pixel_tmp, num_recv_pixel_tmp,                       &
     &    img_composit_tbl%nrank_export, img_composit_tbl%nrank_import)
!
      call alloc_calypso_import_num(img_composit_tbl)
      call alloc_calypso_export_num(img_composit_tbl)
!
      call count_comm_tbl_pvr_composition                               &
     &  (num_send_pixel_tmp, num_recv_pixel_tmp,                        &
     &   img_composit_tbl%nrank_export, img_composit_tbl%nrank_import,  &
     &   img_composit_tbl%ntot_export, img_composit_tbl%irank_export,   &
     &   img_composit_tbl%istack_export, img_composit_tbl%ntot_import,  &
     &   img_composit_tbl%irank_import, img_composit_tbl%istack_import, &
     &   img_composit_tbl%iflag_self_copy)
!
      call alloc_calypso_import_item                                    &
     &   (img_composit_tbl%ntot_import, img_composit_tbl)
      call alloc_calypso_export_item(img_composit_tbl)
!
      call set_comm_tbl_pvr_composition(num_pvr_ray, id_pixel_start,    &
     &   index_pvr_start, num_pixel_xy, irank_4_composit,               &
     &   img_composit_tbl%nrank_export, img_composit_tbl%ntot_export,   &
     &   img_composit_tbl%irank_export, img_composit_tbl%istack_export, &
     &   img_composit_tbl%item_export, img_composit_tbl%ntot_import,    &
     &   img_composit_tbl%item_import, img_composit_tbl%irev_import)
!
      deallocate(num_send_pixel_tmp, num_recv_pixel_tmp)
      deallocate(index_pvr_start)
!
      end subroutine s_const_comm_tbl_img_composit
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sort_index_pvr_start                                   &
     &         (num_pvr_ray, id_pixel_start, index_pvr_start)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &              :: index_pvr_start(num_pvr_ray)
!
      integer(kind = kint), allocatable :: iref_pvr_start(:)
      integer(kind = kint) :: inum
!
!
      allocate(iref_pvr_start(num_pvr_ray))
!
      do inum = 1, num_pvr_ray
        index_pvr_start(inum) = inum
        iref_pvr_start(inum) = id_pixel_start(inum)
      end do
!
      call quicksort_w_index(num_pvr_ray, iref_pvr_start,               &
     &    ione, num_pvr_ray, index_pvr_start)
      deallocate(iref_pvr_start)
!
      end subroutine sort_index_pvr_start
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_send_pixel_tmp                               &
     &         (num_pixel_xy, irank_4_composit, num_pvr_ray,            &
     &          id_pixel_start, index_pvr_start, num_send_pixel_tmp)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in)                                  &
     &                     :: irank_4_composit(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      integer(kind = kint), intent(in) :: index_pvr_start(num_pvr_ray)
!
      integer(kind = kint), intent(inout) :: num_send_pixel_tmp(nprocs)
!
      integer(kind = kint) :: inum, isrt, ipix, ip
!
!
!$omp parallel workshare
      num_send_pixel_tmp(1:nprocs) = 0
!$omp end parallel workshare
      do inum = 1, num_pvr_ray
        isrt = index_pvr_start(inum)
        ipix =  id_pixel_start(isrt)
        ip = irank_4_composit(ipix) + 1
        num_send_pixel_tmp(ip) = num_send_pixel_tmp(ip) + 1
      end do
!
      end subroutine count_num_send_pixel_tmp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_comm_pe_pvr_composition                          &
     &         (num_send_pixel_tmp, num_recv_pixel_tmp,                 &
     &          ncomm_send_pixel_composit, ncomm_recv_pixel_composit)
!
      integer(kind = kint), intent(in) :: num_send_pixel_tmp(nprocs)
      integer(kind = kint), intent(in) :: num_recv_pixel_tmp(nprocs)
!
      integer(kind = kint), intent(inout) :: ncomm_send_pixel_composit
      integer(kind = kint), intent(inout) :: ncomm_recv_pixel_composit
!
      integer(kind = kint) :: ip, icou1, icou2
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
!
      end subroutine count_comm_pe_pvr_composition
!
!  ---------------------------------------------------------------------
!
      subroutine count_comm_tbl_pvr_composition                         &
     &         (num_send_pixel_tmp, num_recv_pixel_tmp,                 &
     &          ncomm_send_pixel_composit, ncomm_recv_pixel_composit,   &
     &          ntot_send_pixel_composit, irank_send_pixel_composit,    &
     &          istack_send_pixel_composit, ntot_recv_pixel_composit,   &
     &          irank_recv_pixel_composit, istack_recv_pixel_composit,  &
     &          iself_pixel_composit)
!
      integer(kind = kint), intent(in) :: num_send_pixel_tmp(nprocs)
      integer(kind = kint), intent(in) :: num_recv_pixel_tmp(nprocs)
!
      integer(kind = kint), intent(in) :: ncomm_send_pixel_composit
      integer(kind = kint), intent(in) :: ncomm_recv_pixel_composit
!
      integer(kind = kint), intent(inout) :: iself_pixel_composit
      integer(kind = kint), intent(inout) :: ntot_send_pixel_composit
      integer(kind = kint), intent(inout)                               &
     &      :: irank_send_pixel_composit(ncomm_send_pixel_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_send_pixel_composit(0:ncomm_send_pixel_composit)
!
      integer(kind = kint), intent(inout) :: ntot_recv_pixel_composit
      integer(kind = kint), intent(inout)                               &
     &      :: irank_recv_pixel_composit(ncomm_recv_pixel_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_recv_pixel_composit(0:ncomm_recv_pixel_composit)
!
      integer(kind = kint) :: ip, icou1, icou2, i_rank
!
!
      icou1 = 0
      icou2 = 0
      iself_pixel_composit = 0
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
          if(i_rank .eq. my_rank) iself_pixel_composit = 1
        end if
        if(num_recv_pixel_tmp(i_rank+1) .gt. 0) then
          icou2 = icou2 + 1
          irank_recv_pixel_composit(icou2) = i_rank
          istack_recv_pixel_composit(icou2)                             &
     &          = istack_recv_pixel_composit(icou2-1)                   &
     &           + num_recv_pixel_tmp(i_rank+1)
          if(i_rank .eq. my_rank) iself_pixel_composit = 1
        end if
      end do
!
      ntot_send_pixel_composit                                          &
     &       = istack_send_pixel_composit(ncomm_send_pixel_composit)
      ntot_recv_pixel_composit                                          &
     &       = istack_recv_pixel_composit(ncomm_recv_pixel_composit)
!
      end subroutine count_comm_tbl_pvr_composition
!
!  ---------------------------------------------------------------------
!
      subroutine set_comm_tbl_pvr_composition                           &
     &         (num_pvr_ray, id_pixel_start, index_pvr_start,           &
     &          num_pixel_xy, irank_4_composit,                         &
     &          ncomm_send_pixel_composit, ntot_send_pixel_composit,    &
     &          irank_send_pixel_composit, istack_send_pixel_composit,  &
     &          item_send_pixel_composit, ntot_recv_pixel_composit,     &
     &          item_recv_pixel_composit, irev_recv_pixel_composit)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in)                                  &
     &                     :: irank_4_composit(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      integer(kind = kint), intent(in) :: index_pvr_start(num_pvr_ray)
!
      integer(kind = kint), intent(in) :: ncomm_send_pixel_composit
      integer(kind = kint), intent(in) :: ntot_send_pixel_composit
      integer(kind = kint), intent(in) :: ntot_recv_pixel_composit
      integer(kind = kint), intent(in)                                  &
     &      :: irank_send_pixel_composit(ncomm_send_pixel_composit)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_send_pixel_composit(0:ncomm_send_pixel_composit)
!
      integer(kind = kint), intent(inout)                               &
     &      :: item_send_pixel_composit(ntot_send_pixel_composit)
!
      integer(kind = kint), intent(inout)                               &
     &      :: item_recv_pixel_composit(ntot_recv_pixel_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: irev_recv_pixel_composit(ntot_recv_pixel_composit)
!
      integer(kind = kint) :: ip, jst, num
      integer(kind = kint) :: inum, icou, isrt, ipix, i_rank
!
!
      icou = 0
      do 
        isrt = index_pvr_start(icou+1)
        ipix =  id_pixel_start(isrt)
        i_rank = irank_4_composit(ipix)
        do ip = 1, ncomm_send_pixel_composit
          if(irank_send_pixel_composit(ip) .eq. i_rank) then
            jst = istack_send_pixel_composit(ip-1)
            num = istack_send_pixel_composit(ip) - jst
            do inum = 1, num
              icou = icou + 1
              isrt = index_pvr_start(icou)
              item_send_pixel_composit(inum+jst) = isrt
            end do
            exit
          end if
        end do
        if(icou .ge. num_pvr_ray) exit
      end do
!
!$omp parallel do
      do inum = 1, ntot_recv_pixel_composit
        item_recv_pixel_composit(inum) = inum
        irev_recv_pixel_composit(inum) = inum
      end do
!$omp end parallel do
!
      end subroutine set_comm_tbl_pvr_composition
!
!  ---------------------------------------------------------------------
!
      end module const_comm_tbl_img_composit
