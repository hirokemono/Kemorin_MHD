!>@file   t_local_node_id_in_import.f90
!!@brief  module t_local_node_id_in_import
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine init_local_node_id_in_import(new_comm, numnod_new,   &
!!     &                                        inod_recv, lcl_i)
!!      subroutine dealloc_local_nod_id_in_import(lcl_i)
!!        type(communication_table), intent(in) :: new_comm
!!        integer(kind = kint), intent(in) :: numnod_new
!!        integer(kind = kint), intent(in) :: inod_recv(numnod_new)
!!        type(local_node_id_in_import), intent(inout) :: lcl_i
!!
!!      subroutine alloc_local_nod_num_in_import(numnod_new, inod_recv, &
!!     &                                         lcl_i)
!!      subroutine dealloc_item_import_recv(lcl_i)
!!        integer(kind = kint), intent(in) :: numnod_new
!!        integer(kind = kint), intent(in) :: inod_recv(numnod_new)
!!        type(local_node_id_in_import), intent(inout) :: lcl_i
!!@endverbatim
!
      module t_local_node_id_in_import
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_comm_table
!
      implicit none
!
!>      Structure of local node IDs for import table
      type local_node_id_in_import
!>        Total number of items
        integer(kind = kint) :: nmax_import
!>        Number of items for each local node ID
        integer(kind = kint), allocatable :: num_rev_import_recv(:)
!
!>        number of local node ID corresponding with import table
        integer(kind = kint), allocatable :: ntot_recv
!>        Stack for each local node ID
        integer(kind = kint), allocatable :: istack_rev_import_recv(:)
!>        Home process rank each local node ID
        integer(kind = kint), allocatable :: irank_import_recv(:)
!>        Reverse import table using local ID
        integer(kind = kint), allocatable :: irev_import(:)
!
!>        local node ID corresponding with import table
        integer(kind = kint), allocatable :: item_import_recv(:)
      end type local_node_id_in_import
!
      private :: alloc_item_import_recv, set_item_import_recv
      private :: alloc_local_nod_num_in_import
      private :: alloc_local_nod_id_in_import
      private :: count_new_comm_irev_import, set_new_comm_irev_import
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_local_node_id_in_import(new_comm, numnod_new,     &
     &                                        inod_recv, lcl_i)
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
!
      type(local_node_id_in_import), intent(inout) :: lcl_i
!
!
      call alloc_local_nod_num_in_import(numnod_new, inod_recv, lcl_i)
      call count_new_comm_irev_import(new_comm, lcl_i%nmax_import,      &
     &    numnod_new, inod_recv, lcl_i%num_rev_import_recv,             &
     &    lcl_i%istack_rev_import_recv)
!
      call alloc_local_nod_id_in_import(new_comm%ntot_import, lcl_i)
      call set_new_comm_irev_import(new_comm, lcl_i%nmax_import,        &
     &    lcl_i%istack_rev_import_recv, numnod_new, inod_recv,          &
     &    lcl_i%num_rev_import_recv, lcl_i%irev_import,                 &
     &    lcl_i%irank_import_recv)
!
      end subroutine init_local_node_id_in_import
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_local_nod_id_in_import(lcl_i)
!
      type(local_node_id_in_import), intent(inout) :: lcl_i
!
!
      deallocate(lcl_i%irev_import, lcl_i%irank_import_recv)
      deallocate(lcl_i%istack_rev_import_recv)
      deallocate(lcl_i%num_rev_import_recv)
!
      end subroutine dealloc_local_nod_id_in_import
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_item_import_recv(new_comm, numnod_new,            &
     &                                 inod_recv, lcl_i)
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
!
      type(local_node_id_in_import), intent(inout) :: lcl_i
!
!
      call alloc_item_import_recv(new_comm%ntot_import, lcl_i)
      call set_item_import_recv(new_comm, numnod_new, inod_recv,        &
     &                          lcl_i%item_import_recv)
!
      end subroutine init_item_import_recv
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_item_import_recv(lcl_i)
!
      type(local_node_id_in_import), intent(inout) :: lcl_i
!
      if(.not. allocated(lcl_i%item_import_recv)) return
      deallocate(lcl_i%item_import_recv)
!
      end subroutine dealloc_item_import_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_local_nod_num_in_import(numnod_new, inod_recv,   &
     &                                         lcl_i)
!
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
      type(local_node_id_in_import), intent(inout) :: lcl_i
!
!
      lcl_i%nmax_import = maxval(inod_recv)
      allocate(lcl_i%num_rev_import_recv(lcl_i%nmax_import))
      allocate(lcl_i%istack_rev_import_recv(0:lcl_i%nmax_import))
      lcl_i%istack_rev_import_recv(0) = 0
      if(lcl_i%nmax_import .le. 0) return
!
!$omp parallel workshare
      lcl_i%istack_rev_import_recv(1:lcl_i%nmax_import) = 0
      lcl_i%num_rev_import_recv(1:lcl_i%nmax_import) =    0
!$omp end parallel workshare
!
      end subroutine alloc_local_nod_num_in_import
!
! ----------------------------------------------------------------------
!
      subroutine alloc_local_nod_id_in_import(ntot_import, lcl_i)
!
      integer(kind = kint), intent(in) :: ntot_import
      type(local_node_id_in_import), intent(inout) :: lcl_i
!
!
      lcl_i%ntot_recv = ntot_import
      allocate(lcl_i%irank_import_recv(lcl_i%ntot_recv))
      allocate(lcl_i%irev_import(lcl_i%ntot_recv))
!
      if(lcl_i%ntot_recv .lt. 0) return
!$omp parallel workshare
      lcl_i%irank_import_recv(1:lcl_i%ntot_recv) = 0
      lcl_i%irev_import(1:lcl_i%ntot_recv) =       0
!$omp end parallel workshare
!
      end subroutine alloc_local_nod_id_in_import
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_new_comm_irev_import                             &
     &         (new_comm, nmax_import, numnod_new, inod_recv,           &
     &          num_rev_import_recv, istack_rev_import_recv)
!
      use cal_minmax_and_stacks
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
      integer(kind = kint), intent(in) :: nmax_import
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_rev_import_recv(nmax_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_rev_import_recv(0:nmax_import)
!
      integer(kind = kint) :: inod, jnum, jnod, ntot_tmp
!
!
      istack_rev_import_recv(0:nmax_import) = 0
      num_rev_import_recv(1:nmax_import) = 0
!
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        inod = inod_recv(jnod)
        num_rev_import_recv(inod) = num_rev_import_recv(inod) + 1
      end do
!
      call s_cal_total_and_stacks(nmax_import, num_rev_import_recv,     &
     &    izero, istack_rev_import_recv, ntot_tmp)
!
      do inod = 1, nmax_import
        istack_rev_import_recv(inod) = istack_rev_import_recv(inod-1)   &
     &                                + num_rev_import_recv(inod)
      end do
!
      if(ntot_tmp .ne. new_comm%ntot_import) then
        write(*,*) 'Wrong total number of node stack'
      end if
!
      end subroutine count_new_comm_irev_import
!
! ----------------------------------------------------------------------
!
      subroutine set_new_comm_irev_import(new_comm, nmax_import,        &
     &          istack_rev_import_recv, numnod_new, inod_recv,          &
     &          num_rev_import_recv, irev_import, irank_import_recv)
!
      use quicksort
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: nmax_import
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_rev_import_recv(0:nmax_import)
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_rev_import_recv(nmax_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: irev_import(new_comm%ntot_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: irank_import_recv(new_comm%ntot_import)
!
      integer(kind = kint) :: ip, ist, ied, inod, jnum, jnod, jst, num
!
!
!$omp parallel workshare
      num_rev_import_recv(1:nmax_import) = 0
!$omp end parallel workshare
!
      do ip = 1, new_comm%num_neib
        ist = new_comm%istack_import(ip-1) + 1
        ied = new_comm%istack_import(ip  )
        do jnum = ist, ied
          jnod = new_comm%item_import(jnum)
          inod = inod_recv(jnod)
          jst = istack_rev_import_recv(inod-1)
          num = num_rev_import_recv(inod) + 1
!
          irank_import_recv(jst+num) =  new_comm%id_neib(ip)
          irev_import(jst+num) = jnum
          num_rev_import_recv(inod) = num
        end do
      end do
!
      do inod = 1, nmax_import
        ist = istack_rev_import_recv(inod-1) + 1
        ied = istack_rev_import_recv(inod  )
        if((ied-ist) .gt. 1) then
          call quicksort_w_index(new_comm%ntot_import,                  &
     &        irank_import_recv, ist, ied, irev_import)
        end if
      end do
!
      end subroutine set_new_comm_irev_import
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_item_import_recv(ntot_import, lcl_i)
!
      integer(kind = kint), intent(in) :: ntot_import
      type(local_node_id_in_import), intent(inout) :: lcl_i
!
      lcl_i%ntot_recv = ntot_import
      allocate(lcl_i%item_import_recv(lcl_i%ntot_recv))
!
      if(lcl_i%ntot_recv .le. 0) return
!$omp parallel workshare
      lcl_i%item_import_recv(1:lcl_i%ntot_recv) = 0
!$omp end parallel workshare
!
      end subroutine alloc_item_import_recv
!
! ----------------------------------------------------------------------
!
      subroutine set_item_import_recv(new_comm, numnod_new,             &
     &                                inod_recv, item_import_recv)
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
      integer(kind = kint), intent(inout)                               &
     &                     :: item_import_recv(new_comm%ntot_import)
!
      integer(kind = kint) :: jnum, jnod
!
!$omp parallel do private(jnum,jnod)
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        item_import_recv(jnum) = inod_recv(jnod)
      end do
!$omp end parallel do
!
      end subroutine set_item_import_recv
!
! ----------------------------------------------------------------------
!
      end module t_local_node_id_in_import

