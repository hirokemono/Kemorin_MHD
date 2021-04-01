!>@file   const_repart_comm_tbl.f90
!!@brief  module const_repart_comm_tbl
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine const_repartitioned_comm_tbl                         &
!!     &         (internal_node, num_recv_nod, num_recv_trim,           &
!!     &          ntot_import, irank_sort, inod_sort, iflag_dup,        &
!!     &          new_comm)
!!        type(communication_table), intent(inout) :: new_comm
!!@endverbatim
!
      module const_repart_comm_tbl
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_comm_table
!
      implicit none
!
      private :: cnt_repartitioned_num_neib
      private :: cnt_repartitioned_import_num
      private :: set_repartitioned_import_item
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_repartitioned_comm_tbl                           &
     &         (internal_node, num_recv_nod, num_recv_trim,             &
     &          ntot_import, irank_sort, inod_sort, iflag_dup,          &
     &          new_comm)
!
      use calypso_mpi
      use reverse_SR_int
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: num_recv_nod(nprocs)
      integer(kind = kint), intent(in) :: num_recv_trim(nprocs)
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: irank_sort(ntot_import)
      integer(kind = kint), intent(in) :: inod_sort(ntot_import)
      integer(kind = kint), intent(in) :: iflag_dup(ntot_import)
!
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint), allocatable :: inod_external(:)
      integer(kind = kint), allocatable :: irank_external(:)
!
!
      call cnt_repartitioned_num_neib                                   &
     &   (my_rank, nprocs, num_recv_trim, new_comm%num_neib)
      call alloc_comm_table_num(new_comm)
!
      call cnt_repartitioned_import_num(my_rank, nprocs,                &
     &    num_recv_trim, new_comm%num_neib, new_comm%id_neib,           &
     &    new_comm%num_import, new_comm%istack_import,                  &
     &    new_comm%ntot_import)
!
      new_comm%ntot_import = new_comm%istack_import(new_comm%num_neib)
      call alloc_import_item(new_comm)
!
      allocate(inod_external(new_comm%ntot_import))
      allocate(irank_external(new_comm%ntot_import))
!$omp parallel workshare
      inod_external(1:new_comm%ntot_import) =  0
      irank_external(1:new_comm%ntot_import) = 0
!$omp end parallel workshare
!
      call set_repartitioned_import_item                                &
     &   (my_rank, nprocs, internal_node, num_recv_nod,                 &
     &    ntot_import, irank_sort, inod_sort, iflag_dup,                &
     &    new_comm%ntot_import, new_comm%item_import,                   &
     &    irank_external, inod_external)
!
      call num_items_send_recv                                          &
     &   (new_comm%num_neib, new_comm%id_neib, new_comm%num_import,     &
     &    new_comm%num_export, new_comm%istack_export,                  &
     &    new_comm%ntot_export)
!
      call alloc_export_item(new_comm)
      call comm_items_send_recv(new_comm%num_neib, new_comm%id_neib,    &
     &    new_comm%istack_import, new_comm%istack_export,               &
     &    inod_external, new_comm%item_export)
      deallocate(inod_external, irank_external)
!
      end subroutine const_repartitioned_comm_tbl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cnt_repartitioned_num_neib                             &
     &         (my_rank, nprocs, num_recv_trim, num_neib)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: num_recv_trim(nprocs)
      integer(kind = kint), intent(inout) :: num_neib
!
      integer(kind = kint) :: i, ip
!
!
      num_neib = 0
      do i = 1, nprocs-1
        ip = mod(i+my_rank,nprocs)
        if(num_recv_trim(ip+1) .gt. 0) then
          num_neib = num_neib + 1
        end if
      end do
!
      end subroutine cnt_repartitioned_num_neib
!
! ----------------------------------------------------------------------
!
      subroutine cnt_repartitioned_import_num                           &
     &         (my_rank, nprocs, num_recv_trim, num_neib, id_neib,      &
     &          num_import, istack_import, ntot_import)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: num_recv_trim(nprocs)
      integer(kind = kint), intent(in) :: num_neib
!
      integer(kind = kint), intent(inout) :: id_neib(num_neib)
      integer(kind = kint), intent(inout) :: num_import(num_neib)
      integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
      integer(kind = kint), intent(inout) :: ntot_import
!
      integer(kind = kint) :: i, ip, icou
!
!
      icou = 0
      istack_import(icou) = 0
      do i = 1, nprocs-1
        ip = mod(i+my_rank,nprocs)
        if(num_recv_trim(ip+1) .gt. 0) then
          icou = icou + 1
          ip = mod(i+my_rank,nprocs)
          id_neib(icou) = mod(i+my_rank,nprocs)
          num_import(icou) = num_recv_trim(ip+1)
          istack_import(icou)                                           &
     &         = istack_import(icou-1) + num_recv_trim(ip+1)
        end if
      end do
      ntot_import = istack_import(num_neib)
!
      end subroutine cnt_repartitioned_import_num
!
! ----------------------------------------------------------------------
!
      subroutine set_repartitioned_import_item                          &
     &         (my_rank, nprocs, internal_node, num_recv_nod,           &
     &          ntot_import, irank_sort, inod_sort, iflag_dup,          &
     &          ntot_comm_import, item_import,                          &
     &          irank_external, inod_external)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: num_recv_nod(nprocs)
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: irank_sort(ntot_import)
      integer(kind = kint), intent(in) :: inod_sort(ntot_import)
      integer(kind = kint), intent(in) :: iflag_dup(ntot_import)
!
      integer(kind = kint), intent(in) :: ntot_comm_import
      integer(kind = kint), intent(inout)                               &
     &              :: item_import(ntot_comm_import)
      integer(kind = kint), intent(inout)                               &
     &              :: irank_external(ntot_comm_import)
      integer(kind = kint), intent(inout)                               &
     &              :: inod_external(ntot_comm_import)
!
      integer(kind = kint) :: i, j, ip, icou, ist
!
!
      ist = 0
      j = 0
      do i = 1, nprocs-1
        ip = mod(i+my_rank,nprocs)
        do icou = 1, num_recv_nod(ip+1)
          if(iflag_dup(ist+icou) .gt. 0) then
            j = j + 1
            irank_external(j) = irank_sort(ist+icou)
            inod_external(j) =  inod_sort(ist+icou)
            item_import(j) = j + internal_node
          end if
        end do
        ist = ist + num_recv_nod(ip+1)
      end do
!
      end subroutine set_repartitioned_import_item
!
! ----------------------------------------------------------------------
!
      end module const_repart_comm_tbl
