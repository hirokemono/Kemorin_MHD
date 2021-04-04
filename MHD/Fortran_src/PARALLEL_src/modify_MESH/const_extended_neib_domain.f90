!>@file   const_extended_neib_domain.f90
!!@brief  module const_extended_neib_domain
!!
!!@author H. Matsui
!!@date Programmed in March, 2021
!
!>@brief  Trim redundant impoert items from sorted import list
!!
!!@verbatim
!!      subroutine s_const_extended_neib_domain                         &
!!     &         (nod_comm, inod_dbl, mark_nod,                         &
!!     &          add_nod_comm, iflag_process_extend)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(mark_for_each_comm),  intent(in)                         &
!!     &                           :: mark_nod(nod_comm%num_neib)
!!        type(communication_table), intent(inout) :: add_nod_comm
!!        integer(kind = kint), intent(inout) :: iflag_process_extend
!!@endverbatim
      module const_extended_neib_domain
!
      use m_precision
!
      use t_comm_table
      use t_calypso_comm_table
      use t_para_double_numbering
      use t_mark_node_ele_to_extend
!
      implicit none
!
      private :: set_neighbour_domain_by_flag
      private :: count_extended_neib_import, count_extended_neib_export
      private :: count_domain_extended_export
      private :: set_domain_to_export_extend
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_extended_neib_domain                           &
     &         (nod_comm, inod_dbl, mark_nod,                           &
     &          add_nod_comm, iflag_process_extend)
!
      use calypso_mpi
      use calypso_mpi_int
      use reverse_SR_int
      use cal_minmax_and_stacks
!
      implicit none
!
      type(communication_table), intent(in) :: nod_comm
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(mark_for_each_comm),  intent(in)                             &
     &                           :: mark_nod(nod_comm%num_neib)
!
      type(calypso_comm_table), intent(inout) :: add_nod_comm
      integer(kind = kint), intent(inout) :: iflag_process_extend
!
      integer(kind = kint), allocatable :: np_new_export(:)
      integer(kind = kint), allocatable :: istack_pe_new_export(:)
      integer(kind = kint) :: ntot_pe_new_export
      integer(kind = kint), allocatable :: np_new_import(:)
      integer(kind = kint), allocatable :: istack_pe_new_import(:)
      integer(kind = kint) :: ntot_pe_new_import
      integer(kind = kint), allocatable :: ip_new_export(:)
      integer(kind = kint), allocatable :: ip_new_import(:)
!
      integer(kind = kint), allocatable :: iflag_send_pe(:)
      integer(kind = kint), allocatable :: iflag_recv_pe(:)
!
      integer(kind = kint) :: i, ist, ied
!
!
      allocate(iflag_send_pe(nprocs))
      allocate(np_new_export(nod_comm%num_neib))
      allocate(istack_pe_new_export(0:nod_comm%num_neib))
      allocate(np_new_import(nod_comm%num_neib))
      allocate(istack_pe_new_import(0:nod_comm%num_neib))
!
      call count_domain_extended_export(nod_comm,                       &
     &    inod_dbl, mark_nod, np_new_export, iflag_send_pe)
      call s_cal_total_and_stacks(nod_comm%num_neib, np_new_export,     &
     &    izero, istack_pe_new_export, ntot_pe_new_export)
!
      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib, np_new_export,           &
     &    np_new_import, istack_pe_new_import, ntot_pe_new_import)
!
      allocate(ip_new_export(ntot_pe_new_export))
      allocate(ip_new_import(ntot_pe_new_import))
!
      call set_domain_to_export_extend(nod_comm,                        &
     &    inod_dbl, mark_nod, istack_pe_new_export,                     &
     &    ntot_pe_new_export, ip_new_export, iflag_send_pe)
      call comm_items_send_recv                                         &
     &   (nod_comm%num_neib, nod_comm%id_neib, istack_pe_new_export,    &
     &    istack_pe_new_import, ip_new_export, ip_new_import)
!
!      do i = 1, nod_comm%num_neib
!        ist = istack_pe_new_export(i-1)+1
!        ied = istack_pe_new_export(i)
!        write(*,*) my_rank, i, nod_comm%id_neib(i), 'ip_new_export',   &
!     &            ip_new_export(ist:ied)
!      end do
!
!      do i = 1, nod_comm%num_neib
!        ist = istack_pe_new_import(i-1)+1
!        ied = istack_pe_new_import(i)
!        write(*,*) my_rank, nod_comm%id_neib(i), 'ip_new_import',      &
!     &            ip_new_import(ist:ied)
!      end do
!
      allocate(iflag_recv_pe(nprocs))
!$omp parallel workshare
      iflag_recv_pe(1:nprocs) = -1
      iflag_send_pe(1:nprocs) = -1
!$omp end parallel workshare
!
      call count_extended_neib_import(nprocs, nod_comm,                 &
     &    istack_pe_new_import, ntot_pe_new_import, ip_new_import,      &
     &    iflag_recv_pe, add_nod_comm%nrank_import, iflag_process_extend)
!      write(*,*) my_rank, iflag_process_extend, 'new_num_neib',   &
!     &           nod_comm%num_neib, add_nod_comm%num_neib
!
      call calypso_mpi_alltoall_one_int(iflag_recv_pe, iflag_send_pe)
      call count_extended_neib_export(nprocs, nod_comm,                 &
     &    iflag_send_pe, add_nod_comm%nrank_export)
!
      call alloc_calypso_import_num(add_nod_comm)
      call set_neighbour_domain_by_flag(my_rank, nprocs, iflag_recv_pe, &
     &    add_nod_comm%nrank_import, add_nod_comm%irank_import)
!
      call alloc_calypso_export_num(add_nod_comm)
      call set_neighbour_domain_by_flag(my_rank, nprocs, iflag_send_pe, &
     &    add_nod_comm%nrank_export, add_nod_comm%irank_export)
      deallocate(ip_new_export, ip_new_import, istack_pe_new_import)
      deallocate(iflag_recv_pe, iflag_send_pe)
!
      end subroutine s_const_extended_neib_domain
!
! ----------------------------------------------------------------------
!
      subroutine const_extended_neib_domain_org                         &
     &         (nod_comm, add_nod_comm, add_nod_comm_org)
!
      use calypso_mpi
      use calypso_mpi_int
      use reverse_SR_int
!
      implicit none
!
      type(communication_table), intent(in) :: nod_comm
      type(calypso_comm_table), intent(in) :: add_nod_comm
!
      type(communication_table), intent(inout) :: add_nod_comm_org
!
      integer(kind = kint), allocatable :: iflag_recv_pe(:)
!
      integer(kind = kint) :: i, ist, ied, ip, irank, icou
!
!
      allocate(iflag_recv_pe(nprocs))
!
!$omp parallel workshare
      iflag_recv_pe(1:nprocs) = -1
!$omp end parallel workshare
!$omp parallel do private(i,irank)
      do i = 1, nod_comm%num_neib
        irank = nod_comm%id_neib(i)
        iflag_recv_pe(irank+1) = i
      end do
!$omp end parallel do
!
      add_nod_comm_org%num_neib = nod_comm%num_neib
      do i = 1, add_nod_comm%nrank_import
          irank = add_nod_comm%irank_import(i)
          if(iflag_recv_pe(irank+1) .eq. -1) then
            add_nod_comm_org%num_neib = add_nod_comm_org%num_neib + 1
            iflag_recv_pe(irank+1) =  add_nod_comm_org%num_neib
          end if
        end do
      end do
!
      call alloc_comm_table_num(add_nod_comm_org)
      icou = 0
      do ip = 1, nprocs
        irank = mod(my_rank+ip,nprocs)
        if(iflag_recv_pe(irank+1) .gt. 0) then
          icou = icou + 1 
          add_nod_comm_org%id_neib(icou) = irank
        end if
      end do
      deallocate(iflag_recv_pe)
!
      end subroutine const_extended_neib_domain_org
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_neighbour_domain_by_flag(my_rank, nprocs,          &
     &          iflag_recv_pe, num_add_neib, id_add_neib)
!
      implicit none
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: iflag_recv_pe(nprocs)
      integer(kind = kint), intent(in) :: num_add_neib
      integer(kind = kint), intent(inout):: id_add_neib(num_add_neib)
!
      integer(kind = kint) :: icou, ip, irank
!
!
      icou = 0
      do ip = 1, nprocs
        irank = mod(my_rank+ip,nprocs)
        if(iflag_recv_pe(irank+1) .gt. 0) then
          icou = icou + 1 
          id_add_neib(icou) = irank
        end if
      end do
!
      end subroutine set_neighbour_domain_by_flag
!
! ----------------------------------------------------------------------
!
      subroutine count_extended_neib_import(nprocs, nod_comm,           &
     &          istack_pe_new_neib, ntot_pe_new_neib, ip_new_neib,      &
     &          iflag_pe, num_add_neib, iflag_process_extend)
!
      use t_comm_table
!
      implicit none
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), intent(in)                                  &
     &              :: istack_pe_new_neib(0:nod_comm%num_neib)
      integer(kind = kint), intent(in) :: ntot_pe_new_neib
      integer(kind = kint), intent(in) :: ip_new_neib(ntot_pe_new_neib)
      integer, intent(in) :: nprocs
!
      integer(kind = kint), intent(inout) :: iflag_pe(nprocs)
      integer(kind = kint), intent(inout) :: num_add_neib
      integer(kind = kint), intent(inout) :: iflag_process_extend
!
      integer(kind = kint) :: i, ist, ied, inum, irank
!
!
!$omp parallel workshare
      iflag_pe(1:nprocs) = -2
!$omp end parallel workshare
!
!$omp parallel do private(i,irank)
      do i = 1, nod_comm%num_neib
        irank = nod_comm%id_neib(i)
        iflag_pe(irank) = -1
      end do
!$omp end parallel do
!
      iflag_process_extend = 0
      num_add_neib =         0
      do i = 1, nod_comm%num_neib
        ist = istack_pe_new_neib(i-1)+1
        ied = istack_pe_new_neib(i)
        do inum = ist, ied
          irank = ip_new_neib(inum)
          if(iflag_pe(irank+1) .le. -1) then
            if(iflag_pe(irank+1) .eq. -2) iflag_process_extend = 1
            num_add_neib = num_add_neib + 1
            iflag_pe(irank+1) =  num_add_neib
          end if
        end do
      end do
!
      end subroutine count_extended_neib_import
!
! ----------------------------------------------------------------------
!
      subroutine count_extended_neib_export(nprocs, nod_comm,           &
     &                                      iflag_pe, num_add_neib)
!
      use t_comm_table
!
      implicit none
!
      type(communication_table), intent(in) :: nod_comm
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: iflag_pe(nprocs)
!
      integer(kind = kint), intent(inout) :: num_add_neib
!
      integer(kind = kint) :: i
!
!
      num_add_neib = 0
      do i = 1, nprocs
        if(iflag_pe(i) .ge. 0) num_add_neib = num_add_neib + 1
      end do
!
      end subroutine count_extended_neib_export
!
! ----------------------------------------------------------------------
!
      subroutine count_extended_neib_domain_org                         &
     &         (nprocs, nod_comm, istack_pe_new_import,                 &
     &          ntot_pe_new_import, ip_new_import,                      &
     &          iflag_pe, num_add_neib, iflag_process_extend)
!
      use t_comm_table
!
      implicit none
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), intent(in)                               &
     &              :: istack_pe_new_import(0:nod_comm%num_neib)
      integer(kind = kint), intent(in) :: ntot_pe_new_import
      integer(kind = kint), intent(in)                               &
     &              :: ip_new_import(ntot_pe_new_import)
      integer, intent(in) :: nprocs
!
      integer(kind = kint), intent(inout) :: iflag_pe(nprocs)
      integer(kind = kint), intent(inout) :: num_add_neib
      integer(kind = kint), intent(inout) :: iflag_process_extend
!
      integer(kind = kint) :: i, ist, ied, inum, irank
!
!
!$omp parallel do private(i,irank)
      do i = 1, nod_comm%num_neib
        irank = nod_comm%id_neib(i)
        iflag_pe(irank+1) = i
      end do
!$omp end parallel do
!
      iflag_process_extend = 0
      num_add_neib = nod_comm%num_neib
      do i = 1, nod_comm%num_neib
        ist = istack_pe_new_import(i-1)+1
        ied = istack_pe_new_import(i)
        do inum = ist, ied
          irank = ip_new_import(inum)
          if(iflag_pe(irank+1) .eq. -1) then
            num_add_neib = num_add_neib + 1
            iflag_pe(irank+1) =  num_add_neib
            iflag_process_extend = 1
          end if
        end do
      end do
!
      end subroutine count_extended_neib_domain_org
!
! ----------------------------------------------------------------------
!
      subroutine count_domain_extended_export(nod_comm,                 &
     &          inod_dbl, mark_nod, np_new_export, iflag_send_pe)
!
      type(communication_table), intent(in) :: nod_comm
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(mark_for_each_comm),  intent(in)                             &
     &                           :: mark_nod(nod_comm%num_neib)
!
      integer(kind = kint), intent(inout) :: iflag_send_pe(nprocs)
      integer(kind = kint), intent(inout)                               &
     &                     :: np_new_export(nod_comm%num_neib)
!
      integer(kind = kint) :: i, inod, inum, irank
!
!
      do i = 1, nod_comm%num_neib
!$omp parallel workshare
        iflag_send_pe(1:nprocs) = 0
!$omp end parallel workshare
!
        np_new_export(i) = 0
        do inum = 1, mark_nod(i)%num_marked
          inod = mark_nod(i)%idx_marked(inum)
          irank = inod_dbl%irank(inod)
          if(iflag_send_pe(irank+1) .eq. 0) then
            np_new_export(i) = np_new_export(i) + 1
            iflag_send_pe(irank+1) = 1
          end if
        end do
      end do
!
      end subroutine count_domain_extended_export
!
! ----------------------------------------------------------------------
!
      subroutine set_domain_to_export_extend(nod_comm,                 &
     &          inod_dbl, mark_nod, istack_pe_new_export,              &
     &          ntot_pe_new_export, ip_new_export, iflag_send_pe)
!
      type(communication_table), intent(in) :: nod_comm
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(mark_for_each_comm),  intent(in)                             &
     &                           :: mark_nod(nod_comm%num_neib)
      integer(kind = kint), intent(in)                                  &
     &               :: istack_pe_new_export(0:nod_comm%num_neib)
      integer(kind = kint), intent(in) :: ntot_pe_new_export
!
      integer(kind = kint), intent(inout) :: iflag_send_pe(nprocs)
      integer(kind = kint), intent(inout)                               &
     &                     :: ip_new_export(ntot_pe_new_export)
!
      integer(kind = kint) :: i, icou, inod, inum, irank
!
!
      do i = 1, nod_comm%num_neib
!$omp parallel workshare
        iflag_send_pe(1:nprocs) = 0
!$omp end parallel workshare
!
        icou = istack_pe_new_export(i-1)
        do inum = 1, mark_nod(i)%num_marked
          inod = mark_nod(i)%idx_marked(inum)
          irank = inod_dbl%irank(inod)
          if(iflag_send_pe(irank+1) .eq. 0) then
            icou = icou + 1
            ip_new_export(icou) =    irank
            iflag_send_pe(irank+1) = 1
          end if
        end do
      end do
!
      end subroutine set_domain_to_export_extend
!
! ----------------------------------------------------------------------
!
      end module const_extended_neib_domain
