!> @file  extend_comm_table.f90
!!      module extend_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine extend_node_comm_table(nod_comm, org_node, dbl_idx,  &
!!     &          neib_nod, new_comm, new_node)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: org_node
!!        type(parallel_double_numbering), intent(in) :: dbl_idx
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(communication_table), intent(inout) :: new_comm
!!        type(node_data), intent(inout) :: new_node
!!@endverbatim
!
      module extend_comm_table
!
      use m_precision
      use m_constants
      use m_phys_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_work_extend_comm_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine extend_node_comm_table(nod_comm, org_node, dbl_idx,    &
     &          neib_nod, new_comm, new_node)
!
      use t_next_node_ele_4_node
      use solver_SR_type
      use extend_comm_table_SR
      use mark_export_nod_ele_extend
      use cal_minmax_and_stacks
      use find_extended_node_and_ele
      use find_extended_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: org_node
      type(parallel_double_numbering), intent(in) :: dbl_idx
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
!
!>      Structure of double numbering
      type(parallel_double_numbering) :: dbl_id2
!
!>      added_comm%item_export :: export table or flag to be added
!>      added_comm%item_import :: import table or flag to be added
      type(communication_table) :: added_comm
      type(node_buffer_2_extend) :: send_nbuf
      type(node_buffer_2_extend) :: recv_nbuf
!
      integer(kind = kint), allocatable :: iflag_recv(:)
      integer(kind = kint), allocatable :: iflag_send(:)
      integer(kind = kint), allocatable :: iflag_node(:)
!
      integer(kind = kint), allocatable :: inod_import_new(:)
      integer(kind = kint), allocatable :: irank_import_new(:)
      integer(kind = kint), allocatable :: inod_export_new(:)
      integer(kind = kint), allocatable :: irank_export_new(:)
!
      integer(kind = kint) :: inum, inod, i, ip
!
!
      allocate(iflag_node(org_node%numnod))
      iflag_node(1:org_node%numnod) = 0
!
      call alloc_added_comm_table_num(nod_comm, added_comm)
!
      do i = 1, nod_comm%num_neib
        call mark_next_node_of_export(i, nod_comm%num_neib,             &
     &      nod_comm%istack_import, nod_comm%item_import,               &
     &      nod_comm%istack_export, nod_comm%item_export,               &
     &      org_node%numnod, neib_nod%ntot, neib_nod%istack_next,       &
     &      neib_nod%inod_next, iflag_node)
!
        do inod = 1, org_node%numnod
          added_comm%num_export(i) = added_comm%num_export(i)           &
     &                              + iflag_node(inod)
        end do
      end do
!
      call s_cal_total_and_stacks                                       &
     &   (added_comm%num_neib, added_comm%num_export, izero,            &
     &    added_comm%istack_export, added_comm%ntot_export)
!
      call alloc_export_item(added_comm)
      call alloc_node_buffer_2_extend                                   &
     &   (added_comm%ntot_export, send_nbuf)
!
      do i = 1, nod_comm%num_neib
        call mark_next_node_of_export(i, nod_comm%num_neib,             &
     &      nod_comm%istack_import, nod_comm%item_import,               &
     &      nod_comm%istack_export, nod_comm%item_export,               &
     &      org_node%numnod, neib_nod%ntot, neib_nod%istack_next,       &
     &      neib_nod%inod_next, iflag_node)
!
        call copy_node_to_extend_buffer(added_comm%istack_export(i-1),  &
     &     org_node, dbl_idx, iflag_node, send_nbuf)
      end do
!
!
      call SOLVER_SEND_RECV_num_type                                    &
     &   (added_comm, added_comm%num_export, added_comm%num_import)
!
      call s_cal_total_and_stacks                                       &
     &   (added_comm%num_neib, added_comm%num_import, izero,            &
     &    added_comm%istack_import, added_comm%ntot_import)
!
!      call check_num_of_added_table(my_rank, added_comm)
!
      call alloc_import_item(added_comm)
      call alloc_node_buffer_2_extend                                   &
     &   (added_comm%ntot_import, recv_nbuf)
!
      call added_geometry_send_recv(added_comm%num_neib,                &
     &    added_comm%id_neib, added_comm%istack_export,                 &
     &    added_comm%ntot_export, send_nbuf%xx_add,                     &
     &    added_comm%istack_import, added_comm%ntot_import,             &
     &    recv_nbuf%xx_add)
!
      call added_global_id_send_recv(added_comm%num_neib,               &
     &    added_comm%id_neib, added_comm%istack_export,                 &
     &    added_comm%ntot_export, send_nbuf%inod_gl_add,                &
     &    added_comm%istack_import, added_comm%ntot_import,             &
     &    recv_nbuf%inod_gl_add)
!
      call added_nod_id_send_recv(added_comm%num_neib,                  &
     &    added_comm%id_neib, added_comm%istack_export,                 &
     &    added_comm%ntot_export, send_nbuf%inod_add,                   &
     &    added_comm%istack_import, added_comm%ntot_import,             &
     &    recv_nbuf%inod_add)
!
      call added_nod_id_send_recv(added_comm%num_neib,                  &
     &    added_comm%id_neib, added_comm%istack_export,                 &
     &    added_comm%ntot_export, send_nbuf%irank_add,                  &
     &    added_comm%istack_import, added_comm%ntot_import,             &
     &    recv_nbuf%irank_add)
!
      call mark_added_nod_import_to_del                                 &
     &   (org_node%numnod, dbl_idx%inod_local, dbl_idx%irank_home,      &
     &    nod_comm%num_neib, nod_comm%id_neib, nod_comm%ntot_import,    &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    added_comm%num_neib, added_comm%id_neib,                      &
     &    added_comm%ntot_import, added_comm%istack_import,             &
     &    recv_nbuf%inod_add, recv_nbuf%irank_add,                      &
     &    added_comm%item_import)
!
!      call check_added_impoert_items                                   &
!     &   (my_rank, nod_comm, added_comm, dbl_idx, recv_nbuf)
!
      call added_nod_id_send_recv(added_comm%num_neib,                  &
     &    added_comm%id_neib, added_comm%istack_import,                 &
     &    added_comm%ntot_import, added_comm%item_import,               &
     &    added_comm%istack_export, added_comm%ntot_export,             &
     &    added_comm%item_export)
!
!      call check_delete_from_SR_list                                   &
!     &   (my_rank, added_comm, send_nbuf, recv_nbuf)
!
      call dealloc_node_buffer_2_extend(send_nbuf)
!
      call count_nodes_by_extend_sleeve(added_comm, org_node, new_node)
!
      call alloc_node_geometry_base(new_node)
      call alloc_double_numbering(new_node%numnod, dbl_id2)
!
      call set_nodes_by_extend_sleeve(recv_nbuf, org_node, dbl_idx,     &
     &    added_comm, new_node, dbl_id2)
!
!      call check_nodes_by_extend_sleeveorg_node, new_node, dbl_id2)
!
      allocate(iflag_recv(0:nprocs-1))
      allocate(iflag_send(0:nprocs-1))
      iflag_recv(0:nprocs-1) = 0
      iflag_send(0:nprocs-1) = 0
!
      call mark_extended_nod_neib_pe(nprocs, nod_comm, added_comm,      &
     &    recv_nbuf, iflag_send, iflag_recv)
!
      do ip = 0, nprocs-1
        call MPI_Scatter(iflag_recv(0), ione, CALYPSO_INTEGER,          &
     &                   iflag_send(ip), ione, CALYPSO_INTEGER,         &
     &                   ip, CALYPSO_COMM, ierr_MPI)
      end do
!
      call count_extended_nod_neib_pe                                   &
     &   (nprocs, iflag_send, iflag_recv, new_comm)
!
      call alloc_comm_table_num(new_comm)
!
      call set_extended_nod_neib_pe(nprocs, my_rank,                    &
     &   iflag_send, iflag_recv, nod_comm, new_comm)
!
      deallocate(iflag_recv, iflag_send)
!
      call count_extended_node_import                                   &
     &   (recv_nbuf, nod_comm, added_comm, new_comm)
!
      call SOLVER_SEND_RECV_num_type                                    &
     &   (new_comm, new_comm%num_import, new_comm%num_export)
!
      call s_cal_total_and_stacks                                       &
     &   (new_comm%num_neib, new_comm%num_import, izero,                &
     &    new_comm%istack_import, new_comm%ntot_import)
      call s_cal_total_and_stacks                                       &
     &   (new_comm%num_neib, new_comm%num_export, izero,                &
     &    new_comm%istack_export, new_comm%ntot_export)
!
      call alloc_comm_table_item(new_comm)
!
      call set_extended_node_import                                     &
     &   (recv_nbuf, nod_comm, added_comm, new_comm)
!
      call dealloc_node_buffer_2_extend(recv_nbuf)
      call dealloc_comm_table(added_comm)
!
!
      allocate(inod_import_new(new_comm%ntot_import))
      allocate(irank_import_new(new_comm%ntot_import))
      inod_import_new = 0
      irank_import_new = -1
!
      allocate(inod_export_new(new_comm%ntot_export))
      allocate(irank_export_new(new_comm%ntot_export))
      inod_export_new = 0
      irank_export_new = -1
!
      do inum = 1, new_comm%ntot_import
        inod =  new_comm%item_import(inum)
        inod_import_new(inum) =  dbl_id2%inod_local(inod)
        irank_import_new(inum) = dbl_id2%irank_home(inod)
      end do
!
      call added_nod_id_send_recv(new_comm%num_neib, new_comm%id_neib,  &
     &  new_comm%istack_import, new_comm%ntot_import, inod_import_new,  &
     &  new_comm%istack_export, new_comm%ntot_export, inod_export_new)
!
      call added_nod_id_send_recv(new_comm%num_neib, new_comm%id_neib,  &
     &  new_comm%istack_import, new_comm%ntot_import, irank_import_new, &
     &  new_comm%istack_export, new_comm%ntot_export, irank_export_new)
!
      call set_extended_node_export(my_rank, nod_comm, added_comm,      &
     &          inod_export_new, irank_export_new, new_comm)
!
      deallocate(inod_export_new, irank_export_new)
!
      call check_new_node_and_comm(new_comm, new_node, dbl_id2)
!
      end subroutine extend_node_comm_table
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_new_node_and_comm(new_comm, new_node, dbl_id2)
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(parallel_double_numbering), intent(in) :: dbl_id2
!
      integer(kind = kint), allocatable :: inod_lc_check(:)
      integer(kind = kint), allocatable :: irank_lc_check(:)
!
      integer(kind = kint) :: inod, icou
      integer(kind = kint) :: nerror
!
!
      allocate(inod_lc_check(new_node%numnod))
      allocate(irank_lc_check(new_node%numnod))
      inod_lc_check =   0
      irank_lc_check = -1
!
!$omp parallel do
      do inod = 1, new_node%internal_node
        inod_lc_check(inod) = inod
        irank_lc_check(inod) = my_rank
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, inod_lc_check)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, irank_lc_check)
!
      icou = 0
      do inod = new_node%internal_node+1, new_node%numnod
        if(dbl_id2%irank_home(inod) .ne. irank_lc_check(inod)           &
     &    .and. dbl_id2%inod_local(inod) .ne. inod_lc_check(inod)) then
          if(icou .eq. 0) write(50+my_rank,*) 'error list'
          write(50+my_rank,*) inod, my_rank,                            &
     &     dbl_id2%irank_home(inod), irank_lc_check(inod),              &
     &     dbl_id2%inod_local(inod), inod_lc_check(inod)
           icou = icou + 1
        end if
      end do
!
      call MPI_ALLREDUCE(icou, nerror, 1, CALYPSO_INTEGER, MPI_SUM,     &
     &    CALYPSO_COMM,ierr_MPI)
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'Number of wrong communication items:', nerror
!
      deallocate(inod_lc_check, irank_lc_check)
!
      end subroutine check_new_node_and_comm
!
!  ---------------------------------------------------------------------
!
      end module extend_comm_table

