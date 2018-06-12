!> @file  extend_comm_table.f90
!!      module extend_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!@endverbatim
!
      module extend_comm_table
!
      use m_precision
      use m_constants
      use m_phys_constants
      use m_machine_parameter
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
      subroutine extend_node_comm_table(nod_comm, org_node, neib_nod,   &
     &          new_comm, new_node)
!
      use t_next_node_ele_4_node
      use m_merged_ucd_data
      use solver_SR_type
      use extend_comm_table_SR
      use mark_export_nod_ele_extend
      use cal_minmax_and_stacks
      use find_extended_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: org_node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
!
!>      Structure of double numbering
      type(parallel_double_numbering) :: dbl_id1
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
      integer(kind = kint), allocatable :: inod_lc_check(:)
      integer(kind = kint), allocatable :: irank_lc_check(:)
!
      integer(kind = kint) :: inum, inod, i, ist, ied, icou, ip
      integer(kind = kint) :: j, nerror
!
!
      call alloc_double_numbering(org_node%numnod, dbl_id1)
      call set_para_double_numbering                                    &
     &   (org_node%internal_node, nod_comm, dbl_id1)
      call calypso_mpi_barrier
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
      call allocate_type_export_item(added_comm)
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
     &     org_node, dbl_id1, iflag_node, send_nbuf)
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
      call allocate_type_import_item(added_comm)
      call alloc_node_buffer_2_extend                                   &
     &   (added_comm%ntot_import, recv_nbuf)
!
      call added_geometry_send_recv                                     &
     &   (added_comm%num_neib, added_comm%id_neib,                          &
     &    added_comm%istack_export, added_comm%ntot_export, send_nbuf%xx_add,            &
     &    added_comm%istack_import, added_comm%ntot_import, recv_nbuf%xx_add)
      call added_global_id_send_recv                                    &
     &   (added_comm%num_neib, added_comm%id_neib,                          &
     &    added_comm%istack_export, added_comm%ntot_export, send_nbuf%inod_gl_add,       &
     &    added_comm%istack_import, added_comm%ntot_import, recv_nbuf%inod_gl_add)
      call added_nod_id_send_recv(added_comm%num_neib, added_comm%id_neib,  &
     &    added_comm%istack_export, added_comm%ntot_export, send_nbuf%inod_add,          &
     &    added_comm%istack_import, added_comm%ntot_import, recv_nbuf%inod_add)
      call added_nod_id_send_recv(added_comm%num_neib, added_comm%id_neib,  &
     &    added_comm%istack_export, added_comm%ntot_export, send_nbuf%irank_add,         &
     &    added_comm%istack_import, added_comm%ntot_import, recv_nbuf%irank_add)
!
      call mark_added_nod_import_to_del                                 &
     &   (org_node%numnod, dbl_id1%inod_local, dbl_id1%irank_home,      &
     &    nod_comm%num_neib, nod_comm%id_neib, nod_comm%ntot_import,    &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    added_comm%num_neib, added_comm%id_neib,                      &
     &    added_comm%ntot_import, added_comm%istack_import,             &
     &    recv_nbuf%inod_add, recv_nbuf%irank_add,                      &
     &    added_comm%item_import)
!
!      call check_added_impoert_items                                   &
!     &   (my_rank, nod_comm, added_comm, dbl_id1, recv_nbuf)
!
      call added_nod_id_send_recv(added_comm%num_neib, added_comm%id_neib,  &
     &    added_comm%istack_import, added_comm%ntot_import, added_comm%item_import,              &
     &    added_comm%istack_export, added_comm%ntot_export, added_comm%item_export)
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
      call set_nodes_by_extend_sleeve(added_comm, recv_nbuf,            &
     &    org_node, dbl_id1, new_node, dbl_id2)
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
      call allocate_type_comm_tbl_num(new_comm)
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
      call allocate_type_comm_tbl_item(new_comm)
!
      call set_extended_node_import                                     &
     &   (recv_nbuf, nod_comm, added_comm, new_comm)
!
      call dealloc_node_buffer_2_extend(recv_nbuf)
      call deallocate_type_comm_tbl(added_comm)
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
      call added_nod_id_send_recv(new_comm%num_neib, new_comm%id_neib,  &
     &  new_comm%istack_import, new_comm%ntot_import, irank_import_new, &
     &  new_comm%istack_export, new_comm%ntot_export, irank_export_new)
!
      call set_extended_node_export(nod_comm, added_comm,               &
     &          inod_export_new, irank_export_new, new_comm)
!
      deallocate(inod_export_new, irank_export_new)
!
      allocate(inod_lc_check(new_node%numnod))
      allocate(irank_lc_check(new_node%numnod))
      inod_lc_check =   0
      irank_lc_check = -1
!
      do inod = 1, new_node%internal_node
        inod_lc_check(inod) = inod
        irank_lc_check(inod) = my_rank
      end do
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
      call MPI_ALLREDUCE(icou, nerror, ione, CALYPSO_INTEGER, MPI_SUM, &
     &    CALYPSO_COMM,ierr_MPI)
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'Number of wrong communication items:', nerror
      call calypso_mpi_barrier
!
!      write(*,*) 'num_neib', my_rank,                                  &
!     &       nod_comm%num_neib, new_comm%num_neib
!
      end subroutine extend_node_comm_table
!
!  ---------------------------------------------------------------------
!
      subroutine extend_ele_comm_table                                  &
     &         (nod_comm, ele_comm, org_node, ele, neib_ele,            &
     &          new_comm, new_node, new_ele)
!
      use t_next_node_ele_4_node
      use m_merged_ucd_data
      use solver_SR_type
      use extend_comm_table_SR
      use mark_export_nod_ele_extend
      use const_mesh_information
      use const_element_comm_tables
      use cal_minmax_and_stacks
      use find_extended_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
!
      type(communication_table), intent(in) :: new_comm
!
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
!
!>      Structure of double numbering
      type(parallel_double_numbering) :: dbl_id1
      type(parallel_double_numbering) :: dbl_id2
      type(parallel_double_numbering) :: dbl_ele
!
      type(communication_table) :: added_comm
      type(ele_buffer_2_extend) :: send_ebuf
      type(ele_buffer_2_extend) :: recv_ebuf
!
      integer(kind = kint), allocatable :: iflag_node(:)
      integer(kind = kint), allocatable :: iflag_ele(:)
!
      integer(kind = kint) :: inum, inod, i, ist, ied, icou
!
      integer(kind = kint) :: iele, k1, ip
!
!
      call alloc_double_numbering(new_node%numnod, dbl_id1)
      call set_para_double_numbering                                    &
     &   (new_node%internal_node, nod_comm, dbl_id1)
      call alloc_double_numbering(new_node%numnod, dbl_id2)
      call set_para_double_numbering                                    &
     &   (new_node%internal_node, new_comm, dbl_id2)
      call alloc_double_numbering(ele%numele, dbl_ele)
      call set_para_ele_double_numbering                                &
     &   (new_node%internal_node, ele_comm, ele, dbl_ele)
      call calypso_mpi_barrier
!
      allocate(iflag_node(org_node%numnod))
      allocate(iflag_ele(ele%numele))
      iflag_node(1:org_node%numnod) = 0
      iflag_ele(1:ele%numele) = 0
!
      call alloc_added_comm_table_num(nod_comm, added_comm)
!
      do i = 1, nod_comm%num_neib
        call mark_used_ele_of_export(i, nod_comm%num_neib,              &
     &      new_comm%istack_export, new_comm%item_export,               &
     &      nod_comm%istack_export, nod_comm%item_export,               &
     &      org_node%numnod, neib_ele%ntot, neib_ele%istack_4_node,     &
     &      neib_ele%iele_4_node, ele%numele, ele%nnod_4_ele, ele%ie,   &
     &      iflag_node, iflag_ele)
!
        do inod = 1, ele%numele
          added_comm%num_export(i) = added_comm%num_export(i)           &
     &                              + iflag_ele(inod)
        end do
      end do
!
      call s_cal_total_and_stacks                                       &
     &   (added_comm%num_neib, added_comm%num_export, izero,            &
     &    added_comm%istack_export, added_comm%ntot_export)
!
!      write(*,*) 'istack_send_added ele', added_comm%istack_export
!
      call allocate_type_export_item(added_comm)
      call alloc_ele_buffer_2_extend                                    &
     &   (added_comm%ntot_export, ele, send_ebuf)
!
      do i = 1, nod_comm%num_neib
        call mark_used_ele_of_export(i, nod_comm%num_neib,              &
     &      new_comm%istack_export, new_comm%item_export,               &
     &      nod_comm%istack_export, nod_comm%item_export,               &
     &      org_node%numnod, neib_ele%ntot, neib_ele%istack_4_node,     &
     &      neib_ele%iele_4_node, ele%numele, ele%nnod_4_ele, ele%ie,   &
     &      iflag_node, iflag_ele)
!
        call copy_ele_to_extend_buffer(added_comm%istack_export(i-1),   &
     &      ele, dbl_ele, dbl_id1, iflag_ele, send_ebuf)
      end do
!
!      call check_ie_send_added                                         &
!     &   (my_rank, added_comm, ele, send_ebuf)
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
      call allocate_type_import_item(added_comm)
      call alloc_ele_buffer_2_extend                                    &
     &   (added_comm%ntot_import, ele, recv_ebuf)
!
      call added_global_id_send_recv                                    &
     &   (added_comm%num_neib, added_comm%id_neib,                          &
     &    added_comm%istack_export, added_comm%ntot_export, send_ebuf%iele_gl_add,       &
     &    added_comm%istack_import, added_comm%ntot_import, recv_ebuf%iele_gl_add)
      call added_nod_id_send_recv(added_comm%num_neib, added_comm%id_neib,  &
     &    added_comm%istack_export, added_comm%ntot_export, send_ebuf%iele_add,          &
     &    added_comm%istack_import, added_comm%ntot_import, recv_ebuf%iele_add)
      call added_nod_id_send_recv(added_comm%num_neib, added_comm%id_neib,  &
     &    added_comm%istack_export, added_comm%ntot_export, send_ebuf%irank_add,         &
     &    added_comm%istack_import, added_comm%ntot_import, recv_ebuf%irank_add)
      do k1 = 1, ele%nnod_4_ele
        call added_nod_id_send_recv                                     &
     &     (added_comm%num_neib, added_comm%id_neib,                        &
     &      added_comm%istack_export, added_comm%ntot_export, send_ebuf%ie_added(1,k1),    &
     &      added_comm%istack_import, added_comm%ntot_import, recv_ebuf%ie_added(1,k1))
        call added_nod_id_send_recv                                     &
     &     (added_comm%num_neib, added_comm%id_neib,                        &
     &      added_comm%istack_export, added_comm%ntot_export, send_ebuf%ip_added(1,k1),    &
     &      added_comm%istack_import, added_comm%ntot_import,  recv_ebuf%ip_added(1,k1))
      end do
!
!      call check_element_list_to_add                                   &
!     &   (my_rank, added_comm, ele, send_ebuf, recv_ebuf)
!
      call dealloc_ele_buffer_2_extend(send_ebuf)
!
      call mark_added_ele_import_to_del                                 &
     &   (nprocs, ele%numele, dbl_ele%inod_local, dbl_ele%irank_home,   &
     &    added_comm%num_neib, added_comm%ntot_import,                  &
     &    added_comm%istack_import,                                     &
     &    recv_ebuf%iele_add, recv_ebuf%irank_add,                      &
     &    added_comm%item_import)
!
      call added_nod_id_send_recv(added_comm%num_neib, added_comm%id_neib,  &
     &    added_comm%istack_import, added_comm%ntot_import, added_comm%item_import,  &
     &    added_comm%istack_export, added_comm%ntot_export, added_comm%item_export)
!
      call count_ele_by_extend_sleeve(added_comm, ele, new_ele)
!
      call allocate_ele_connect_type(new_ele)
!
      call set_ele_by_extend_sleeve(added_comm, recv_ebuf, ele,         &
     &    new_node, dbl_id2, new_ele)
!
      call dealloc_ele_buffer_2_extend(recv_ebuf)
      call deallocate_type_comm_tbl(added_comm)
!
!      write(50+my_rank,*) 'new_ele%iele', new_ele%numele
!      do i = 1, new_ele%numele
!         write(50+my_rank,*) new_ele%iele_global(i), new_ele%ie(i,1:8)
!      end do
!
      call calypso_mpi_barrier
!
      end subroutine extend_ele_comm_table
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_nodes_by_extend_sleeve                           &
     &         (added_comm, org_node, new_node)
!
      type(communication_table), intent(in) :: added_comm
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inum
!
!
      new_node%internal_node = org_node%internal_node
      new_node%numnod = org_node%numnod
      do inum = 1, added_comm%ntot_import
        if(added_comm%item_import(inum) .eq. 0) then
          new_node%numnod = new_node%numnod + 1
        end if
      end do
!
      end subroutine count_nodes_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodes_by_extend_sleeve(added_comm, recv_nbuf,      &
     &          org_node, dbl_id1, new_node, dbl_id2)
!
      type(communication_table), intent(in) :: added_comm
      type(node_data), intent(in) :: org_node
      type(parallel_double_numbering), intent(in) :: dbl_id1
      type(node_buffer_2_extend), intent(in) :: recv_nbuf
!
      type(node_data), intent(inout) :: new_node
      type(parallel_double_numbering), intent(inout) :: dbl_id2
!
      integer(kind = kint) :: inum, inod, icou
!
!
!$omp parallel do
      do inod = 1, org_node%numnod
        new_node%inod_global(inod) = org_node%inod_global(inod)
        new_node%xx(inod,1) = org_node%xx(inod,1)
        new_node%xx(inod,2) = org_node%xx(inod,2)
        new_node%xx(inod,3) = org_node%xx(inod,3)
        dbl_id2%inod_local(inod) = dbl_id1%inod_local(inod)
        dbl_id2%irank_home(inod) = dbl_id1%irank_home(inod)
      end do
!$omp end parallel do
!
      icou = org_node%numnod
      do inum = 1, added_comm%ntot_import
        if(added_comm%item_import(inum) .eq. 0) then
          icou = icou + 1
          added_comm%item_import(inum) = icou
          new_node%inod_global(icou) = recv_nbuf%inod_gl_add(inum)
          new_node%xx(icou,1) = recv_nbuf%xx_add(inum,1)
          new_node%xx(icou,2) = recv_nbuf%xx_add(inum,2)
          new_node%xx(icou,3) = recv_nbuf%xx_add(inum,3)
          dbl_id2%inod_local(icou) = recv_nbuf%inod_add(inum)
          dbl_id2%irank_home(icou) = recv_nbuf%irank_add(inum)
        end if
      end do
!
      end subroutine set_nodes_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!
      subroutine check_nodes_by_extend_sleeve                           &
     &         (org_node, new_node, dbl_id2)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(in) :: new_node
      type(parallel_double_numbering), intent(in) :: dbl_id2
!
      integer(kind = kint) :: inod
!
!
      write(100+my_rank,*) new_node%numnod,                             &
     &             new_node%internal_node, org_node%numnod
      do inod = 1, new_node%numnod
        write(100+my_rank,*) inod, dbl_id2%inod_local(inod),            &
     &         dbl_id2%irank_home(inod), new_node%inod_global(inod) 
      end do
!
      end subroutine check_nodes_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!
      subroutine count_ele_by_extend_sleeve(added_comm, ele, new_ele)
!
      type(communication_table), intent(in) :: added_comm
      type(element_data), intent(in) :: ele
!
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: inum
!
!
      new_ele%numele =     ele%numele
      new_ele%nnod_4_ele = ele%nnod_4_ele
      new_ele%internal_ele = ele%internal_ele
      do inum = 1, added_comm%ntot_import
        if(added_comm%item_import(inum) .eq. 0) then
          new_ele%numele = new_ele%numele + 1
        end if
      end do
!
      end subroutine count_ele_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_by_extend_sleeve(added_comm, recv_ebuf,        &
     &          ele, new_node, dbl_id2, new_ele)
!
      type(communication_table), intent(in) :: added_comm
      type(ele_buffer_2_extend), intent(in) :: recv_ebuf
      type(element_data), intent(in) :: ele
      type(node_data), intent(in) :: new_node
      type(parallel_double_numbering), intent(in) :: dbl_id2
!
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: inum, icou, ist, ied, i
      integer(kind = kint) :: k1, jnod
!
!
!$omp parallel workshare
      new_ele%elmtyp(1:ele%numele) = ele%elmtyp(1:ele%numele)
      new_ele%nodelm(1:ele%numele) = ele%nodelm(1:ele%numele)
      new_ele%iele_global(1:ele%numele) = ele%iele_global(1:ele%numele)
!$omp end parallel workshare
!$omp parallel workshare
      new_ele%elmtyp(ele%numele+1:new_ele%numele) = ele%elmtyp(1)
      new_ele%nodelm(ele%numele+1:new_ele%numele) = ele%nodelm(1)
!$omp end parallel workshare
!
!$omp parallel
      do k1 = 1, ele%nnod_4_ele
!$omp workshare
        new_ele%ie(1:ele%numele,k1) = ele%ie(1:ele%numele,k1)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      icou = ele%numele
      do i = 1, added_comm%num_neib
        ist = added_comm%istack_import(i-1) + 1
        ied = added_comm%istack_import(i)
        do inum = ist, ied
          if(added_comm%item_import(inum) .lt. 0)  cycle
!
          icou = icou + 1
          new_ele%iele_global(icou) = recv_ebuf%iele_gl_add(inum)
          do k1 = 1, ele%nnod_4_ele
            do jnod = new_node%numnod, 1, -1
              if(recv_ebuf%ip_added(inum,k1)                            &
     &              .eq. dbl_id2%irank_home(jnod)                       &
     &          .and. recv_ebuf%ie_added(inum,k1)                       &
     &              .eq. dbl_id2%inod_local(jnod)) then
                new_ele%ie(icou,k1) = jnod
                exit
              end if
            end do
          end do
        end do
      end do
!
      end subroutine set_ele_by_extend_sleeve
!
!  ---------------------------------------------------------------------
!
      end module extend_comm_table

