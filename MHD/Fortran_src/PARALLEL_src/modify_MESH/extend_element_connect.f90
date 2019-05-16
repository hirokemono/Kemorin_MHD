!> @file  extend_element_connect.f90
!!      module extend_element_connect
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine extend_ele_connectivity                              &
!!     &         (nod_comm, ele_comm, org_node, ele, dbl_id1, neib_ele, &
!!     &          new_comm, new_node, new_ele)
!!@endverbatim
!
      module extend_element_connect
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
      subroutine extend_ele_connectivity                                &
     &         (nod_comm, ele_comm, org_node, ele, dbl_id1, neib_ele,   &
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
      use find_extended_node_and_ele
      use find_extended_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: ele
      type(parallel_double_numbering), intent(in) :: dbl_id1
      type(element_around_node), intent(in) :: neib_ele
!
      type(communication_table), intent(in) :: new_comm
!
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
!
!>      Structure of double numbering
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
      integer(kind = kint) :: inod, i, iele, k1
!
!
      call alloc_double_numbering(new_node%numnod, dbl_id2)
      call set_para_double_numbering                                    &
     &   (new_node%internal_node, new_comm, dbl_id2)
      call alloc_double_numbering(ele%numele, dbl_ele)
      call set_para_ele_double_numbering                                &
     &   (new_node%internal_node, ele_comm, ele, dbl_ele)
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
      call alloc_export_item(added_comm)
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
      deallocate(iflag_node, iflag_ele)
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
      call alloc_import_item(added_comm)
      call alloc_ele_buffer_2_extend                                    &
     &   (added_comm%ntot_import, ele, recv_ebuf)
!
      call added_global_id_send_recv(added_comm%num_neib,               &
     &    added_comm%id_neib, added_comm%istack_export,                 &
     &    added_comm%ntot_export, send_ebuf%iele_gl_add,                &
     &    added_comm%istack_import, added_comm%ntot_import,             &
     &    recv_ebuf%iele_gl_add)
      call added_nod_id_send_recv(added_comm%num_neib,                  &
     &    added_comm%id_neib, added_comm%istack_export,                 &
     &    added_comm%ntot_export, send_ebuf%iele_add,                   &
     &    added_comm%istack_import, added_comm%ntot_import,             &
     &    recv_ebuf%iele_add)
      call added_nod_id_send_recv(added_comm%num_neib,                  &
     &    added_comm%id_neib, added_comm%istack_export,                 &
     &    added_comm%ntot_export, send_ebuf%irank_add,                  &
     &    added_comm%istack_import, added_comm%ntot_import,             &
     &    recv_ebuf%irank_add)
      do k1 = 1, ele%nnod_4_ele
        call added_nod_id_send_recv(added_comm%num_neib,                &
     &      added_comm%id_neib, added_comm%istack_export,               &
     &      added_comm%ntot_export, send_ebuf%ie_added(1,k1),           &
     &      added_comm%istack_import, added_comm%ntot_import,           &
     &      recv_ebuf%ie_added(1,k1))
        call added_nod_id_send_recv(added_comm%num_neib,                &
     &      added_comm%id_neib, added_comm%istack_export,               &
     &      added_comm%ntot_export, send_ebuf%ip_added(1,k1),           &
     &      added_comm%istack_import, added_comm%ntot_import,           &
     &      recv_ebuf%ip_added(1,k1))
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
      call added_nod_id_send_recv(added_comm%num_neib,                  &
     &    added_comm%id_neib, added_comm%istack_import,                 &
     &    added_comm%ntot_import, added_comm%item_import,               &
     &    added_comm%istack_export, added_comm%ntot_export,             &
     &    added_comm%item_export)
!
      call count_ele_by_extend_sleeve(added_comm, ele, new_ele)
!
      call allocate_ele_connect_type(new_ele)
!
      call set_ele_by_extend_sleeve(added_comm, recv_ebuf, ele,         &
     &    new_node, dbl_id2, new_ele)
!
      call dealloc_ele_buffer_2_extend(recv_ebuf)
      call dealloc_comm_table(added_comm)
!
      call dealloc_double_numbering(dbl_id2)
      call dealloc_double_numbering(dbl_ele)
!
!      write(50+my_rank,*) 'new_ele%iele', new_ele%numele
!      do i = 1, new_ele%numele
!         write(50+my_rank,*) new_ele%iele_global(i), new_ele%ie(i,1:8)
!      end do
!
      end subroutine extend_ele_connectivity
!
!  ---------------------------------------------------------------------
!
      end module extend_element_connect

