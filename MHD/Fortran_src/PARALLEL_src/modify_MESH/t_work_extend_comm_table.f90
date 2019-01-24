!> @file  t_work_extend_comm_table.f90
!!      module t_work_extend_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Work structure for sleeve extender
!!
!!@verbatim
!!      subroutine alloc_added_comm_table_num(nod_comm, added_comm)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: added_comm
!!      subroutine alloc_node_buffer_2_extend(ntot_item, node_buf)
!!        type(node_buffer_2_extend), intent(inout) :: node_buf
!!      subroutine alloc_ele_buffer_2_extend(ntot_item, ele, ele_buf)
!!        type(element_data), intent(in) :: ele
!!        type(ele_buffer_2_extend), intent(inout) :: ele_buf
!!
!!      subroutine dealloc_node_buffer_2_extend(node_buf)
!!        type(node_buffer_2_extend), intent(inout) :: node_buf
!!      subroutine dealloc_ele_buffer_2_extend(ele_buf)
!!        type(ele_buffer_2_extend), intent(inout) :: ele_buf
!!
!!      subroutine check_num_of_added_table(my_rank, added_comm)
!!        type(communication_table), intent(in) :: added_comm
!!      subroutine check_added_impoert_items                            &
!!     &         (my_rank, nod_comm, added_comm, dbl_id1, recv_nbuf)
!!        type(communication_table), intent(in) :: nod_comm, added_comm
!!        type(parallel_double_numbering), intent(in) :: dbl_id1
!!        type(node_buffer_2_extend), intent(in) :: recv_nbuf
!!      subroutine check_delete_from_SR_list                            &
!!     &         (my_rank, added_comm, send_nbuf, recv_nbuf)
!!        type(communication_table), intent(in) :: added_comm
!!        type(node_buffer_2_extend), intent(in) :: send_nbuf, recv_nbuf
!!      subroutine check_ie_send_added                                  &
!!     &         (my_rank, added_comm, ele, send_ebuf)
!!        type(communication_table), intent(in) ::  added_comm
!!        type(element_data), intent(in) :: ele
!!        type(ele_buffer_2_extend), intent(in) :: send_ebuf
!!      subroutine check_element_list_to_add                            &
!!     &         (my_rank, added_comm, ele, send_ebuf, recv_ebuf)
!!        type(communication_table), intent(in) ::  added_comm
!!        type(element_data), intent(in) :: ele
!!        type(ele_buffer_2_extend), intent(in) :: send_ebuf
!!        type(ele_buffer_2_extend), intent(in) :: recv_ebuf
!!@endverbatim
!
      module t_work_extend_comm_table
!
      use m_precision
      use m_constants
      use m_phys_constants
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
!
      implicit none
!
      type node_buffer_2_extend
        integer(kind = kint) :: ntot
        integer(kind = kint), allocatable :: inod_add(:)
        integer(kind = kint), allocatable :: irank_add(:)
        integer(kind = kint_gl), allocatable :: inod_gl_add(:)
        real(kind = kreal), allocatable :: xx_add(:,:)
      end type node_buffer_2_extend
!
      type ele_buffer_2_extend
        integer(kind = kint) :: ntot
        integer(kind = kint) :: nnod_4_ele
        integer(kind = kint), allocatable :: iele_add(:)
        integer(kind = kint), allocatable :: irank_add(:)
        integer(kind = kint), allocatable :: iele_lc(:)
        integer(kind = kint_gl), allocatable :: iele_gl_add(:)
        integer(kind = kint), allocatable :: ie_added(:,:)
        integer(kind = kint), allocatable :: ip_added(:,:)
      end type ele_buffer_2_extend
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_added_comm_table_num(nod_comm, added_comm)
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(inout) :: added_comm
!
      added_comm%num_neib = nod_comm%num_neib
      call alloc_comm_table_num(added_comm)
!
      if(added_comm%num_neib .gt. 0) then
!$omp parallel workshare
        added_comm%id_neib(1:nod_comm%num_neib)                         &
     &      = nod_comm%id_neib(1:nod_comm%num_neib)
!$omp end parallel workshare
      end if
!
      end subroutine alloc_added_comm_table_num
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_node_buffer_2_extend(ntot_item, node_buf)
!
      integer(kind = kint), intent(in) :: ntot_item
      type(node_buffer_2_extend), intent(inout) :: node_buf
!
!
      node_buf%ntot = ntot_item
      allocate(node_buf%inod_add(node_buf%ntot))
      allocate(node_buf%irank_add(node_buf%ntot))
      allocate(node_buf%xx_add(node_buf%ntot,3))
      allocate(node_buf%inod_gl_add(node_buf%ntot))
!
      if(node_buf%ntot .le. 0) return
!$omp parallel workshare
      node_buf%xx_add(1:node_buf%ntot,1) = 0.0d0
      node_buf%xx_add(1:node_buf%ntot,2) = 0.0d0
      node_buf%xx_add(1:node_buf%ntot,3) = 0.0d0
      node_buf%inod_add(1:node_buf%ntot) =     0
      node_buf%irank_add(1:node_buf%ntot) =    0
      node_buf%inod_gl_add(1:node_buf%ntot) =  0
!$omp end parallel workshare
!
      end subroutine alloc_node_buffer_2_extend
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ele_buffer_2_extend(ntot_item, ele, ele_buf)
!
      integer(kind = kint), intent(in) :: ntot_item
      type(element_data), intent(in) :: ele
      type(ele_buffer_2_extend), intent(inout) :: ele_buf
!
!
      ele_buf%ntot = ntot_item
      ele_buf%nnod_4_ele = ele%nnod_4_ele
      allocate(ele_buf%iele_add(ele_buf%ntot))
      allocate(ele_buf%irank_add(ele_buf%ntot))
      allocate(ele_buf%iele_gl_add(ele_buf%ntot))
      allocate(ele_buf%iele_lc(ele_buf%ntot))
      allocate(ele_buf%ie_added(ele_buf%ntot,ele_buf%nnod_4_ele))
      allocate(ele_buf%ip_added(ele_buf%ntot,ele_buf%nnod_4_ele))
!
      if(ele_buf%ntot .le. 0) return
!$omp parallel workshare
      ele_buf%iele_add(1:ele_buf%ntot) =     0
      ele_buf%irank_add(1:ele_buf%ntot) =    0
      ele_buf%iele_gl_add(1:ele_buf%ntot) =  0
      ele_buf%iele_lc(1:ele_buf%ntot) =      0
!$omp end parallel workshare
!$omp parallel workshare
      ele_buf%ie_added(1:ele_buf%ntot,1:ele_buf%nnod_4_ele) = 0
      ele_buf%ip_added(1:ele_buf%ntot,1:ele_buf%nnod_4_ele) = 0
!$omp end parallel workshare
!
      end subroutine alloc_ele_buffer_2_extend
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_node_buffer_2_extend(node_buf)
!
      type(node_buffer_2_extend), intent(inout) :: node_buf
!
!
      deallocate(node_buf%inod_add)
      deallocate(node_buf%irank_add)
      deallocate(node_buf%xx_add)
      deallocate(node_buf%inod_gl_add)
!
      end subroutine dealloc_node_buffer_2_extend
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ele_buffer_2_extend(ele_buf)
!
      type(ele_buffer_2_extend), intent(inout) :: ele_buf
!
!
      deallocate(ele_buf%iele_add)
      deallocate(ele_buf%irank_add)
      deallocate(ele_buf%iele_gl_add)
      deallocate(ele_buf%iele_lc)
      deallocate(ele_buf%ie_added)
      deallocate(ele_buf%ip_added)
!
      end subroutine dealloc_ele_buffer_2_extend
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_num_of_added_table(my_rank, added_comm)
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) :: added_comm
!
!
      write(*,*) 'istack_send_added', my_rank, added_comm%istack_export
      write(*,*) 'ntot_send_added', my_rank, added_comm%ntot_export
      write(*,*) 'istack_recv_added', my_rank, added_comm%istack_import
      write(*,*) 'ntot_recv_added', my_rank, added_comm%ntot_import
!
      end subroutine check_num_of_added_table
!
!  ---------------------------------------------------------------------
!
      subroutine check_added_impoert_items                              &
     &         (my_rank, nod_comm, added_comm, dbl_id1, recv_nbuf)
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) :: nod_comm, added_comm
      type(parallel_double_numbering), intent(in) :: dbl_id1
      type(node_buffer_2_extend), intent(in) :: recv_nbuf
!
      integer(kind = kint) :: inum, inod, i, ist, ied
!
!
      do i = 1, nod_comm%num_neib
        ist = nod_comm%istack_import(i-1) + 1
        ied = nod_comm%istack_import(i)
        write(120+my_rank,*) 'import', nod_comm%id_neib(i), ist, ied
!
        do inum = ist, ied
          inod = nod_comm%item_import(inum)
          write(120+my_rank,*) inum, inod,                              &
     &        dbl_id1%irank_home(inod), dbl_id1%inod_local(inod), '  '
        end do
      end do
      do i = 1, nod_comm%num_neib
        ist = added_comm%istack_import(i-1) + 1
        ied = added_comm%istack_import(i)
        write(120+my_rank,*) 'added_comm%istack_import',                &
     &                        nod_comm%id_neib(i), ist, ied
!
        do inum = ist, ied
          write(120+my_rank,*) inum, recv_nbuf%irank_add(inum),         &
     &         recv_nbuf%inod_add(inum), added_comm%item_import(inum)
        end do
      end do
!
      end subroutine check_added_impoert_items
!
!  ---------------------------------------------------------------------
!
      subroutine check_delete_from_SR_list                              &
     &         (my_rank, added_comm, send_nbuf, recv_nbuf)
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) :: added_comm
      type(node_buffer_2_extend), intent(in) :: send_nbuf, recv_nbuf
!
      integer(kind = kint) :: inum
!
!
      do inum = 1, added_comm%ntot_import
        if(added_comm%item_import(inum) .lt. 0) write(*,*)              &
     &      'recv delete', my_rank, inum,                               &
     &       recv_nbuf%irank_add(inum), recv_nbuf%inod_add(inum)
      end do
      do inum = 1, added_comm%ntot_export
        if(added_comm%item_export(inum) .lt. 0) write(*,*)              &
     &      'send delete', my_rank, inum,                               &
     &       send_nbuf%irank_add(inum), send_nbuf%inod_add(inum)
      end do
!
      end subroutine check_delete_from_SR_list
!
!  ---------------------------------------------------------------------
!
      subroutine check_ie_send_added                                    &
     &         (my_rank, added_comm, ele, send_ebuf)
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) ::  added_comm
      type(element_data), intent(in) :: ele
      type(ele_buffer_2_extend), intent(in) :: send_ebuf
!!
      integer(kind = kint) :: inum, i, ist, ied
!
!
      do i = 1, added_comm%num_neib
        ist = added_comm%istack_export(i-1) + 1
        ied = added_comm%istack_export(i)
        write(50+my_rank,*) 'added_comm%istack_export',                 &
     &                      i, added_comm%id_neib(i), ist, ied
        do inum = ist, ied
          if(send_ebuf%irank_add(inum) .eq. added_comm%id_neib(i)) then
              write(50+my_rank,*) inum, send_ebuf%iele_lc(inum),        &
     &         send_ebuf%ie_added(inum,1:ele%nnod_4_ele)
              write(50+my_rank,*) inum, send_ebuf%irank_add(inum),      &
     &         send_ebuf%ip_added(inum,1:ele%nnod_4_ele)
          end if
        end do
      end do
!
      end subroutine check_ie_send_added
!
!  ---------------------------------------------------------------------
!
      subroutine check_element_list_to_add                              &
     &         (my_rank, added_comm, ele, send_ebuf, recv_ebuf)
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) ::  added_comm
      type(element_data), intent(in) :: ele
      type(ele_buffer_2_extend), intent(in) :: send_ebuf
      type(ele_buffer_2_extend), intent(in) :: recv_ebuf
!
      integer(kind = kint) :: inum, i, ist, ied
!
!
      do i = 1, added_comm%num_neib
        ist = added_comm%istack_import(i-1) + 1
        ied = added_comm%istack_import(i)
        write(50+my_rank,*) 'istack_recv_added',  &
     &                      i, added_comm%id_neib(i), ist, ied
        do inum = ist, ied
          if(send_ebuf%irank_add(inum) .eq. added_comm%id_neib(i)) then
              write(50+my_rank,*) inum,                                 &
     &          recv_ebuf%ie_added(inum,1:ele%nnod_4_ele)
              write(50+my_rank,*) inum,                                 &
     &          recv_ebuf%ip_added(inum,1:ele%nnod_4_ele)
          end if
        end do
      end do
!
      end subroutine check_element_list_to_add
!
!  ---------------------------------------------------------------------
!
      end module t_work_extend_comm_table

