!>@file   check_data_for_repartition.f90
!!@brief  module check_data_for_repartition
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine check_repart_node_transfer(nod_comm, node,           &
!!     &          new_comm, new_node, part_tbl, new_ids_on_org)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(in) :: new_node
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(node_ele_double_number), intent(in) :: new_ids_on_org
!!      subroutine check_num_of_neighbourings                           &
!!     &         (new_comm, ext_tbl, num_recv_tmp2)
!!        type(communication_table), intent(in) :: new_comm
!!        type(calypso_comm_table), intent(in) :: ext_tbl
!!      subroutine check_new_node_comm_table(my_rank, new_comm)
!!        type(communication_table), intent(in) :: new_comm
!!
!!      subroutine compare_calypso_comm_tbls(ref_tbl, cmp_tbl)
!!        type(calypso_comm_table), intent(in) :: ref_tbl, cmp_tbl
!!@endverbatim
!
      module check_data_for_repartition
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_calypso_comm_table
      use t_para_double_numbering
      use t_repart_double_numberings
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_repart_node_transfer(nod_comm, node,             &
     &          new_comm, new_node, part_tbl, new_ids_on_org)
!
      use m_solver_SR
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(calypso_comm_table), intent(in) :: part_tbl
      type(node_ele_double_number), intent(in) :: new_ids_on_org
!
      integer(kind = kint), allocatable :: inod_old_lc(:)
      integer(kind = kint), allocatable :: irank_old_lc(:)
      integer(kind = kint), allocatable :: inod_new_lc(:)
      integer(kind = kint), allocatable :: irank_new_lc(:)
!
      type(node_ele_double_number) :: new_recved_id1
      type(node_ele_double_number) :: new_recved_id2
!
      integer(kind = kint) :: inod
!
!
      allocate(inod_old_lc(node%numnod))
      allocate(irank_old_lc(node%numnod))
!
      do inod = 1, node%internal_node
        inod_old_lc(inod) =  inod
        irank_old_lc(inod) = my_rank
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, SR_sig1, SR_i1, irank_old_lc)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, SR_sig1, SR_i1, inod_old_lc)
!

      allocate(inod_new_lc(new_node%numnod))
      allocate(irank_new_lc(new_node%numnod))
      call alloc_double_numbering(new_node%numnod, new_recved_id1)
      call alloc_double_numbering(new_node%numnod, new_recved_id2)
!
      do inod = 1, new_node%internal_node
        inod_new_lc(inod) =  inod
        irank_new_lc(inod) = my_rank
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, SR_sig1, SR_i1, irank_new_lc)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, SR_sig1, SR_i1, inod_new_lc)
!
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    new_ids_on_org%index(1), new_recved_id1%index(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    new_ids_on_org%irank(1), new_recved_id1%irank(1))
!
      call SOLVER_SEND_RECV_int_type(new_node%numnod, new_comm,         &
     &    SR_sig1, SR_i1, new_recved_id1%index)
      call SOLVER_SEND_RECV_int_type(new_node%numnod, new_comm,         &
     &    SR_sig1, SR_i1, new_recved_id1%irank)
!
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    new_ids_on_org%index(1), new_recved_id2%index(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    new_ids_on_org%irank(1), new_recved_id2%irank(1))
      call SOLVER_SEND_RECV_int_type(new_node%numnod, new_comm,         &
     &    SR_sig1, SR_i1, new_recved_id2%index)
      call SOLVER_SEND_RECV_int_type(new_node%numnod, new_comm,         &
     &    SR_sig1, SR_i1, new_recved_id2%irank)
!
!      write(*,*) 'Check new_recved_id2%irank, new_recved_id2%index'
      do inod = 1, new_node%internal_node
        if(new_recved_id1%index(inod) .ne. inod_new_lc(inod)            &
     &    .or. new_recved_id1%irank(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong new_recved_id1%index!' , inod
        end if
        if(new_recved_id2%index(inod) .ne. inod_new_lc(inod)            &
     &    .or. new_recved_id2%irank(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong new_recved_id2!' , inod
        end if
      end do
!
      do inod = new_node%internal_node+1, new_node%numnod
        if(new_recved_id1%index(inod) .ne. inod_new_lc(inod)            &
     &    .or. new_recved_id1%irank(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong new_recved_id1%index!' , inod
        end if
        if(new_recved_id2%index(inod) .ne. inod_new_lc(inod)            &
     &    .or. new_recved_id2%irank(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong new_recved_id2!' , inod
        end if
      end do
      deallocate(irank_old_lc, inod_old_lc)
      deallocate(irank_new_lc, inod_new_lc)
      call dealloc_double_numbering(new_recved_id1)
      call dealloc_double_numbering(new_recved_id2)
!
      end subroutine check_repart_node_transfer
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_num_of_neighbourings                             &
     &         (new_comm, ext_tbl, num_recv_tmp2)
!
      use calypso_mpi_int
!
      type(communication_table), intent(in) :: new_comm
      type(calypso_comm_table), intent(in) :: ext_tbl
!
      integer(kind = kint), intent(in) :: num_recv_tmp2(nprocs)
!
      integer(kind = kint), allocatable :: num_send_3(:)
      integer(kind = kint), allocatable :: num_recv_3(:)
!
      integer(kind = kint) :: i, ip
!
!
      write(100+my_rank,*) my_rank, 'num_recv_tmp2(i)'
      do i = 1, new_comm%num_neib
        ip = new_comm%id_neib(i)
        write(100+my_rank,*) i, new_comm%num_import(i),                 &
     &                      num_recv_tmp2(ip+1)
      end do
!
      allocate(num_send_3(nprocs))
      allocate(num_recv_3(nprocs))
      num_recv_3(1:nprocs) = 0
      do i = 1, ext_tbl%nrank_import
        ip = ext_tbl%irank_import(i)
        num_recv_3(ip+1) = ext_tbl%istack_import(i)                     &
     &                    - ext_tbl%istack_import(i-1)
      end do
      call calypso_mpi_alltoall_one_int(num_recv_3, num_send_3)
!
      write(100+my_rank,*) my_rank, 'num_recv_3(i), num_send_3(i)'
      do i = 1, nprocs
        write(100+my_rank,*) i-1, num_recv_3(i), num_send_3(i)
        if(num_recv_3(i) .gt. 0 .and. num_send_3(i) .eq. 0) then
          write(*,*) 'something wrong', my_rank, i-1
        end if
        if(num_recv_3(i) .eq. 0 .and. num_send_3(i) .gt. 0) then
          write(*,*) 'something wrong', my_rank, i-1
        end if
      end do
      deallocate(num_recv_3, num_send_3)
!
      end subroutine check_num_of_neighbourings
!
! ----------------------------------------------------------------------
!
      subroutine check_new_node_comm_table(my_rank, new_comm)
!
      integer, intent(in) :: my_rank
      type(communication_table), intent(in) :: new_comm
!
      integer(kind = kint) :: i, icou, ist, ied
!
!
      write(my_rank+100,*) 'i, new_comm',                               &
     &    new_comm%num_neib, new_comm%ntot_import
      do icou = 1, new_comm%num_neib
        ist = new_comm%istack_export(icou-1) + 1
        ied = new_comm%istack_export(icou)
        write(my_rank+100,*) 'i, new_comm%istack_export(icou)', &
     &      new_comm%id_neib(icou), new_comm%istack_export(icou)
        do i = ist, ied
          write(my_rank+100,*) i, new_comm%item_export(i)
        end do
      end do
!
      end subroutine check_new_node_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine compare_calypso_comm_tbls(ref_tbl, cmp_tbl)
!
      type(calypso_comm_table), intent(in) :: ref_tbl, cmp_tbl
!
      integer(kind =kint) :: i
!
      if(cmp_tbl%iflag_self_copy .ne. ref_tbl%iflag_self_copy)        &
     &     write(*,*) 'iflag_self_copy is wrong', my_rank
      if(cmp_tbl%nrank_import .ne. ref_tbl%nrank_import)              &
     &     write(*,*) 'nrank_import is wrong', my_rank
      if(cmp_tbl%ntot_import .ne. ref_tbl%ntot_import)                &
     &     write(*,*) 'ntot_import is wrong', my_rank
!
      do i = 1, cmp_tbl%nrank_import
        if(cmp_tbl%irank_import(i) .ne.ref_tbl%irank_import(i))       &
     &     write(*,*) 'irank_import is wrong', my_rank, i
      end do
      do i = 0, cmp_tbl%nrank_import
        if(cmp_tbl%istack_import(i) .ne.ref_tbl%istack_import(i))     &
     &     write(*,*) 'istack_import is wrong', my_rank, i
      end do
      do i = 1, cmp_tbl%ntot_import
        if(cmp_tbl%item_import(i) .ne. ref_tbl%item_import(i))        &
     &     write(*,*) 'item_import is wrong', my_rank, i
      end do
      do i = 1, cmp_tbl%ntot_import
        if(cmp_tbl%irev_import(i) .ne. ref_tbl%irev_import(i))        &
     &     write(*,*) 'irev_import is wrong', my_rank, i
      end do
!
      if(cmp_tbl%nrank_export .ne. ref_tbl%nrank_export)              &
     &     write(*,*) 'nrank_export is wrong', my_rank
      if(cmp_tbl%ntot_export .ne. ref_tbl%ntot_export)                &
     &     write(*,*) 'ntot_export is wrong', my_rank
      do i = 1, cmp_tbl%nrank_export
        if(cmp_tbl%irank_export(i) .ne. ref_tbl%irank_export(i))      &
     &     write(*,*) 'irank_export is wrong', my_rank
      end do
      do i = 0, cmp_tbl%nrank_export
        if(cmp_tbl%istack_export(i) .ne. ref_tbl%istack_export(i))    &
     &     write(*,*) 'istack_export is wrong', my_rank, i
      end do
!
      do i = 1, cmp_tbl%ntot_export
        if(cmp_tbl%item_export(i) .ne. ref_tbl%item_export(i))       &
     &     write(*,*) 'item_export is wrong', my_rank, i
      end do
!
      end subroutine compare_calypso_comm_tbls
!
!------------------------------------------------------------------
!
      end module check_data_for_repartition
