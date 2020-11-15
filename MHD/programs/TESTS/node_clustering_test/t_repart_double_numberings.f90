!>@file   t_repart_double_numberings.f90
!!@brief  module t_repart_double_numberings
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine alloc_double_numbering_data(n_point, local_ids)
!!      subroutine dealloc_double_numbering_data(local_ids)
!!        integer(kind = kint), intent(in) :: n_point
!!        type(calypso_comm_table), intent(inout) :: local_ids
!!      subroutine node_dbl_numbering_to_repart                         &
!!     &         (nod_comm, node, part_tbl, new_ids_on_org)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(double_numbering_data), intent(inout) :: new_ids_on_org
!!      subroutine ext_node_dbl_numbering_by_SR(node, ext_tbl,          &
!!     &          new_ids_on_org, internal_node, recieved_nod_ids)
!!        type(node_data), intent(in) :: node
!!        type(calypso_comm_table), intent(in) :: ext_tbl
!!        type(double_numbering_data), intent(in) :: new_ids_on_org
!!        type(double_numbering_data), intent(inout) :: recieved_nod_ids
!!      subroutine double_numbering_4_element(ele, ele_comm, ele_ids)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: ele_comm
!!        type(double_numbering_data), intent(inout) :: ele_ids
!!
!!      subroutine calypso_rev_SR_type_int                              &
!!     &         (cps_tbl, nnod_new, nnod_org, iX_new, iX_org)
!!        type(calypso_comm_table), intent(in) :: cps_tbl
!!
!!      subroutine check_repart_node_transfer(nod_comm, node,           &
!!     &          new_comm, new_node, part_tbl, new_ids_on_org)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(in) :: new_node
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(double_numbering_data), intent(in) :: new_ids_on_org
!!@endverbatim
!
      module t_repart_double_numberings
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
!
      implicit none
!
      type double_numbering_data
        integer(kind = kint) :: n_point
        integer(kind = kint), allocatable :: index(:)
        integer(kind = kint), allocatable :: irank(:)
      end type double_numbering_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_double_numbering_data(n_point, local_ids)
!
      integer(kind = kint), intent(in) :: n_point
      type(double_numbering_data), intent(inout) :: local_ids
!
      local_ids%n_point = n_point
      allocate(local_ids%irank(local_ids%n_point))
      allocate(local_ids%index(local_ids%n_point))
!
!$omp parallel workshare
      local_ids%irank(1:local_ids%n_point) = -1
      local_ids%index(1:local_ids%n_point) = 0
!$omp end parallel workshare
!
      end subroutine alloc_double_numbering_data
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_double_numbering_data(local_ids)
!
      type(double_numbering_data), intent(inout) :: local_ids
!
      deallocate(local_ids%irank, local_ids%index)
!
      end subroutine dealloc_double_numbering_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine node_dbl_numbering_to_repart                           &
     &         (nod_comm, node, part_tbl, new_ids_on_org)
!
      use nod_phys_send_recv
      use reverse_SR_int
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      type(calypso_comm_table), intent(in) :: part_tbl
!
      type(double_numbering_data), intent(inout) :: new_ids_on_org
!
      type(double_numbering_data) :: recieved_ids
      integer(kind = kint) :: inod
!
!
!    Set local recieved_ids in internal node
      call alloc_double_numbering_data                                  &
     &   (part_tbl%ntot_import, recieved_ids)
!$omp parallel do
      do inod = 1, part_tbl%ntot_import
        recieved_ids%index(inod) =    inod
        recieved_ids%irank(inod) = my_rank
      end do
!$omp end parallel do
!
!    Send localrecieved_ids into original domain new_ids_on_org
      call calypso_rev_SR_type_int                                      &
     &   (part_tbl, part_tbl%ntot_import, node%numnod,                  &
     &    recieved_ids%index, new_ids_on_org%index)
      call calypso_rev_SR_type_int                                      &
     &   (part_tbl, part_tbl%ntot_import, node%numnod,                  &
     &    recieved_ids%irank, new_ids_on_org%irank)
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, new_ids_on_org%irank)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, new_ids_on_org%index)
      call dealloc_double_numbering_data(recieved_ids)
!
      end subroutine node_dbl_numbering_to_repart
!
! ----------------------------------------------------------------------
!
      subroutine ext_node_dbl_numbering_by_SR(node, ext_tbl,            &
     &          new_ids_on_org, internal_node, recieved_nod_ids)
!
      use calypso_SR_type
      use select_copy_from_recv
!
      type(node_data), intent(in) :: node
      type(calypso_comm_table), intent(in) :: ext_tbl
      type(double_numbering_data), intent(in) :: new_ids_on_org
!
      integer(kind = kint), intent(in) :: internal_node
      type(double_numbering_data), intent(inout) :: recieved_nod_ids
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, internal_node
        recieved_nod_ids%index(inod) =    inod
        recieved_nod_ids%irank(inod) = my_rank
      end do
!$omp end parallel do
!
!    Set local recieved_nod_ids in external node
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import, new_ids_on_org%irank,       &
     &    recieved_nod_ids%irank(internal_node+1))
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import, new_ids_on_org%index,       &
     &    recieved_nod_ids%index(internal_node+1))
!
      end subroutine ext_node_dbl_numbering_by_SR
!
! ----------------------------------------------------------------------
!
      subroutine double_numbering_4_element(ele, ele_comm, ele_ids)
!
      use solver_SR_type
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
      type(double_numbering_data), intent(inout) :: ele_ids
!
      integer(kind = kint) :: iele
!
!
!$omp parallel do
      do iele = 1, ele%numele
        ele_ids%index(iele) = iele
        ele_ids%irank(iele) = my_rank
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (ele%numele, ele_comm, ele_ids%index)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (ele%numele, ele_comm, ele_ids%irank)
!
      end subroutine double_numbering_4_element
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine calypso_rev_SR_type_int                                &
     &         (cps_tbl, nnod_new, nnod_org, iX_new, iX_org)
!
      use m_solver_SR
      use calypso_SR_int
      use select_copy_from_recv
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      integer(kind = kint), intent(in) :: iX_new(nnod_new)
!
      integer(kind = kint), intent(inout) :: iX_org(nnod_org)
!
      integer(kind = kint), allocatable :: irev_tmp(:)
!
      allocate(irev_tmp(nnod_org))
!$omp parallel workshare
      irev_tmp(1:nnod_org) = 0
!$omp end parallel workshare
!
      call calypso_send_recv_int(iflag_import_item, nnod_new, nnod_org, &
     &    cps_tbl%nrank_import, cps_tbl%iflag_self_copy,                &
     &    cps_tbl%irank_import, cps_tbl%istack_import,                  &
     &    cps_tbl%item_import,                                          &
     &    cps_tbl%nrank_export, cps_tbl%iflag_self_copy,                &
     &    cps_tbl%irank_export, cps_tbl%istack_export,                  &
     &    cps_tbl%item_export, irev_tmp,                                &
     &    SR_sig1, SR_i1, iX_new, iX_org)
!
      deallocate(irev_tmp)
!
      end subroutine calypso_rev_SR_type_int
!
! ----------------------------------------------------------------------
!
      subroutine check_repart_node_transfer(nod_comm, node,             &
     &          new_comm, new_node, part_tbl, new_ids_on_org)
!
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(calypso_comm_table), intent(in) :: part_tbl
      type(double_numbering_data), intent(in) :: new_ids_on_org
!
      integer(kind = kint), allocatable :: inod_old_lc(:)
      integer(kind = kint), allocatable :: irank_old_lc(:)
      integer(kind = kint), allocatable :: inod_new_lc(:)
      integer(kind = kint), allocatable :: irank_new_lc(:)
!
      type(double_numbering_data) :: new_recved_id1
      type(double_numbering_data) :: new_recved_id2
!
      integer(kind = kint) :: icou, inum, i, ist, ied, num
      integer(kind = kint) :: ip, inod, iele, k1, ipart, iflag
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
     &   (node%numnod, nod_comm, irank_old_lc)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_old_lc)
!

      allocate(inod_new_lc(new_node%numnod))
      allocate(irank_new_lc(new_node%numnod))
      call alloc_double_numbering_data(new_node%numnod, new_recved_id1)
      call alloc_double_numbering_data(new_node%numnod, new_recved_id2)
!
      do inod = 1, new_node%internal_node
        inod_new_lc(inod) =  inod
        irank_new_lc(inod) = my_rank
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, irank_new_lc)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, inod_new_lc)
!
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    new_ids_on_org%index(1), new_recved_id1%index(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    new_ids_on_org%irank(1), new_recved_id1%irank(1))
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, new_recved_id1%index)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, new_recved_id1%irank)
!
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    new_ids_on_org%index(1), new_recved_id2%index(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    new_ids_on_org%irank(1), new_recved_id2%irank(1))
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, new_recved_id2%index)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, new_recved_id2%irank)
!
      write(*,*) 'Check new_recved_id2%irank, new_recved_id2%index'
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
      call dealloc_double_numbering_data(new_recved_id1)
      call dealloc_double_numbering_data(new_recved_id2)
!
      end subroutine check_repart_node_transfer
!
! ----------------------------------------------------------------------
!
      end module t_repart_double_numberings
