!>@file   t_repart_double_numberings.f90
!!@brief  module t_repart_double_numberings
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine node_dbl_numbering_to_repart                         &
!!     &         (nod_comm, node, part_tbl, idomain_new, inod_new)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!      subroutine ext_node_dbl_numbering_by_SR                         &
!!     &         (node, ext_tbl, idomain_new, inod_new,                 &
!!     &          numnod, internal_node, idomain_recv, inod_recv)
!!        type(node_data), intent(in) :: node
!!        type(calypso_comm_table), intent(inout) :: ext_tbl
!!      subroutine double_numbering_4_element(ele, ele_comm,            &
!!     &                                      iele_local, iele_domain)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: ele_comm
!!
!!      subroutine calypso_rev_SR_type_int                              &
!!     &         (cps_tbl, nnod_new, nnod_org, iX_new, iX_org)
!!        type(calypso_comm_table), intent(in) :: cps_tbl
!!
!!      subroutine check_repart_node_transfer                           &
!!     &         (nod_comm, node, new_comm, new_node, part_tbl,         &
!!     &         idomain_new, inod_new)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(in) :: new_node
!!        type(calypso_comm_table), intent(in) :: part_tbl
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine node_dbl_numbering_to_repart                           &
     &         (nod_comm, node, part_tbl, idomain_new, inod_new)
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
      integer(kind = kint), intent(inout) :: idomain_new(node%numnod)
      integer(kind = kint), intent(inout) :: inod_new(node%numnod)
!
      integer(kind = kint), allocatable :: idomain_recv(:)
      integer(kind = kint), allocatable :: inod_recv(:)
!
      integer(kind = kint) :: inod
!
!
!    Set local (idomain_recv, inod_recv) in internal node
      allocate(inod_recv(part_tbl%ntot_import))
      allocate(idomain_recv(part_tbl%ntot_import))
!$omp parallel do
      do inod = 1, part_tbl%ntot_import
        inod_recv(inod) =    inod
        idomain_recv(inod) = my_rank
      end do
!$omp end parallel do
!
!
!    Send local (idomain_recv, inod_recv) into original domain
!       (inod_new, idomain_new)
      call calypso_rev_SR_type_int(part_tbl,                            &
     &    part_tbl%ntot_import, node%numnod, inod_recv, inod_new)
      call calypso_rev_SR_type_int(part_tbl,                            &
     &    part_tbl%ntot_import, node%numnod, idomain_recv, idomain_new)
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, idomain_new)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_new)
      deallocate(inod_recv, idomain_recv)
!
      end subroutine node_dbl_numbering_to_repart
!
! ----------------------------------------------------------------------
!
      subroutine ext_node_dbl_numbering_by_SR                           &
     &         (node, ext_tbl, idomain_new, inod_new,                   &
     &          numnod, internal_node, idomain_recv, inod_recv)
!
      use calypso_SR_type
      use select_copy_from_recv
!
      type(node_data), intent(in) :: node
      type(calypso_comm_table), intent(inout) :: ext_tbl
!
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
      integer(kind = kint), intent(in) :: inod_new(node%numnod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(inout) :: idomain_recv(numnod)
      integer(kind = kint), intent(inout) :: inod_recv(numnod)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, internal_node
        inod_recv(inod) =    inod
        idomain_recv(inod) = my_rank
      end do
!$omp end parallel do
!
!    Set local (idomain_recv, inod_recv) in external node
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    idomain_new(1), idomain_recv(internal_node+1))
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    inod_new(1), inod_recv(internal_node+1))
!
      end subroutine ext_node_dbl_numbering_by_SR
!
! ----------------------------------------------------------------------
!
      subroutine double_numbering_4_element(ele, ele_comm,              &
     &                                      iele_local, iele_domain)
!
      use solver_SR_type
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
!
      integer(kind = kint), intent(inout) :: iele_local(ele%numele)
      integer(kind = kint), intent(inout) :: iele_domain(ele%numele)
!
      integer(kind = kint) :: iele
!
!
!$omp parallel do
      do iele = 1, ele%numele
        iele_local(iele) = iele
        iele_domain(iele) = my_rank
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm, iele_local)
      call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm, iele_domain)
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
      subroutine check_repart_node_transfer                             &
     &         (nod_comm, node, new_comm, new_node, part_tbl,           &
     &         idomain_new, inod_new)
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
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
      integer(kind = kint), intent(in) :: inod_new(node%numnod)
!
      integer(kind = kint), allocatable :: inod_old_lc(:)
      integer(kind = kint), allocatable :: irank_old_lc(:)
      integer(kind = kint), allocatable :: inod_new_lc(:)
      integer(kind = kint), allocatable :: irank_new_lc(:)
!
      integer(kind = kint), allocatable :: inod_trns1(:)
      integer(kind = kint), allocatable :: irank_trns1(:)
      integer(kind = kint), allocatable :: inod_trns2(:)
      integer(kind = kint), allocatable :: irank_trns2(:)
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
      allocate(inod_trns1(new_node%numnod))
      allocate(irank_trns1(new_node%numnod))
      allocate(inod_trns2(new_node%numnod))
      allocate(irank_trns2(new_node%numnod))
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
     &    inod_new(1), inod_trns1(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    idomain_new(1), irank_trns1(1))
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, inod_trns1)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, irank_trns1)
!
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    inod_new(1), inod_trns2(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    idomain_new(1), irank_trns2(1))
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, inod_trns2)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, irank_trns2)
!
      write(*,*) 'Check irank_trns2, inod_trns2'
      do inod = 1, new_node%internal_node
        if(inod_trns1(inod) .ne. inod_new_lc(inod)                      &
     &      .or. irank_trns1(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns1!' , inod
        end if
        if(inod_trns2(inod) .ne. inod_new_lc(inod)                      &
     &      .or. irank_trns2(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns2!' , inod
        end if
      end do
!
      do inod = new_node%internal_node+1, new_node%numnod
        if(inod_trns1(inod) .ne. inod_new_lc(inod)                      &
     &      .or. irank_trns1(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns1!' , inod
        end if
        if(inod_trns2(inod) .ne. inod_new_lc(inod)                      &
     &      .or. irank_trns2(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns2!' , inod
        end if
      end do
      deallocate(irank_old_lc, inod_old_lc)
      deallocate(irank_new_lc, inod_new_lc)
      deallocate(irank_trns1, inod_trns1)
      deallocate(irank_trns2, inod_trns2)
!
      end subroutine check_repart_node_transfer
!
! ----------------------------------------------------------------------
!
      end module t_repart_double_numberings
