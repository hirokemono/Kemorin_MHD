!>@file   const_repart_mesh_data.f90
!!@brief  module const_repart_mesh_data
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine set_repart_node_position                             &
!!     &         (part_tbl, node, new_comm, new_node)
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(inout) :: new_node
!!      subroutine set_repart_element_connect                           &
!!     &         (new_numele, ele, ele_tbl, new_ids_on_org,             &
!!     &          ie_newdomain, ie_newnod, new_ele)
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!        type(node_ele_double_number), intent(in) :: new_ids_on_org
!!        type(element_data), intent(inout) :: new_ele
!!@endverbatim
!
      module const_repart_mesh_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
      subroutine set_repart_node_position                               &
     &         (part_tbl, node, new_comm, new_node)
!
      use m_solver_SR
      use calypso_mpi_int
      use calypso_SR_type
      use select_copy_from_recv
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(calypso_comm_table), intent(in) :: part_tbl
      type(communication_table), intent(in) :: new_comm
!
      type(node_data), intent(inout) :: new_node
!
!
      new_node%internal_node =                 part_tbl%ntot_import
      new_node%numnod = new_comm%ntot_import + part_tbl%ntot_import
!
!      write(*,*) my_rank, 'new_nomond', new_node%internal_node,        &
!     &           new_node%numnod, new_comm%ntot_import
      call alloc_node_geometry_base(new_node)
!
      call calypso_SR_type_int8(iflag_import_item, part_tbl,            &
     &    node%numnod, new_node%internal_node,                          &
     &    node%inod_global(1), new_node%inod_global(1),                 &
     &    SR_sig1, SR_il1)
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,1), new_node%xx(1,1), SR_sig1, SR_r1)
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,2), new_node%xx(1,2), SR_sig1, SR_r1)
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,3), new_node%xx(1,3), SR_sig1, SR_r1)
!
      call SOLVER_SEND_RECV_int8_type(new_node%numnod, new_comm,        &
     &    SR_sig1, SR_il1, new_node%inod_global)
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, SR_sig1, SR_r1, new_node%xx(1,1))
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, SR_sig1, SR_r1, new_node%xx(1,2))
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, SR_sig1, SR_r1, new_node%xx(1,3))
!
      end subroutine set_repart_node_position
!
! ----------------------------------------------------------------------
!
      subroutine set_repart_element_connect                             &
     &         (new_numele, ele, ele_tbl, new_ids_on_org,               &
     &          ie_newdomain, ie_newnod, new_ele)
!
      use m_solver_SR
      use calypso_SR_type
      use select_copy_from_recv
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(node_ele_double_number), intent(in) :: new_ids_on_org
      integer(kind = kint), intent(in) :: new_numele
!
      type(element_data), intent(inout) :: new_ele
      integer(kind = kint), intent(inout)                               &
     &            :: ie_newnod(ele%numele,ele%nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &            :: ie_newdomain(ele%numele,ele%nnod_4_ele)
!
      integer(kind = kint), allocatable :: i4_recv(:)
      integer(kind = kint_gl), allocatable :: i8_recv(:)
!
      integer(kind = kint) :: k1, iele, inod
!
!
!$omp parallel
      do k1 = 1, ele%nnod_4_ele
!$omp do private(iele,inod)
        do iele = 1, ele%numele
          inod = ele%ie(iele,k1)
          ie_newnod(iele,k1) =    new_ids_on_org%index(inod)
          ie_newdomain(iele,k1) = new_ids_on_org%irank(inod)
        end do
!$omp end do
      end do
!$omp end parallel
!
      allocate(i4_recv(ele_tbl%ntot_import))
      allocate(i8_recv(ele_tbl%ntot_import))
!$omp parallel workshare
      i4_recv(1:ele_tbl%ntot_import) = 0
      i8_recv(1:ele_tbl%ntot_import) = 0
!$omp end parallel workshare
!
      new_ele%numele =     new_numele
      new_ele%nnod_4_ele = ele%nnod_4_ele
      call alloc_ele_connect(new_ele)
!
!$omp parallel workshare
      new_ele%elmtyp(1:new_ele%numele) = ele%elmtyp(1)
      new_ele%nodelm(1:new_ele%numele) = ele%nodelm(1)
!$omp end parallel workshare
!
      call calypso_SR_type_int8(iflag_import_item, ele_tbl,             &
     &    ele%numele, ele_tbl%ntot_import, ele%iele_global(1), i8_recv, &
     &    SR_sig1, SR_il1)
!$omp parallel workshare
      new_ele%iele_global(1:new_ele%numele) = i8_recv(1:new_ele%numele)
!$omp end parallel workshare
!
      do k1 = 1, ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import, ie_newnod(1,k1), i4_recv,  &
     &      SR_sig1, SR_i1)
!$omp parallel workshare
        new_ele%ie(1:new_ele%numele,k1) = i4_recv(1:new_ele%numele)
!$omp end parallel workshare
      end do
!
      deallocate(i4_recv, i8_recv)
!
      end subroutine set_repart_element_connect
!
! ----------------------------------------------------------------------
!
      end module const_repart_mesh_data
