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
!!     &         (node, new_comm, new_node, part_tbl)
!!        type(node_data), intent(in) :: node
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(inout) :: new_node
!!      subroutine set_repart_element_connect                           &
!!     &         (new_numele, node, ele, ele_tbl, idomain_new, inod_new,&
!!     &          ie_newdomain, ie_newnod, new_ele)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
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
     &         (node, new_comm, new_node, part_tbl)
!
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
     &    node%inod_global(1), new_node%inod_global(1))
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,1), new_node%xx(1,1))
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,2), new_node%xx(1,2))
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,3), new_node%xx(1,3))
!
      call SOLVER_SEND_RECV_int8_type                                   &
     &   (new_node%numnod, new_comm, new_node%inod_global)
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, new_node%xx(1,1))
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, new_node%xx(1,2))
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, new_node%xx(1,3))
!
      end subroutine set_repart_node_position
!
! ----------------------------------------------------------------------
!
      subroutine set_repart_element_connect                             &
     &         (new_numele, node, ele, ele_tbl, idomain_new, inod_new,  &
     &          ie_newdomain, ie_newnod, new_ele)
!
      use calypso_SR_type
      use select_copy_from_recv
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      integer(kind = kint), intent(in) :: new_numele
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
      integer(kind = kint), intent(in) :: inod_new(node%numnod)
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
          ie_newnod(iele,k1) =    inod_new(inod)
          ie_newdomain(iele,k1) = idomain_new(inod)
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
      call allocate_ele_connect_type(new_ele)
!
!$omp parallel workshare
      new_ele%elmtyp(1:new_ele%numele) = ele%elmtyp(1)
      new_ele%nodelm(1:new_ele%numele) = ele%nodelm(1)
!$omp end parallel workshare
!
      call calypso_SR_type_int8(iflag_import_item, ele_tbl,             &
     &    ele%numele, ele_tbl%ntot_import, ele%iele_global(1), i8_recv)
!$omp parallel workshare
      new_ele%iele_global(1:new_ele%numele) = i8_recv(1:new_ele%numele)
!$omp end parallel workshare
!
      do k1 = 1, ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import, ie_newnod(1,k1), i4_recv)
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
