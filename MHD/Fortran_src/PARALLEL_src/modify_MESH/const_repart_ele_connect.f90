!>@file   const_repart_ele_connect.f90
!!@brief  module const_repart_ele_connect
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine s_const_repart_ele_connect(mesh, ele_comm, part_tbl, &
!!     &          new_ids_on_org, new_comm, new_node, new_ele, ele_tbl, &
!!     &          SR_sig, SR_i, SR_il)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(communication_table), intent(in) :: ele_comm
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(node_ele_double_number), intent(in) :: new_ids_on_org
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(in) :: new_node
!!        type(calypso_comm_table), intent(inout) :: ele_tbl
!!        type(element_data), intent(inout) :: new_ele
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine const_reparition_ele_connect                         &
!!     &         (ele, ele_tbl, new_ids_on_org, org_iele_dbl,           &
!!     &          new_numele, new_comm, new_node, new_ele,              &
!!     &          SR_sig, SR_i, SR_il)
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!        type(node_ele_double_number), intent(in) :: new_ids_on_org
!!        type(node_ele_double_number), intent(in) :: org_iele_dbl
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(in) :: new_node
!!        integer(kind = kint), intent(in) :: new_numele
!!        type(element_data), intent(inout) :: new_ele
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!@endverbatim
!
      module const_repart_ele_connect
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_next_node_ele_4_node
      use t_calypso_comm_table
      use t_sorting_for_repartition
      use t_para_double_numbering
      use t_repart_double_numberings
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_repart_ele_connect(mesh, ele_comm, part_tbl,   &
     &          new_ids_on_org, new_comm, new_node, new_ele, ele_tbl,   &
     &          SR_sig, SR_i, SR_il)
!
      use ele_trans_tbl_4_repart
      use compare_mesh_structures
!
      type(mesh_geometry), intent(in) :: mesh
      type(communication_table), intent(in) :: ele_comm
      type(calypso_comm_table), intent(in) :: part_tbl
      type(node_ele_double_number), intent(in) :: new_ids_on_org
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
!
      type(calypso_comm_table), intent(inout) :: ele_tbl
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      type(node_ele_double_number) :: org_iele_dbl
!
      integer(kind = kint) :: new_numele, icount_error
!
!
      call alloc_double_numbering(mesh%ele%numele, org_iele_dbl)
      call double_numbering_4_element(mesh%ele, ele_comm, org_iele_dbl, &
     &                                SR_sig, SR_i)
!
      call const_ele_trans_tbl_for_repart                               &
     &   (mesh%node, mesh%ele, part_tbl, new_ids_on_org%irank, ele_tbl)
!      call check_element_transfer_tbl(mesh%ele, ele_tbl)
!
      call trim_overlapped_ele_by_repart                                &
     &   (mesh, org_iele_dbl, ele_tbl, new_numele, SR_sig, SR_i)
!
      call const_reparition_ele_connect                                 &
     &   (mesh%ele, ele_tbl, new_ids_on_org, org_iele_dbl,              &
     &    new_numele, new_comm, new_node, new_ele, SR_sig, SR_i, SR_il)
!
      call dealloc_double_numbering(org_iele_dbl)
!
      end subroutine s_const_repart_ele_connect
!
! ----------------------------------------------------------------------
!
      subroutine const_reparition_ele_connect                           &
     &         (ele, ele_tbl, new_ids_on_org, org_iele_dbl,             &
     &          new_numele, new_comm, new_node, new_ele,                &
     &          SR_sig, SR_i, SR_il)
!
      use search_ext_node_repartition
      use const_repart_mesh_data
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(node_ele_double_number), intent(in) :: new_ids_on_org
      type(node_ele_double_number), intent(in) :: org_iele_dbl
!
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      integer(kind = kint), intent(in) :: new_numele
!
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint), allocatable :: ie_newnod(:,:)
      integer(kind = kint), allocatable :: ie_newdomain(:,:)
!
!
      allocate(ie_newnod(ele%numele,ele%nnod_4_ele))
      allocate(ie_newdomain(ele%numele,ele%nnod_4_ele))
!$omp parallel workshare
      ie_newnod(1:ele%numele,1:ele%nnod_4_ele) =    0
      ie_newdomain(1:ele%numele,1:ele%nnod_4_ele) = 0
!$omp end parallel workshare
!
      call set_repart_element_connect(new_numele, ele, ele_tbl,         &
     &    new_ids_on_org, ie_newdomain, ie_newnod, new_ele,             &
     &    SR_sig, SR_i, SR_il)
!
      call s_search_ext_node_repartition                                &
     &   (ele, ele_tbl, org_iele_dbl, ie_newdomain,                     &
     &    new_comm, new_node, new_ele, SR_sig, SR_i)
      deallocate(ie_newnod, ie_newdomain)
!
      end subroutine const_reparition_ele_connect
!
! ----------------------------------------------------------------------
!
      subroutine compare_ele_connect_2                                  &
     &         (id_rank, org_ele, new_ele, icount_error)
!
      use t_geometry_data
!
      integer, intent(in) :: id_rank
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(in) :: new_ele
      integer(kind = kint), intent(inout) :: icount_error
!
      integer(kind = kint) :: iele, k1
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) id_rank, 'numele', org_ele%numele, new_ele%numele
        write(*,*) id_rank, 'nnod_4_ele', org_ele%nnod_4_ele,           &
     &                                     new_ele%nnod_4_ele
        write(*,*) id_rank, 'first_ele_type', org_ele%first_ele_type,   &
     &                               new_ele%first_ele_type
      end if
!
      icount_error = 0
      if(org_ele%numele .ne. new_ele%numele) then
        write(*,*) 'Number of element is differenct: ',                 &
     &             org_ele%numele, new_ele%numele
        icount_error = icount_error + 1
      end if
      if(org_ele%nnod_4_ele .ne. new_ele%nnod_4_ele) then
        write(*,*) 'Element type is differennt: ',                      &
     &             org_ele%nnod_4_ele, new_ele%nnod_4_ele
        icount_error = icount_error + 1
      end if
!
      do iele = 1, org_ele%numele
        if(org_ele%elmtyp(iele) .ne. new_ele%elmtyp(iele)) then
          write(*,*) 'element type at ', iele, ' is differ',            &
     &        org_ele%elmtyp(iele), new_ele%elmtyp(iele)
          icount_error = icount_error + 1
        end if
      end do
      do iele = 1, org_ele%numele
        if(org_ele%nodelm(iele) .ne. new_ele%nodelm(iele)) then
          write(*,*) 'number of node for ', iele, ' is differ',         &
     &        org_ele%nodelm(iele), new_ele%nodelm(iele)
          icount_error = icount_error + 1
        end if
      end do
      do k1 = 1, org_ele%nnod_4_ele
        do iele = 1, org_ele%numele
          if(org_ele%ie(iele,k1) .ne. new_ele%ie(iele,k1)) then
            icount_error = icount_error + 1
          end if
        end do
      end do
!
      end subroutine compare_ele_connect_2
!
!------------------------------------------------------------------
!
      end module const_repart_ele_connect
