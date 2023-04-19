!>@file   search_ext_node_repartition.f90
!!@brief  module search_ext_node_repartition
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine s_search_ext_node_repartition                        &
!!     &         (ele, ele_tbl, org_iele_dbl, ie_newdomain,             &
!!     &          new_comm, new_node, new_ele, SR_sig, SR_i)
!!      subroutine search_ext_node_with_ele_comm                        &
!!     &        (ele, ele_tbl, org_iele_dbl, new_iele_dbl, new_inod_dbl,&
!!     &         ie_newdomain, new_comm, new_node, new_ele_comm,        &
!!     &         new_ele, SR_sig, SR_i)
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!        type(communication_table), intent(in) :: new_comm
!!        type(communication_table), intent(in) :: new_ele_comm
!!        type(node_ele_double_number), intent(in) :: org_iele_dbl
!!        type(node_ele_double_number), intent(in) :: new_iele_dbl
!!        type(node_ele_double_number), intent(in) :: new_inod_dbl
!!        type(node_data), intent(in) :: new_node
!!        type(element_data), intent(inout) :: new_ele
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!
      module search_ext_node_repartition
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_geometry_data
      use t_comm_table
      use t_calypso_comm_table
      use t_solver_SR
      use t_solver_SR_int
      use t_para_double_numbering
      use t_repart_double_numberings
      use t_local_node_id_in_import
      use t_works_for_ext_node_search
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_search_ext_node_repartition                          &
     &         (ele, ele_tbl, org_iele_dbl, ie_newdomain,               &
     &          new_comm, new_node, new_ele, SR_sig, SR_i)
!
      use calypso_SR_type
      use solver_SR_type
      use search_from_list
      use select_copy_from_recv
      use find_external_node_4_repart
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(node_ele_double_number), intent(in) :: org_iele_dbl
!
      integer(kind = kint), intent(in)                                  &
     &              :: ie_newdomain(ele%numele,ele%nnod_4_ele)
!
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint), allocatable :: icount_node(:)
!
      type(works_for_ext_node_search), save :: wk_ext1
      type(local_node_id_in_import), save :: lcl_1
!
      integer(kind = kint) :: inod, icou
!
!
      call alloc_works_ext_node_search(new_node, ele_tbl,               &
     &    new_ele%numele,new_ele%nnod_4_ele, wk_ext1)
!
      call set_works_for_ext_node_search                                &
     &   (ele, ele_tbl, org_iele_dbl, ie_newdomain,                     &
     &    new_comm, new_node, new_ele, wk_ext1, SR_sig, SR_i)
!
      call init_local_node_id_in_import(new_comm, new_node%numnod,      &
     &                                  wk_ext1%inod_recv, lcl_1)
!      call init_item_import_recv(new_comm, new_node%numnod,            &
!     &                           wk_ext1%inod_recv, lcl_import)
!
      allocate(icount_node(new_node%numnod))
!$omp parallel workshare
      icount_node(1:new_node%numnod) = 0
!$omp end parallel workshare
!
      call find_ext_node_minimum(new_comm, wk_ext1, lcl_1,              &
     &                           new_ele, icount_node)
!      call dealloc_item_import_recv(lcl_import)
      call dealloc_local_nod_id_in_import(lcl_1)
      call dealloc_works_ext_node_search(wk_ext1)
!
      if(i_debug .gt. 0) then
        icou = 0
        do inod = 1, new_node%numnod
          if(icount_node(inod) .eq. 0) icou = icou + 1
        end do
        write(*,*) my_rank, 'Missing connectivity: ', icou,             &
     &          ' of ', new_ele%nnod_4_ele*new_ele%numele
      end if
!
      deallocate(icount_node)
!
      end subroutine s_search_ext_node_repartition
!
! ----------------------------------------------------------------------
!
      subroutine search_ext_node_with_ele_comm                          &
     &         (ele, ele_tbl, org_iele_dbl, new_iele_dbl, new_inod_dbl, &
     &          ie_newdomain, new_comm, new_node, new_ele_comm,         &
     &          new_ele, SR_sig, SR_i)
!
      use calypso_SR_type
      use solver_SR_type
      use search_from_list
      use select_copy_from_recv
      use find_external_node_4_repart
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(communication_table), intent(in) :: new_comm, new_ele_comm
      type(node_data), intent(in) :: new_node
      type(node_ele_double_number), intent(in) :: org_iele_dbl
      type(node_ele_double_number), intent(in) :: new_iele_dbl
      type(node_ele_double_number), intent(in) :: new_inod_dbl
!
      integer(kind = kint), intent(in)                                  &
     &              :: ie_newdomain(ele%numele,ele%nnod_4_ele)
!
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint), allocatable :: icount_node(:)
!
      type(double_indices_connectivity), save :: conn_dbl1
      type(works_for_ext_node_search), save :: wk_ext1
      type(local_node_id_in_import), save :: lcl_1
!
!
      call alloc_works_ext_node_search(new_node, ele_tbl,               &
     &    new_ele%numele,new_ele%nnod_4_ele, wk_ext1)
!
      call set_works_for_ext_node_search                                &
     &   (ele, ele_tbl, org_iele_dbl, ie_newdomain,                     &
     &    new_comm, new_node, new_ele, wk_ext1, SR_sig, SR_i)
!
      call init_local_node_id_in_import(new_comm, new_node%numnod,      &
     &                                  wk_ext1%inod_recv, lcl_1)
!
      allocate(icount_node(new_node%numnod))
!$omp parallel workshare
      icount_node(1:new_node%numnod) = 0
!$omp end parallel workshare
!
      call find_ext_node_minimum(new_comm, wk_ext1, lcl_1,              &
     &                           new_ele, icount_node)
!
      call alloc_dbl_idx_connect                                        &
     &   (new_ele%numele, new_ele%nnod_4_ele, conn_dbl1)
      call set_dbl_index_in_ele_connect                                 &
     &   (new_ele_comm, new_inod_dbl, new_iele_dbl,                     &
     &    new_ele, conn_dbl1, SR_sig, SR_i)
!
      call find_ext_node_full(new_iele_dbl, new_comm,                   &
     &    conn_dbl1, wk_ext1, lcl_1, new_ele, icount_node)
!      call dealloc_item_import_recv(lcl_import)
      call dealloc_local_nod_id_in_import(lcl_1)
      call dealloc_dbl_idx_connect(conn_dbl1)
      call dealloc_works_ext_node_search(wk_ext1)
      deallocate(icount_node)
!
      end subroutine search_ext_node_with_ele_comm
!
! ----------------------------------------------------------------------
!
      end module search_ext_node_repartition
