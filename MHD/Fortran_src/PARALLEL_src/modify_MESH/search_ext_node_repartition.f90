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
      call find_ext_node_full(new_iele_dbl, new_comm, new_node,         &
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
! ----------------------------------------------------------------------
!
      subroutine find_ext_node_minimum(new_comm, wk_ext, lcl_c,         &
     &                                 new_ele, icount_node)
!
      type(communication_table), intent(in) :: new_comm
!
      type(works_for_ext_node_search), intent(in) :: wk_ext
      type(local_node_id_in_import), intent(in) :: lcl_c
!
      type(element_data), intent(inout) :: new_ele
      integer(kind = kint), intent(inout) :: icount_node(:)
!
      integer(kind = kint) :: inod, icou, iele, k1
!
!
      icou = 0
      do iele = 1, wk_ext%num_loop
        do k1 = 1, new_ele%nnod_4_ele
          new_ele%ie(iele,k1) = search_repart_external_node             &
     &                        (wk_ext%ie_tmp(iele,k1),                  &
     &                         wk_ext%ie_domain_recv(iele,k1),          &
     &                         my_rank, new_comm, lcl_c)
!
          inod = new_ele%ie(iele,k1)
          if(inod .le. 0) then
            write(*,*) my_rank, 'Node cannot be found for ',            &
     &        new_ele%iele_global(iele), iele, k1,                      &
     &        wk_ext%ie_domain_recv(iele,k1), wk_ext%ie_tmp(iele,k1),   &
     &        wk_ext%iele_org_local(iele),                              &
     &        wk_ext%iele_org_domain(iele)
            icou = icou + 1
          else
            icount_node(inod) = icount_node(inod) + 1
          end if
        end do
      end do
!
      end subroutine find_ext_node_minimum
!
! ----------------------------------------------------------------------
!
      subroutine find_ext_node_full(new_iele_dbl, new_comm, new_node,   &
     &          conn_dbl, wk_ext, lcl_c, new_ele, icount_node)
!
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(node_ele_double_number), intent(in) :: new_iele_dbl
!
      type(double_indices_connectivity), intent(in) :: conn_dbl
      type(works_for_ext_node_search), intent(in) :: wk_ext
      type(local_node_id_in_import), intent(in) :: lcl_c
!
      type(element_data), intent(inout) :: new_ele
      integer(kind = kint), intent(inout) :: icount_node(:)
!
      integer(kind = kint) :: inod, icou, iele, k1
!
!
      icou = 0
      do iele = 1, new_ele%numele
        if(new_iele_dbl%irank(iele) .eq. my_rank) cycle
!
        do k1 = 1, new_ele%nnod_4_ele
          new_ele%ie(iele,k1) = search_repart_external_node             &
     &                        (conn_dbl%ie_local(iele,k1),              &
     &                         conn_dbl%irank_e(iele,k1),               &
     &                         my_rank, new_comm, lcl_c)
!
          inod = new_ele%ie(iele,k1)
          if(inod .le. 0) then
            write(*,*) my_rank, 'Node cannot be found for ',            &
     &        new_ele%iele_global(iele), iele, k1,                      &
     &        conn_dbl%irank_e(iele,k1), conn_dbl%ie_local(iele,k1),    &
     &        wk_ext%iele_org_local(iele), wk_ext%iele_org_domain(iele)
            icou = icou + 1
          else
            icount_node(inod) = icount_node(inod) + 1
          end if
        end do
      end do
!
      end subroutine find_ext_node_full
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_repart_external_node         &
     &                            (inod, ip, id_rank, new_comm, lcl_i)
!
      use t_local_node_id_in_import
      use search_from_list
!
      integer(kind = kint), intent(in) :: ip, inod
      integer(kind = kint), intent(in) :: id_rank
!
      type(communication_table), intent(in) :: new_comm
      type(local_node_id_in_import), intent(in) :: lcl_i
!
      integer(kind = kint) :: ie_new
!
      integer(kind = kint) :: jnum, jst, jed, knum
!
!
      if(ip .eq. id_rank) then
        ie_new = inod
      else
        ie_new = 0
        jst = lcl_i%istack_rev_import_recv(inod-1) + 1
        jed = lcl_i%istack_rev_import_recv(inod  )
!
        if((jed-jst) .ge. 0) then
          knum = search_from_sorted_data(ip, jst, jed,                  &
     &                                   new_comm%ntot_import,          &
     &                                   lcl_i%irank_import_recv)
          if(knum.ge.jst .and. knum.le.jed) then
            jnum =   lcl_i%irev_import(knum)
            ie_new = new_comm%item_import(jnum)
          end if
        end if
      end if
      search_repart_external_node = ie_new
!
      end function search_repart_external_node
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_repart_ext_node_old          &
     &                            (inod, ip, id_rank, lcl_i,            &
     &                             new_comm, ie_new)
!
      use t_local_node_id_in_import
      use search_from_list
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ip, inod
!
      type(communication_table), intent(in) :: new_comm
      type(local_node_id_in_import), intent(in) :: lcl_i
!
      integer(kind = kint), intent(inout) :: ie_new
!
      integer(kind = kint) :: inum, ist, ied, jnum
!
!
      if(ip .eq. id_rank) then
        ie_new = inod
      else
        ie_new = 0
        inum = search_from_list_data(ip, ione, new_comm%num_neib,       &
     &                         new_comm%num_neib, new_comm%id_neib)
        ist = 0
        ied = -1
        if(inum.ge.ione .and. inum.le.new_comm%num_neib) then
          ist = new_comm%istack_import(inum-1) + 1
          ied = new_comm%istack_import(inum)
        end if
!
        if(ied .ge. ist) then
          jnum = search_from_sorted_data(inod, ist, ied,                &
     &                    new_comm%ntot_import, lcl_i%item_import_recv)
!
          if(jnum.ge.ist .and. jnum.le.ied) then
            ie_new = new_comm%item_import(jnum)
          end if
        end if
      end if
      search_repart_ext_node_old = ie_new
!
      end function search_repart_ext_node_old
!
! ----------------------------------------------------------------------
!
      end module search_ext_node_repartition
