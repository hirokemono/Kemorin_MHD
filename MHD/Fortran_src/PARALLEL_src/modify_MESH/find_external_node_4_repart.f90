!>@file   find_external_node_4_repart.f90
!!@brief  module find_external_node_4_repart
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine find_ext_node_minimum(new_comm, wk_ext, lcl_c,       &
!!     &                                 new_ele, icount_node)
!!      subroutine find_ext_node_full(new_iele_dbl, new_comm,           &
!!     &          conn_dbl, wk_ext, lcl_c, new_ele, icount_node)
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_ele_double_number), intent(in) :: new_iele_dbl
!!        type(double_indices_connectivity), intent(in) :: conn_dbl
!!        type(works_for_ext_node_search), intent(in) :: wk_ext
!!        type(local_node_id_in_import), intent(in) :: lcl_c
!!        type(element_data), intent(inout) :: new_ele
!!        integer(kind = kint), intent(inout) :: icount_node(:)
!!
!!      integer(kind = kint) function search_repart_external_node       &
!!     &                            (inod, ip, id_rank, new_comm, lcl_i)
!!      integer(kind = kint) function search_repart_ext_node_old        &
!!     &                            (inod, ip, id_rank, new_comm, lcl_i)
!!        integer(kind = kint), intent(in) :: ip, inod
!!        integer(kind = kint), intent(in) :: id_rank
!!        type(communication_table), intent(in) :: new_comm
!!        type(local_node_id_in_import), intent(in) :: lcl_i
!!@endverbatim
!
      module find_external_node_4_repart
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_comm_table
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
      subroutine find_ext_node_full(new_iele_dbl, new_comm,             &
     &          conn_dbl, wk_ext, lcl_c, new_ele, icount_node)
!
      type(communication_table), intent(in) :: new_comm
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
     &                            (inod, ip, id_rank, new_comm, lcl_i)
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
      integer(kind = kint) :: ie_new
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
      end module find_external_node_4_repart
