!>@file   t_works_for_ext_node_search.f90
!!@brief  module t_works_for_ext_node_search
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Work area to search external node address
!!
!!@verbatim
!!      subroutine alloc_works_ext_node_search                          &
!!     &         (new_node, ele_tbl, new_numele, new_nnod_4_ele, wk_ext)
!!      subroutine dealloc_works_ext_node_search(wk_ext)
!!      subroutine set_works_for_ext_node_search                        &
!!     &         (ele, ele_tbl, org_iele_dbl, ie_newdomain,             &
!!     &          new_comm, new_node, new_ele, wk_ext, SR_sig, SR_i)
!!        integer(kind = kint), intent(in) :: new_numele, new_nnod_4_ele
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(in) :: new_node
!!        type(node_ele_double_number), intent(in) :: org_iele_dbl
!!        integer(kind = kint), intent(in)                              &
!!     &            :: ie_newdomain(ele%numele,ele%nnod_4_ele)
!!        type(element_data), intent(inout) :: new_ele
!!        type(works_for_ext_node_search), intent(inout) :: wk_ext
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!
!!      subroutine alloc_dbl_idx_connect                                &
!!     &         (new_numele, new_nnod_4_ele, conn_dbl)
!!      subroutine dealloc_dbl_idx_connect(conn_dbl)
!!      subroutine set_dbl_index_in_ele_connect                         &
!!     &          (new_ele_comm, new_inod_dbl, new_iele_dbl,            &
!!     &           new_ele, conn_dbl, SR_sig, SR_i)
!!        integer(kind = kint), intent(in) :: new_numele, new_nnod_4_ele
!!        type(communication_table), intent(in) :: new_ele_comm
!!        type(node_ele_double_number), intent(in) :: new_inod_dbl
!!        type(node_ele_double_number), intent(in) :: new_iele_dbl
!!        type(double_indices_connectivity), intent(inout) :: conn_dbl
!!        type(element_data), intent(inout) :: new_ele
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!
      module t_works_for_ext_node_search
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
!
      implicit none
!
      type works_for_ext_node_search
        integer(kind = kint) :: num_loop
        integer(kind = kint), allocatable :: inod_recv(:)
        integer(kind = kint), allocatable :: icount_node(:)
!
        integer(kind = kint), allocatable :: iele_org_local(:)
        integer(kind = kint), allocatable :: iele_org_domain(:)
        integer(kind = kint), allocatable :: ie_domain_recv(:,:)
!
        integer(kind = kint), allocatable :: ie_tmp(:,:)
      end type works_for_ext_node_search
!
      type double_indices_connectivity
        integer(kind = kint), allocatable :: ie_local(:,:)
        integer(kind = kint), allocatable :: irank_e(:,:)
      end type double_indices_connectivity
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_works_ext_node_search                            &
     &         (new_node, ele_tbl, new_numele, new_nnod_4_ele, wk_ext)
!
      use t_geometry_data
      use t_calypso_comm_table
!
      type(node_data), intent(in) :: new_node
      type(calypso_comm_table), intent(in) :: ele_tbl
      integer(kind = kint), intent(in) :: new_numele, new_nnod_4_ele
      type(works_for_ext_node_search), intent(inout) :: wk_ext
!
!
      wk_ext%num_loop = min(new_numele, ele_tbl%ntot_import)
!
      allocate(wk_ext%inod_recv(new_node%numnod))
      if(new_node%numnod .gt. 0) then
!$omp parallel workshare
        wk_ext%inod_recv(1:new_node%numnod) = 0
!$omp end parallel workshare
      end if
!
      allocate(wk_ext%ie_tmp(new_numele,new_nnod_4_ele))
      allocate(wk_ext%ie_domain_recv(new_numele,new_nnod_4_ele))
!
      allocate(wk_ext%iele_org_local(new_numele))
      allocate(wk_ext%iele_org_domain(new_numele))
!
      if(new_numele .gt. 0) then
!$omp parallel workshare
        wk_ext%ie_tmp(1:new_numele,1:new_nnod_4_ele) =          0
        wk_ext%ie_domain_recv(1:new_numele,1:new_nnod_4_ele) = -1
!$omp end parallel workshare
!
!$omp parallel workshare
        wk_ext%iele_org_local(1:new_numele) =  0
        wk_ext%iele_org_domain(1:new_numele) = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_works_ext_node_search
!
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_works_ext_node_search(wk_ext)
!
      type(works_for_ext_node_search), intent(inout) :: wk_ext
!
      deallocate(wk_ext%iele_org_local, wk_ext%iele_org_domain)
      deallocate(wk_ext%ie_tmp, wk_ext%ie_domain_recv)
      deallocate(wk_ext%inod_recv)
!
      end subroutine dealloc_works_ext_node_search
!
! ----------------------------------------------------------------------
!
      subroutine set_works_for_ext_node_search                          &
     &         (ele, ele_tbl, org_iele_dbl, ie_newdomain,               &
     &          new_comm, new_node, new_ele, wk_ext, SR_sig, SR_i)
!
      use t_repart_double_numberings
      use calypso_SR_type
      use solver_SR_type
      use search_from_list
      use select_copy_from_recv
      use quicksort
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(node_ele_double_number), intent(in) :: org_iele_dbl
!
      integer(kind = kint), intent(in)                                  &
     &            :: ie_newdomain(ele%numele,ele%nnod_4_ele)
!
      type(element_data), intent(inout) :: new_ele
      type(works_for_ext_node_search), intent(inout) :: wk_ext
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint), allocatable :: i4_recv(:)
      integer(kind = kint) :: inod, k1
!
!
      allocate(i4_recv(ele_tbl%ntot_import))
!
!$omp parallel do
      do inod = 1, new_node%numnod
        wk_ext%inod_recv(inod) =   inod
      end do
!$omp end parallel do
      call SOLVER_SEND_RECV_int_type                                    &
     &  (new_node%numnod, new_comm, SR_sig, SR_i, wk_ext%inod_recv)
!
!$omp parallel workshare
      wk_ext%ie_tmp(1:new_ele%numele,1:new_ele%nnod_4_ele)              &
     &   =  new_ele%ie(1:new_ele%numele,1:new_ele%nnod_4_ele)
      new_ele%ie(1:new_ele%numele,1:new_ele%nnod_4_ele) = 0
!$omp end parallel workshare
!
      do k1 = 1, ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import, ie_newdomain(1,k1),        &
     &      i4_recv(1), SR_sig, SR_i)
!
!$omp parallel workshare
        wk_ext%ie_domain_recv(1:wk_ext%num_loop,k1)                     &
     &                    = i4_recv(1:wk_ext%num_loop)
!$omp end parallel workshare
      end do
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, org_iele_dbl%index(1),       &
     &    i4_recv(1), SR_sig, SR_i)
!$omp parallel workshare
      wk_ext%iele_org_local(1:wk_ext%num_loop)                          &
     &                    = i4_recv(1:wk_ext%num_loop)
!$omp end parallel workshare
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, org_iele_dbl%irank(1),       &
     &    i4_recv(1), SR_sig, SR_i)
!$omp parallel workshare
      wk_ext%iele_org_domain(1:wk_ext%num_loop)                         &
     &                    = i4_recv(1:wk_ext%num_loop)
!$omp end parallel workshare
!
      deallocate(i4_recv)
!
      end subroutine set_works_for_ext_node_search
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_dbl_idx_connect                                  &
     &         (new_numele, new_nnod_4_ele, conn_dbl)
!
      integer(kind = kint), intent(in) :: new_numele, new_nnod_4_ele
      type(double_indices_connectivity), intent(inout) :: conn_dbl
!
!
      allocate(conn_dbl%ie_local(new_numele,new_nnod_4_ele))
      allocate(conn_dbl%irank_e(new_numele,new_nnod_4_ele))
!
      if(new_numele .le. 0) return
!$omp parallel workshare
        conn_dbl%ie_local(1:new_numele,1:new_nnod_4_ele) =  0
        conn_dbl%irank_e(1:new_numele,1:new_nnod_4_ele) =  -1
!$omp end parallel workshare
!
      end subroutine alloc_dbl_idx_connect
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_dbl_idx_connect(conn_dbl)
!
      type(double_indices_connectivity), intent(inout) :: conn_dbl
!
      deallocate(conn_dbl%ie_local, conn_dbl%irank_e)
!
      end subroutine dealloc_dbl_idx_connect
!
! ----------------------------------------------------------------------
!
      subroutine set_dbl_index_in_ele_connect                           &
     &          (new_ele_comm, new_inod_dbl, new_iele_dbl,              &
     &           new_ele, conn_dbl, SR_sig, SR_i)
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: new_ele_comm
      type(node_ele_double_number), intent(in) :: new_inod_dbl
      type(node_ele_double_number), intent(in) :: new_iele_dbl
!
      type(element_data), intent(inout) :: new_ele
      type(double_indices_connectivity), intent(inout) :: conn_dbl
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      integer(kind = kint) :: inod, iele, k1
!
!
      do k1 = 1, new_ele%nnod_4_ele
        call SOLVER_SEND_RECV_int_type(new_ele%numele, new_ele_comm,    &
     &                                 SR_sig, SR_i, new_ele%ie(1,k1))
      end do
!
!$omp parallel workshare
      conn_dbl%ie_local(1:new_ele%numele,1:new_ele%nnod_4_ele) = 0
      conn_dbl%irank_e(1:new_ele%numele,1:new_ele%nnod_4_ele) = -1
!$omp end parallel workshare
!
!$omp parallel do private(k1,iele,inod)
      do k1 = 1, new_ele%nnod_4_ele
        do iele = 1, new_ele%numele
          if(new_iele_dbl%irank(iele) .eq. my_rank) then
            inod = new_ele%ie(iele,k1)
            conn_dbl%ie_local(iele,k1) = new_inod_dbl%index(inod)
            conn_dbl%irank_e(iele,k1) =  new_inod_dbl%irank(inod)
          end if
        end do
      end do
!$omp end parallel do
!
      do k1 = 1, new_ele%nnod_4_ele
        call SOLVER_SEND_RECV_int_type(new_ele%numele, new_ele_comm,    &
     &      SR_sig, SR_i, conn_dbl%ie_local(1,k1))
        call SOLVER_SEND_RECV_int_type(new_ele%numele, new_ele_comm,    &
     &      SR_sig, SR_i, conn_dbl%irank_e(1,k1))
      end do
!
      end subroutine set_dbl_index_in_ele_connect
!
! ----------------------------------------------------------------------
!
      end module t_works_for_ext_node_search
