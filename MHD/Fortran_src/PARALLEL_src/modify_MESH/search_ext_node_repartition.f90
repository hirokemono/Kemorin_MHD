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
!!     &         (ele, ele_tbl, element_ids, ie_newdomain,              &
!!     &          new_comm, new_node, new_ele)
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(in) :: new_node
!!        type(node_ele_double_number), intent(in) :: element_ids
!!        type(element_data), intent(inout) :: new_ele
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
     &         (ele, ele_tbl, element_ids, ie_newdomain,                &
     &          new_comm, new_node, new_ele)
!
      use t_para_double_numbering
      use t_repart_double_numberings
      use calypso_SR_type
      use solver_SR_type
      use search_from_list
      use select_copy_from_recv
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(node_ele_double_number), intent(in) :: element_ids
!
      integer(kind = kint), intent(in)                                  &
     &              :: ie_newdomain(ele%numele,ele%nnod_4_ele)
!
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint), allocatable :: inod_recv(:)
      integer(kind = kint), allocatable :: icount_node(:)
      integer(kind = kint), allocatable :: item_import_recv(:)
!
      integer(kind = kint), allocatable :: iele_org_local(:)
      integer(kind = kint), allocatable :: iele_org_domain(:)
      integer(kind = kint), allocatable :: ie_domain_recv(:,:)
!
      integer(kind = kint), allocatable :: i4_recv(:)
!
      integer(kind = kint) :: ip, inod, icou, inum, ist, ied
      integer(kind = kint) :: iele, k1, jnum, jnod, iflag
!
!
      allocate(i4_recv(ele_tbl%ntot_import))
      allocate(ie_domain_recv(new_ele%numele,new_ele%nnod_4_ele))
!
      do k1 = 1, ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import, ie_newdomain(1,k1),        &
     &      i4_recv(1))
!$omp parallel workshare
        ie_domain_recv(1:new_ele%numele,k1) = i4_recv(1:new_ele%numele)
!$omp end parallel workshare
      end do
!
      allocate(iele_org_local(new_ele%numele))
      allocate(iele_org_domain(new_ele%numele))
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, element_ids%index(1),        &
     &    i4_recv(1))
!$omp parallel workshare
      iele_org_local(1:new_ele%numele) = i4_recv(1:new_ele%numele)
!$omp end parallel workshare
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, element_ids%irank(1),        &
     &    i4_recv(1))
!$omp parallel workshare
      iele_org_domain(1:new_ele%numele) = i4_recv(1:new_ele%numele)
!$omp end parallel workshare
!
      allocate(inod_recv(new_node%numnod))
      allocate(icount_node(new_node%numnod))
!$omp parallel do
      do inod = 1, new_node%numnod
        inod_recv(inod) =   inod
        icount_node(inod) = 0
      end do
!$omp end parallel do
      call SOLVER_SEND_RECV_int_type                                    &
     &  (new_node%numnod, new_comm, inod_recv)
!
      allocate(item_import_recv(new_comm%ntot_import))
!$omp parallel do private(jnum,jnod)
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        item_import_recv(jnum) = inod_recv(jnod)
      end do
!$omp end parallel do
!
      icou = 0
      do iele = 1, new_ele%numele
        do k1 = 1, new_ele%nnod_4_ele
          ip =   ie_domain_recv(iele,k1)
          inod = new_ele%ie(iele,k1)
          iflag = 0
          if(ip .eq. my_rank) then
            icount_node(inod) = icount_node(inod) + 1
          else
            inum = search_from_list_data(ip, ione, new_comm%num_neib,   &
     &                             new_comm%num_neib, new_comm%id_neib)
            if(inum.ge.ione .and. inum.le.new_comm%num_neib) then
              ist = new_comm%istack_import(inum-1) + 1
              ied = new_comm%istack_import(inum)
            else
              ist = 0
              ied = 0
            end if
!
            if(ist .gt. 0) then
              jnum = search_from_sorted_data(inod, ist, ied,            &
     &                          new_comm%ntot_import, item_import_recv)
              if(jnum.ge.ist .and. jnum.le.ied) then
                jnod = new_comm%item_import(jnum)
                iflag = item_import_recv(jnum)
                new_ele%ie(iele,k1) = jnod
                icount_node(jnod) = icount_node(jnod) + 1
              end if
            end if
!
            if(iflag .le. 0) then
              new_ele%ie(iele,k1) = 0
              write(*,*) my_rank, 'Node cannot be found for ',         &
     &           new_ele%iele_global(iele), iele, k1, ip, inod,        &
     &           iele_org_local(iele), iele_org_domain(iele)
              icou = icou + 1
            end if
          end if
        end do
      end do
      deallocate(i4_recv, ie_domain_recv, item_import_recv)
      deallocate(iele_org_local, iele_org_domain, inod_recv)
!
      if(i_debug .gt. 0) then
        write(*,*) my_rank, 'Missing connectivity: ', icou,             &
     &          ' of ', new_ele%nnod_4_ele*new_ele%numele
!
        icou = 0
        do inod = 1, new_node%numnod
          if(icount_node(inod) .eq. 0) icou = icou + 1
        end do
        write(*,*) my_rank, 'Missing connenction: ', icou,              &
     &          ' of ', new_node%numnod
      end if
!
      deallocate(icount_node)
!
      end subroutine s_search_ext_node_repartition
!
! ----------------------------------------------------------------------
!
      end module search_ext_node_repartition
