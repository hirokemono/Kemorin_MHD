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
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!        type(communication_table), intent(in) :: new_comm
!!        type(node_data), intent(in) :: new_node
!!        type(node_ele_double_number), intent(in) :: org_iele_dbl
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
     &              :: ie_newdomain(ele%numele,ele%nnod_4_ele)
!
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint), allocatable :: inod_recv(:)
      integer(kind = kint), allocatable :: icount_node(:)
!      integer(kind = kint), allocatable :: item_import_recv(:)
!
      integer(kind = kint), allocatable :: iele_org_local(:)
      integer(kind = kint), allocatable :: iele_org_domain(:)
      integer(kind = kint), allocatable :: ie_domain_recv(:,:)
!
      integer(kind = kint), allocatable :: ie_tmp(:,:)
      integer(kind = kint), allocatable :: i4_recv(:)
!
      integer(kind = kint), allocatable :: num_rev_import_recv(:)
      integer(kind = kint), allocatable :: istack_rev_import_recv(:)
      integer(kind = kint), allocatable :: irank_import_recv(:)
      integer(kind = kint), allocatable :: irev_import(:)
      integer(kind = kint) :: nmax_import
!
      integer(kind = kint) :: inod, icou, iele, k1, num_loop
!
!
      num_loop = min(new_ele%numele, ele_tbl%ntot_import)
!
      allocate(inod_recv(new_node%numnod))
!
      allocate(ie_tmp(new_ele%numele,new_ele%nnod_4_ele))
      allocate(i4_recv(ele_tbl%ntot_import))
      allocate(ie_domain_recv(new_ele%numele,new_ele%nnod_4_ele))
!
      allocate(iele_org_local(new_ele%numele))
      allocate(iele_org_domain(new_ele%numele))
!
      call set_works_for_ext_node_search                                &
     &   (ele, ele_tbl, org_iele_dbl, ie_newdomain,                     &
     &    new_comm, new_node, new_ele, num_loop, inod_recv,             &
     &    iele_org_local, iele_org_domain, ie_domain_recv,              &
     &    ie_tmp, i4_recv, SR_sig, SR_i)
!
!      allocate(item_import_recv(new_comm%ntot_import))
!      call set_item_import_recv(new_comm, new_node%numnod,             &
!     &                          inod_recv, item_import_recv)
!
      nmax_import = maxval(inod_recv)
      allocate(num_rev_import_recv(nmax_import))
      allocate(istack_rev_import_recv(0:nmax_import))
      istack_rev_import_recv(0) = 0
!$omp parallel workshare
      istack_rev_import_recv(1:nmax_import) = 0
      num_rev_import_recv(1:nmax_import) =    0
!$omp end parallel workshare
!
      call count_new_comm_irev_import                                   &
     &   (new_comm, nmax_import, new_node%numnod, inod_recv,            &
     &    num_rev_import_recv, istack_rev_import_recv)
!
      allocate(irank_import_recv(new_comm%ntot_import))
      allocate(irev_import(new_comm%ntot_import))
!
      call set_new_comm_irev_import(new_comm, nmax_import,              &
     &    istack_rev_import_recv, new_node%numnod, inod_recv,           &
     &    num_rev_import_recv, irev_import, irank_import_recv)
!
!
!
      allocate(icount_node(new_node%numnod))
!$omp parallel workshare
      icount_node(1:new_node%numnod) = 0
!$omp end parallel workshare
!
      icou = 0
      do iele = 1, num_loop
        do k1 = 1, new_ele%nnod_4_ele
          new_ele%ie(iele,k1) = search_repart_external_node             &
     &                       (ie_tmp(iele,k1), ie_domain_recv(iele,k1), &
     &                        my_rank, new_comm, nmax_import,           &
     &                        istack_rev_import_recv, irev_import,      &
     &                        irank_import_recv)
!
          inod = new_ele%ie(iele,k1)
          if(inod .le. 0) then
            write(*,*) my_rank, 'Node cannot be found for ',            &
     &         new_ele%iele_global(iele), iele, k1,                     &
     &         ie_domain_recv(iele,k1), ie_tmp(iele,k1),                &
     &         iele_org_local(iele), iele_org_domain(iele)
            icou = icou + 1
          else
            icount_node(inod) = icount_node(inod) + 1
          end if
        end do
      end do
!      deallocate(item_import_recv)
      deallocate(irev_import, irank_import_recv)
      deallocate(istack_rev_import_recv, num_rev_import_recv)
      deallocate(i4_recv, ie_domain_recv)
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
! ----------------------------------------------------------------------
!
      subroutine set_works_for_ext_node_search                          &
     &         (ele, ele_tbl, org_iele_dbl, ie_newdomain,               &
     &          new_comm, new_node, new_ele, num_loop, inod_recv,       &
     &          iele_org_local, iele_org_domain, ie_domain_recv,        &
     &          ie_tmp, i4_recv, SR_sig, SR_i)
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
!
      integer(kind = kint), intent(inout) :: num_loop
      integer(kind = kint), intent(inout) :: inod_recv(new_node%numnod)
!
      integer(kind = kint), intent(inout)                               &
     &            :: iele_org_local(new_ele%numele)
      integer(kind = kint), intent(inout)                               &
     &            :: iele_org_domain(new_ele%numele)
      integer(kind = kint), intent(inout)                               &
     &            :: ie_domain_recv(new_ele%numele,new_ele%nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &            :: ie_tmp(new_ele%numele,new_ele%nnod_4_ele)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: i4_recv(ele_tbl%ntot_import)
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: inod, k1
!
!
!$omp parallel do
      do inod = 1, new_node%numnod
        inod_recv(inod) =   inod
      end do
!$omp end parallel do
      call SOLVER_SEND_RECV_int_type                                    &
     &  (new_node%numnod, new_comm, SR_sig, SR_i, inod_recv)
!
!$omp parallel workshare
      ie_tmp(1:new_ele%numele,1:new_ele%nnod_4_ele)                     &
     &   =  new_ele%ie(1:new_ele%numele,1:new_ele%nnod_4_ele)
      new_ele%ie(1:new_ele%numele,1:new_ele%nnod_4_ele) = 0
!$omp end parallel workshare
!
      do k1 = 1, ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import, ie_newdomain(1,k1),        &
     &      i4_recv(1), SR_sig, SR_i)
!$omp parallel workshare
        ie_domain_recv(1:num_loop,k1) = i4_recv(1:num_loop)
!$omp end parallel workshare
      end do
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, org_iele_dbl%index(1),       &
     &    i4_recv(1), SR_sig, SR_i)
!$omp parallel workshare
      iele_org_local(1:num_loop) = i4_recv(1:num_loop)
!$omp end parallel workshare
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, org_iele_dbl%irank(1),       &
     &    i4_recv(1), SR_sig, SR_i)
!$omp parallel workshare
      iele_org_domain(1:num_loop) = i4_recv(1:num_loop)
!$omp end parallel workshare
!
      end subroutine set_works_for_ext_node_search
!
! ----------------------------------------------------------------------
!
      subroutine set_dbl_index_in_ele_connect                           &
     &          (new_ele_comm, new_inod_dbl, new_iele_dbl,              &
     &           new_ele, ie_local, irank_e, SR_sig, SR_i)
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: new_ele_comm
      type(node_ele_double_number), intent(in) :: new_inod_dbl
      type(node_ele_double_number), intent(in) :: new_iele_dbl
!
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint), intent(inout)                               &
     &            :: ie_local(new_ele%numele,new_ele%nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &            :: irank_e(new_ele%numele,new_ele%nnod_4_ele)
!
      integer(kind = kint) :: inod, iele, k1, num_loop
!
!
      do k1 = 1, new_ele%nnod_4_ele
        call SOLVER_SEND_RECV_int_type(new_ele%numele, new_ele_comm,    &
     &                                 SR_sig, SR_i, new_ele%ie(1,k1))
      end do
!
!$omp parallel workshare
      ie_local(1:new_ele%numele,1:new_ele%nnod_4_ele) = 0
      irank_e(1:new_ele%numele,1:new_ele%nnod_4_ele) = -1
!$omp end parallel workshare
!
!$omp parallel do private(k1,iele,inod)
      do k1 = 1, new_ele%nnod_4_ele
        do iele = 1, new_ele%numele
          if(new_iele_dbl%irank(iele) .eq. my_rank) then
            inod = new_ele%ie(iele,k1)
            ie_local(iele,k1) = new_inod_dbl%index(inod)
            irank_e(iele,k1) =  new_inod_dbl%irank(inod)
          end if
        end do
      end do
!$omp end parallel do
!
      do k1 = 1, new_ele%nnod_4_ele
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, SR_sig, SR_i, ie_local(1,k1))
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, SR_sig, SR_i, irank_e(1,k1))
      end do
!
      end subroutine set_dbl_index_in_ele_connect
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_new_comm_irev_import                             &
     &         (new_comm, nmax_import, numnod_new, inod_recv,           &
     &          num_rev_import_recv, istack_rev_import_recv)
!
      use cal_minmax_and_stacks
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
      integer(kind = kint), intent(in) :: nmax_import
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_rev_import_recv(nmax_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_rev_import_recv(0:nmax_import)
!
      integer(kind = kint) :: inod, jnum, jnod, ntot_tmp
!
!
      istack_rev_import_recv(0:nmax_import) = 0
      num_rev_import_recv(1:nmax_import) = 0
!
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        inod = inod_recv(jnod)
        num_rev_import_recv(inod) = num_rev_import_recv(inod) + 1
      end do
!
      call s_cal_total_and_stacks(nmax_import, num_rev_import_recv,     &
     &    izero, istack_rev_import_recv, ntot_tmp)
!
      do inod = 1, nmax_import
        istack_rev_import_recv(inod) = istack_rev_import_recv(inod-1)   &
     &                                + num_rev_import_recv(inod)
      end do
!
      if(ntot_tmp .ne. new_comm%ntot_import) then
        write(*,*) 'Wrong total number of node stack'
      end if
!
      end subroutine count_new_comm_irev_import
!
! ----------------------------------------------------------------------
!
      subroutine set_new_comm_irev_import(new_comm, nmax_import,        &
     &          istack_rev_import_recv, numnod_new, inod_recv,          &
     &          num_rev_import_recv, irev_import, irank_import_recv)
!
      use quicksort
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: nmax_import
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_rev_import_recv(0:nmax_import)
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_rev_import_recv(nmax_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: irev_import(new_comm%ntot_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: irank_import_recv(new_comm%ntot_import)
!
      integer(kind = kint) :: ip, ist, ied, inod, jnum, jnod, jst, num
!
!
!$omp parallel workshare
      num_rev_import_recv(1:nmax_import) = 0
!$omp end parallel workshare
!
      do ip = 1, new_comm%num_neib
        ist = new_comm%istack_import(ip-1) + 1
        ied = new_comm%istack_import(ip  )
        do jnum = ist, ied
          jnod = new_comm%item_import(jnum)
          inod = inod_recv(jnod)
          jst = istack_rev_import_recv(inod-1)
          num = num_rev_import_recv(inod) + 1
!
          irank_import_recv(jst+num) =  new_comm%id_neib(ip)
          irev_import(jst+num) = jnum
          num_rev_import_recv(inod) = num
        end do
      end do
!
      do inod = 1, nmax_import
        ist = istack_rev_import_recv(inod-1) + 1
        ied = istack_rev_import_recv(inod  )
        if((ied-ist) .gt. 1) then
          call quicksort_w_index(new_comm%ntot_import,                  &
     &        irank_import_recv, ist, ied, irev_import)
        end if
      end do
!
      end subroutine set_new_comm_irev_import
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_repart_external_node         &
     &                           (inod, ip, id_rank, new_comm,          &
     &                            nmax_import, istack_rev_import_recv,  &
     &                            irev_import, irank_import_recv)
!
      use search_from_list
!
      integer(kind = kint), intent(in) :: ip, inod
      integer(kind = kint), intent(in) :: id_rank
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: nmax_import
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_rev_import_recv(0:nmax_import)
      integer(kind = kint), intent(in)                                  &
     &                     :: irev_import(new_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &                      :: irank_import_recv(new_comm%ntot_import)
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
        jst = istack_rev_import_recv(inod-1) + 1
        jed = istack_rev_import_recv(inod  )
!
        if((jed-jst) .ge. 0) then
          knum = search_from_sorted_data(ip, jst, jed,              &
     &                  new_comm%ntot_import, irank_import_recv)
          if(knum.ge.jst .and. knum.le.jed) then
            jnum = irev_import(knum)
            ie_new = new_comm%item_import(jnum)
          end if
        end if
      end if
      search_repart_external_node = ie_new
!
      end function search_repart_external_node
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_item_import_recv(new_comm, numnod_new,             &
     &                                inod_recv, item_import_recv)
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
      integer(kind = kint), intent(inout)                               &
     &                     :: item_import_recv(new_comm%ntot_import)
!
      integer(kind = kint) :: jnum, jnod
!
!$omp parallel do private(jnum,jnod)
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        item_import_recv(jnum) = inod_recv(jnod)
      end do
!$omp end parallel do
!
      end subroutine set_item_import_recv
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_repart_ext_node_old          &
     &                            (inod, ip, id_rank, item_import_recv, &
     &                             new_comm, ie_new)
!
      use search_from_list
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ip, inod
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in)                                  &
     &                     :: item_import_recv(new_comm%ntot_import)
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
        inum = search_from_list_data(ip, ione, new_comm%num_neib,  &
     &                         new_comm%num_neib, new_comm%id_neib)
        ist = 0
        ied = -1
        if(inum.ge.ione .and. inum.le.new_comm%num_neib) then
          ist = new_comm%istack_import(inum-1) + 1
          ied = new_comm%istack_import(inum)
        end if
!
        if(ied .ge. ist) then
          jnum = search_from_sorted_data(inod, ist, ied,           &
     &                      new_comm%ntot_import, item_import_recv)
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
