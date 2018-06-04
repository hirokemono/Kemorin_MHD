!> @file  extend_comm_table.f90
!!      module extend_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!@endverbatim
!
      module extend_comm_table
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use t_comm_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine extend_node_comm_table(nod_comm, node, neib_nod)
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use m_merged_ucd_data
      use solver_SR_type
      use extend_comm_table_SR
      use mark_export_nod_ele_extend
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
!>      Structure of double numbering
      type(parallel_double_numbering) :: dbl_id1
!
      integer(kind = kint), allocatable :: num_send_added(:)
      integer(kind = kint), allocatable :: istack_send_added(:)
!
      integer(kind = kint) :: ntot_send_added
      integer(kind = kint), allocatable :: inod_send_added(:)
      integer(kind = kint), allocatable :: irank_send_added(:)
      integer(kind = kint), allocatable :: iflag_send_2_del(:)
      integer(kind = kint_gl), allocatable :: inod_gl_send_added(:)
      real(kind = kreal), allocatable :: xx_send_added(:,:)
!
      integer(kind = kint), allocatable :: num_recv_added(:)
      integer(kind = kint), allocatable :: istack_recv_added(:)
!
      integer(kind = kint), allocatable :: ntot_recv_added
      integer(kind = kint), allocatable :: inod_recv_added(:)
      integer(kind = kint), allocatable :: irank_recv_added(:)
      integer(kind = kint), allocatable :: iflag_recv_2_del(:)
      integer(kind = kint_gl), allocatable :: inod_gl_recv_added(:)
      real(kind = kreal), allocatable :: xx_recv_added(:,:)
!
      integer(kind = kint), allocatable :: iflag_node(:)
      integer(kind = kint), allocatable :: iflag_ele(:)
!
      integer(kind = kint) :: inum, inod, i, ist, ied, icou
      integer(kind = kint) :: jnum, jnod, j, jst, jed
!
!
      call alloc_double_numbering(node%numnod, dbl_id1)
      call set_para_double_numbering(nod_comm, dbl_id1)
      call calypso_mpi_barrier
!
      allocate(iflag_node(node%numnod))
      iflag_node(1:node%numnod) = 0
!
      allocate(num_send_added(nod_comm%num_neib))
      allocate(istack_send_added(0:nod_comm%num_neib))
      if(nod_comm%num_neib .gt. 0) num_send_added = 0
      istack_send_added = 0
!
      do i = 1, nod_comm%num_neib
        call mark_next_node_of_export(i, nod_comm%num_neib,             &
     &      nod_comm%istack_import, nod_comm%item_import,               &
     &      nod_comm%istack_export, nod_comm%item_export,               &
     &      node%numnod, neib_nod%ntot, neib_nod%istack_next,           &
     &      neib_nod%inod_next, iflag_node)
!
        do inod = 1, node%numnod
          num_send_added(i) = num_send_added(i) + iflag_node(inod)
        end do
        istack_send_added(i) = istack_send_added(i-1)                   &
     &                          + num_send_added(i)
      end do
!
      ntot_send_added = istack_send_added(nod_comm%num_neib)
!
      allocate(inod_send_added(ntot_send_added))
      allocate(xx_send_added(ntot_send_added,3))
      allocate(irank_send_added(ntot_send_added))
      allocate(iflag_send_2_del(ntot_send_added))
      allocate(inod_gl_send_added(ntot_send_added))
!
!$omp parallel workshare
      xx_send_added(1:ntot_send_added,1) = 0.0d0
      xx_send_added(1:ntot_send_added,2) = 0.0d0
      xx_send_added(1:ntot_send_added,3) = 0.0d0
      inod_send_added(1:ntot_send_added) =     0
      irank_send_added(1:ntot_send_added) =    0
      inod_gl_send_added(1:ntot_send_added) =  0
      iflag_send_2_del(1:ntot_send_added) =    0
!$omp end parallel workshare
!
      do i = 1, nod_comm%num_neib
        call mark_next_node_of_export(i, nod_comm%num_neib,             &
     &      nod_comm%istack_import, nod_comm%item_import,               &
     &      nod_comm%istack_export, nod_comm%item_export,               &
     &      node%numnod, neib_nod%ntot, neib_nod%istack_next,           &
     &      neib_nod%inod_next, iflag_node)
!
        icou = istack_send_added(i-1)
        do inod = 1, node%numnod
          if(iflag_node(inod) .gt. 0) then
            icou = icou + 1
            inod_send_added(icou) =    dbl_id1%inod_local(inod)
            irank_send_added(icou) =   dbl_id1%irank_home(inod)
            inod_gl_send_added(icou) = node%inod_global(inod)
            xx_send_added(icou,1) =    node%xx(inod,1)
            xx_send_added(icou,2) =    node%xx(inod,2)
            xx_send_added(icou,3) =    node%xx(inod,3)
          end if
        end do
      end do
!
!
      allocate(num_recv_added(nod_comm%num_neib))
      allocate(istack_recv_added(0:nod_comm%num_neib))
      if(nod_comm%num_neib .gt. 0) num_recv_added = 0
      istack_recv_added = 0
!
      call SOLVER_SEND_RECV_num_type                                    &
     &   (nod_comm, num_send_added, num_recv_added)
!
      do i = 1, nod_comm%num_neib
        istack_recv_added(i) = istack_recv_added(i-1)                   &
     &                          + num_recv_added(i)
      end do
      ntot_recv_added = istack_recv_added(nod_comm%num_neib)
!
      write(*,*) 'istack_send_added', my_rank, istack_send_added
      write(*,*) 'ntot_send_added', my_rank, ntot_send_added
      write(*,*) 'istack_recv_added', my_rank, istack_recv_added
      write(*,*) 'ntot_recv_added', my_rank, ntot_recv_added
!
      allocate(inod_recv_added(ntot_recv_added))
      allocate(irank_recv_added(ntot_recv_added))
      allocate(iflag_recv_2_del(ntot_recv_added))
      allocate(inod_gl_recv_added(ntot_recv_added))
      allocate(xx_recv_added(ntot_recv_added,3))
!
!$omp parallel workshare
      xx_recv_added(1:ntot_recv_added,1) = 0.0d0
      xx_recv_added(1:ntot_recv_added,2) = 0.0d0
      xx_recv_added(1:ntot_recv_added,3) = 0.0d0
      inod_recv_added(1:ntot_recv_added) =     0
      irank_recv_added(1:ntot_recv_added) =    0
      inod_gl_recv_added(1:ntot_recv_added) =  0
      iflag_recv_2_del(1:ntot_recv_added) =    0
!$omp end parallel workshare
!
      call added_geometry_send_recv(nod_comm,                           &
     &    istack_send_added, ntot_send_added, xx_send_added,            &
     &    istack_recv_added, ntot_recv_added, xx_recv_added)
      call added_global_id_send_recv(nod_comm,                          &
     &    istack_send_added, ntot_send_added, inod_gl_send_added,       &
     &    istack_recv_added, ntot_recv_added, inod_gl_recv_added)
      call added_nod_id_send_recv(nod_comm,                             &
     &    istack_send_added, ntot_send_added, inod_send_added,          &
     &    istack_recv_added, ntot_recv_added, inod_recv_added)
      call added_nod_id_send_recv(nod_comm,                             &
     &    istack_send_added, ntot_send_added, irank_send_added,         &
     &    istack_recv_added, ntot_recv_added, irank_recv_added)
!
!
      do i = 1, nod_comm%num_neib
        ist = istack_recv_added(i-1) + 1
        ied = istack_recv_added(i)
!
        do inum = ist, ied
          if(irank_recv_added(inum) .eq. nod_comm%id_neib(i)) cycle
!
          do j = 1, nod_comm%num_neib
            if(irank_recv_added(inum) .ne. nod_comm%id_neib(j)) cycle
!
            jst = istack_recv_added(i-1) + 1
            jed = istack_recv_added(i)
            do jnum = jst, jed
              if(inod_recv_added(jnum) .eq. inod_recv_added(inum)) then
                iflag_recv_2_del(inum) = 1
                exit
              end if
            end do
            if(iflag_recv_2_del(inum) .gt. 0) exit
          end do
          if(iflag_recv_2_del(inum) .gt. 0) exit
        end do
      end do
!
      do inum = 1, ntot_recv_added
        if(iflag_recv_2_del(inum) .gt. 0) write(*,*) 'recv delete', my_rank, &
     &      inum, irank_recv_added(inum), inod_recv_added(inum)
      end do
!
      call added_nod_id_send_recv(nod_comm,                             &
     &    istack_recv_added, ntot_recv_added, iflag_recv_2_del,         &
     &    istack_send_added, ntot_send_added, iflag_send_2_del)
!
      do inum = 1, ntot_send_added
        if(iflag_send_2_del(inum) .gt. 0) write(*,*) 'send delete', my_rank, &
     &      inum, irank_send_added(inum), inod_send_added(inum)
      end do
!
      end subroutine extend_node_comm_table
!
!  ---------------------------------------------------------------------
!
      subroutine extend_ele_comm_table                                  &
     &         (nod_comm, ele_comm, node, ele, neib_ele, neib_nod)
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use m_merged_ucd_data
      use solver_SR_type
      use extend_comm_table_SR
      use mark_export_nod_ele_extend
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
!>      Structure of double numbering
      type(parallel_double_numbering) :: dbl_id1
      type(parallel_double_numbering) :: dbl_ele
!
      integer(kind = kint), allocatable :: num_send_added(:)
      integer(kind = kint), allocatable :: istack_send_added(:)
!
      integer(kind = kint) :: ntot_send_added
      integer(kind = kint), allocatable :: iele_send_added(:)
      integer(kind = kint), allocatable :: irank_send_added(:)
      integer(kind = kint), allocatable :: iflag_send_2_del(:)
      integer(kind = kint_gl), allocatable :: iele_gl_send_added(:)
      integer(kind = kint), allocatable :: ie_send_added(:,:)
      integer(kind = kint), allocatable :: ip_send_added(:,:)
!
      integer(kind = kint), allocatable :: num_recv_added(:)
      integer(kind = kint), allocatable :: istack_recv_added(:)
!
      integer(kind = kint), allocatable :: ntot_recv_added
      integer(kind = kint), allocatable :: iele_recv_added(:)
      integer(kind = kint), allocatable :: irank_recv_added(:)
      integer(kind = kint), allocatable :: iflag_recv_2_del(:)
      integer(kind = kint_gl), allocatable :: iele_gl_recv_added(:)
      integer(kind = kint), allocatable :: ie_recv_added(:,:)
      integer(kind = kint), allocatable :: ip_recv_added(:,:)
!
      integer(kind = kint), allocatable :: iflag_node(:)
      integer(kind = kint), allocatable :: iflag_ele(:)
!
      integer(kind = kint) :: inum, inod, i, ist, ied, icou
      integer(kind = kint) :: jnum, jnod, j, jst, jed
!
      integer(kind = kint) :: iele, k1, i1, iflag
!
      call alloc_double_numbering(node%numnod, dbl_id1)
      call set_para_double_numbering(nod_comm, dbl_id1)
      call alloc_double_numbering(ele%numele, dbl_ele)
      call set_para_double_numbering(ele_comm, dbl_ele)
      call calypso_mpi_barrier
!
      allocate(iflag_node(node%numnod))
      allocate(iflag_ele(ele%numele))
      iflag_node(1:node%numnod) = 0
      iflag_ele(1:ele%numele) = 0
!
      allocate(num_send_added(nod_comm%num_neib))
      allocate(istack_send_added(0:nod_comm%num_neib))
      if(nod_comm%num_neib .gt. 0) num_send_added = 0
      istack_send_added = 0
!
      do i = 1, nod_comm%num_neib
        call mark_used_ele_of_export(i, nod_comm%num_neib,              &
     &      nod_comm%istack_import, nod_comm%item_import,               &
     &      nod_comm%istack_export, nod_comm%item_export,               &
     &      node%numnod, neib_ele%ntot, neib_ele%istack_4_node,         &
     &      neib_ele%iele_4_node, ele%numele, ele%nnod_4_ele, ele%ie,   &
     &      iflag_node, iflag_ele)
!
        do inod = 1, ele%numele
          num_send_added(i) = num_send_added(i) + iflag_ele(inod)
        end do
        istack_send_added(i) = istack_send_added(i-1)                   &
     &                          + num_send_added(i)
      end do
      ntot_send_added = istack_send_added(nod_comm%num_neib)
!
      write(*,*) 'istack_send_added ele', istack_send_added
!
      allocate(iele_send_added(ntot_send_added))
      allocate(ie_send_added(ntot_send_added,ele%nnod_4_ele))
      allocate(ip_send_added(ntot_send_added,ele%nnod_4_ele))
      allocate(irank_send_added(ntot_send_added))
      allocate(iflag_send_2_del(ntot_send_added))
      allocate(iele_gl_send_added(ntot_send_added))
!
!$omp parallel workshare
      ie_send_added(1:ntot_send_added,1:ele%nnod_4_ele) = 0
      ip_send_added(1:ntot_send_added,1:ele%nnod_4_ele) = 0
!$omp end parallel workshare
!$omp parallel workshare
      iele_send_added(1:ntot_send_added) =     0
      irank_send_added(1:ntot_send_added) =    0
      iele_gl_send_added(1:ntot_send_added) =  0
      iflag_send_2_del(1:ntot_send_added) =    0
!$omp end parallel workshare
!
      do i = 1, nod_comm%num_neib
        call mark_used_ele_of_export(i, nod_comm%num_neib,              &
     &      nod_comm%istack_import, nod_comm%item_import,               &
     &      nod_comm%istack_export, nod_comm%item_export,               &
     &      node%numnod, neib_ele%ntot, neib_ele%istack_4_node,         &
     &      neib_ele%iele_4_node, ele%numele, ele%nnod_4_ele, ele%ie,   &
     &      iflag_node, iflag_ele)
!
        icou = istack_send_added(i-1)
        do iele = 1, ele%numele
          if(iflag_ele(iele) .gt. 0) then
            icou = icou + 1
            iele_send_added(icou) =    dbl_ele%inod_local(iele)
            irank_send_added(icou) =   dbl_ele%irank_home(iele)
            iele_gl_send_added(icou) = ele%iele_global(iele)
            do k1 = 1, ele%nnod_4_ele
              inod = ele%ie(iele,k1)
              ie_send_added(icou,k1) = dbl_id1%inod_local(inod)
              ip_send_added(icou,k1) = dbl_id1%irank_home(inod)
            end do
          end if
        end do
      end do
!
!
      allocate(num_recv_added(nod_comm%num_neib))
      allocate(istack_recv_added(0:nod_comm%num_neib))
      if(nod_comm%num_neib .gt. 0) num_recv_added = 0
      istack_recv_added = 0
!
      call SOLVER_SEND_RECV_num_type                                    &
     &   (nod_comm, num_send_added, num_recv_added)
!
      do i = 1, nod_comm%num_neib
        istack_recv_added(i) = istack_recv_added(i-1)                   &
     &                          + num_recv_added(i)
      end do
      ntot_recv_added = istack_recv_added(nod_comm%num_neib)
!
      write(*,*) 'istack_send_added', my_rank, istack_send_added
      write(*,*) 'ntot_send_added', my_rank, ntot_send_added
      write(*,*) 'istack_recv_added', my_rank, istack_recv_added
      write(*,*) 'ntot_recv_added', my_rank, ntot_recv_added
!
      allocate(iele_recv_added(ntot_recv_added))
      allocate(irank_recv_added(ntot_recv_added))
      allocate(iflag_recv_2_del(ntot_recv_added))
      allocate(iele_gl_recv_added(ntot_recv_added))
      allocate(ie_recv_added(ntot_recv_added,ele%nnod_4_ele))
      allocate(ip_recv_added(ntot_recv_added,ele%nnod_4_ele))
!
!$omp parallel workshare
      iele_recv_added(1:ntot_recv_added) =     0
      irank_recv_added(1:ntot_recv_added) =    0
      iele_gl_recv_added(1:ntot_recv_added) =  0
      iflag_recv_2_del(1:ntot_recv_added) =    0
!$omp end parallel workshare
!$omp parallel workshare
      ie_recv_added(1:ntot_recv_added,1:ele%nnod_4_ele) = 0
      ip_recv_added(1:ntot_recv_added,1:ele%nnod_4_ele) = 0
!$omp end parallel workshare
!
      call added_global_id_send_recv(nod_comm,                          &
     &    istack_send_added, ntot_send_added, iele_gl_send_added,       &
     &    istack_recv_added, ntot_recv_added, iele_gl_recv_added)
      call added_nod_id_send_recv(nod_comm,                             &
     &    istack_send_added, ntot_send_added, iele_send_added,          &
     &    istack_recv_added, ntot_recv_added, iele_recv_added)
      call added_nod_id_send_recv(nod_comm,                             &
     &    istack_send_added, ntot_send_added, irank_send_added,         &
     &    istack_recv_added, ntot_recv_added, irank_recv_added)
      do k1 = 1, ele%nnod_4_ele
        call added_nod_id_send_recv(nod_comm,                           &
     &      istack_send_added, ntot_send_added, ie_send_added(1,k1),    &
     &      istack_recv_added, ntot_recv_added, ie_recv_added(1,k1))
        call added_nod_id_send_recv(nod_comm,                           &
     &      istack_send_added, ntot_send_added, ip_send_added(1,k1),    &
     &      istack_recv_added, ntot_recv_added, ip_recv_added(1,k1))
      end do
!
      do i = 1, ntot_recv_added
        if(my_rank .eq. ip_recv_added(i,1)) then
          iflag_recv_2_del(i) = 1
          i1 = ie_recv_added(i,1)
          ist = neib_nod%istack_next(i1-1) + 1
          ied = neib_nod%istack_next(i1)
          do k1 = 2, ele%nnod_4_ele
            iflag = 0
            do inum = ist, ied
              inod = neib_nod%inod_next(inum)
              if(      ie_recv_added(i,k1).eq.dbl_id1%inod_local(inod)  &
    &            .and. ip_recv_added(i,k1).eq.dbl_id1%irank_home(inod)) &
    &          then
                iflag = 1
                exit
              end if
            end do
            iflag_recv_2_del(i) = iflag_recv_2_del(i) * iflag
          end do
        else
          do j = 1, nod_comm%num_neib
            if(nod_comm%id_neib(j) .ne. ip_recv_added(i,1)) cycle
!
            i1 = 0
            jst = nod_comm%istack_import(j-1) + 1
            jed = nod_comm%istack_import(j)
            do jnum = jst, jed
              inod = nod_comm%item_import(jnum)
              if(      ie_recv_added(i,1).eq.dbl_id1%inod_local(inod)   &
    &            .and. ip_recv_added(i,1).eq.dbl_id1%irank_home(inod))  &
    &          then
                i1 = inod
                exit
              end if
            end do
            if(i1 .eq. 0) cycle
!
            ist = neib_nod%istack_next(i1-1) + 1
            ied = neib_nod%istack_next(i1)
            do k1 = 2, ele%nnod_4_ele
              iflag = 0
              do inum = ist, ied
                inod = neib_nod%inod_next(inum)
                if(    ie_recv_added(i,k1).eq.dbl_id1%inod_local(inod)  &
    &            .and. ip_recv_added(i,k1).eq.dbl_id1%irank_home(inod)) &
    &            then
                  iflag = 1
                  exit
                end if
              end do
              iflag_recv_2_del(i) = iflag_recv_2_del(i) * iflag
            end do
          end do
        end if
          if(iflag_recv_2_del(i) .eq. 0) write(*,*) 'ie_send_added(iele,1)', my_rank, i, &
    &               irank_recv_added(i), iele_recv_added(i)
      end do
!
      call added_nod_id_send_recv(nod_comm,                             &
     &    istack_recv_added, ntot_recv_added, iflag_recv_2_del,         &
     &    istack_send_added, ntot_send_added, iflag_send_2_del)
!
!
      end subroutine extend_ele_comm_table
!
!  ---------------------------------------------------------------------
!
      end module extend_comm_table

