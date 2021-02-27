!> @file  mark_export_nod_ele_extend.f90
!!      module mark_export_nod_ele_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine mark_next_node_of_export(id_neib, num_neib,          &
!!     &         istack_import, item_import, istack_export, item_export,&
!!     &         numnod, ntot_next, istack_next, inod_next, iflag_node)
!!      subroutine mark_used_ele_of_export(id_neib, num_neib,           &
!!     &          istack_export_new, item_export_new,                   &
!!     &          istack_export, item_export,                           &
!!     &          numnod, ntot_ele, istack_4_node, iele_4_node,         &
!!     &          numele, nnod_4_ele, ie, iflag_node, iflag_ele)
!!
!!      subroutine mark_added_nod_import_to_del                         &
!!     &         (numnod, inod_local, irank_home, num_neib, id_neib,    &
!!     &          ntot_import, istack_import, item_import, num_neib_add,&
!!     &          id_neib_add, ntot_import_add, istack_import_add,      &
!!     &          inod_recv_add, irank_recv_add, iflag_recv_2_del)
!!      subroutine mark_added_ele_import_to_del                         &
!!     &         (num_pe, numele, iele_local, irank_home,               &
!!     &          num_neib_add, ntot_import_add, istack_import_add,     &
!!     &          iele_recv_add, irank_recv_add, iflag_recv_2_del)
!!@endverbatim
!
      module mark_export_nod_ele_extend
!
      use m_precision
      use t_next_node_ele_4_node
!
      implicit none
!
      integer(kind = kint), parameter, private :: many = 512
!
      type comm_table_for_each_pe
        integer(kind = kint) :: num_each_import = 0
        integer(kind = kint), allocatable :: item_each_import(:)
!
        integer(kind = kint) :: num_each_export = 0
        integer(kind = kint), allocatable :: item_each_export(:)
      end type comm_table_for_each_pe
!
      type mark_for_each_comm
        integer(kind = kint) :: nnod_marked = 0
        integer(kind = kint), allocatable :: inod_marked(:)
      end type mark_for_each_comm
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_comm_table_for_each(ineib, nod_comm, each_comm)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: ineib
      type(communication_table), intent(in) ::       nod_comm
      type(comm_table_for_each_pe), intent(inout) :: each_comm
!
      integer(kind = kint) :: ist, i
!
!
      each_comm%num_each_import = nod_comm%istack_import(ineib)         &
     &                           - nod_comm%istack_import(ineib-1)
      allocate(each_comm%item_each_import(each_comm%num_each_import))
!
      ist = nod_comm%istack_import(ineib-1) 
      do i = 1, each_comm%num_each_import
        each_comm%item_each_import(i) = nod_comm%item_import(i+ist)
      end do
!
      each_comm%num_each_export = nod_comm%istack_export(ineib)         &
     &                           - nod_comm%istack_export(ineib-1)
      allocate(each_comm%item_each_export(each_comm%num_each_export))
!
      ist = nod_comm%istack_export(ineib-1) 
      do i = 1, each_comm%num_each_export
        each_comm%item_each_export(i) = nod_comm%item_export(i+ist)
      end do
!
      end subroutine init_comm_table_for_each
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_comm_table_for_each(each_comm)
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
!
      deallocate(each_comm%item_each_import)
      deallocate(each_comm%item_each_export)
!
      end subroutine dealloc_comm_table_for_each
!
!  ---------------------------------------------------------------------
!
      subroutine mark_next_node_of_export                               &
     &         (neib_nod, each_comm, numnod, mark_nod, iflag_node)
!
      use quicksort
!
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(comm_table_for_each_pe), intent(in) :: each_comm
      integer(kind = kint), intent(in) :: numnod
!
      type(mark_for_each_comm), intent(inout) :: mark_nod
      integer(kind = kint), intent(inout) :: iflag_node(numnod)
!
      integer(kind = kint) :: inum, inod, icou
      integer(kind = kint) :: jst, jed, jnum, jnod
!
!
!$omp parallel workshare
      iflag_node(1:numnod) = 0
!$omp end parallel workshare
!
      do inum = 1, each_comm%num_each_export
        inod = each_comm%item_each_export(inum)
        jst = neib_nod%istack_next(inod-1) + 1
        jed = neib_nod%istack_next(inod)
!        if((jed-jst) .ge. many) cycle
!
        do jnum = jst, jed
          jnod = neib_nod%inod_next(jnum)
          iflag_node(jnod) = 1
        end do
      end do
!
      do inum = 1, each_comm%num_each_export
        inod = each_comm%item_each_export(inum)
        iflag_node(inod) = 0
      end do
!
      do inum = 1, each_comm%num_each_import
        inod = each_comm%item_each_import(inum)
        iflag_node(inod) = 0
      end do
!
      mark_nod%nnod_marked = 0
      do inod = 1, numnod
        if(iflag_node(inod) .gt. 0) then
          mark_nod%nnod_marked = mark_nod%nnod_marked + 1
        end if
      end do
      allocate(mark_nod%inod_marked(inod))
!
      icou = 0
      do inod = 1, numnod
        if(iflag_node(inod) .gt. 0) then
          icou = icou + 1
          mark_nod%inod_marked(icou) = inod
        end if
      end do
!
      end subroutine mark_next_node_of_export
!
!  ---------------------------------------------------------------------
!
      subroutine mark_used_ele_of_export(id_neib, num_neib,             &
     &          istack_export_new, item_export_new,                     &
     &          istack_export, item_export,                             &
     &          numnod, ntot_ele, istack_4_node, iele_4_node,           &
     &          numele, iflag_ele)
!
      integer(kind = kint), intent(in) :: id_neib, num_neib
      integer(kind = kint), intent(in) :: istack_export_new(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                 :: item_export_new(istack_export_new(num_neib))
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                     :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(in) :: numele, numnod
      integer(kind = kint), intent(in) :: ntot_ele
      integer(kind = kint), intent(in) :: istack_4_node(0:numnod)
      integer(kind = kint), intent(in) :: iele_4_node(ntot_ele)
!
      integer(kind = kint), intent(inout) :: iflag_ele(numele)
!
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, jed, jnum, jele
!
!
!$omp parallel workshare
      iflag_ele(1:numele) = 0
!$omp end parallel workshare
!
      ist = istack_export(id_neib-1) + 1
      ied = istack_export(id_neib)
      do inum = ist, ied
        inod = item_export(inum)
        jst = istack_4_node(inod-1) + 1
        jed = istack_4_node(inod)
        do jnum = jst, jed
          jele = iele_4_node(jnum)
          iflag_ele(jele) = 1
        end do
      end do
!
      end subroutine mark_used_ele_of_export
!
!  ---------------------------------------------------------------------
!
      subroutine mark_added_nod_import_to_del                           &
     &         (numnod, inod_local, irank_home, num_neib, id_neib,      &
     &          ntot_import, istack_import, item_import, num_neib_add,  &
     &          id_neib_add, ntot_import_add, istack_import_add,        &
     &          inod_recv_add, irank_recv_add, iflag_recv_2_del)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_local(numnod)
      integer(kind = kint), intent(in) :: irank_home(numnod)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_import
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: item_import(ntot_import)
!
      integer(kind = kint), intent(in) :: num_neib_add, ntot_import_add
      integer(kind = kint), intent(in) :: id_neib_add(num_neib_add)
      integer(kind = kint), intent(in)                                  &
     &            :: istack_import_add(0:num_neib_add)
!
      integer(kind = kint), intent(in)                                  &
     &            :: inod_recv_add(ntot_import_add)
      integer(kind = kint), intent(in)                                  &
     &            :: irank_recv_add(ntot_import_add)
!
      integer(kind = kint), intent(inout)                               &
     &            :: iflag_recv_2_del(ntot_import_add)
!
      integer(kind = kint) :: i, ist, ied, inum
      integer(kind = kint) :: j, jst, jed, jnum, jnod
!
!
      do i = 1, num_neib_add
        ist = istack_import_add(i-1) + 1
        ied = istack_import_add(i)
!
        do inum = ist, ied
          if(iflag_recv_2_del(inum) .lt. 0) cycle
          if(irank_recv_add(inum) .eq. id_neib_add(i)) cycle
!
          do j = 1, num_neib_add
            if(i .eq. j) cycle
!
            jst = istack_import_add(j-1) + 1
            jed = istack_import_add(j)
            do jnum = jst, jed
              if(   irank_recv_add(jnum) .eq. irank_recv_add(inum)      &
     &         .and. inod_recv_add(jnum) .eq. inod_recv_add(inum)       &
     &         .and. iflag_recv_2_del(jnum) .eq. 0) then
                iflag_recv_2_del(inum) = -1
                exit
              end if
            end do
            if(iflag_recv_2_del(inum) .lt. 0) exit
          end do
        end do
      end do
!
      do i = 1, num_neib
        ist = istack_import_add(i-1) + 1
        ied = istack_import_add(i)
!
        do inum = ist, ied
          if(iflag_recv_2_del(inum) .lt. 0) cycle
          if(irank_recv_add(inum) .eq. id_neib(i)) cycle
!
          do jnum = 1, istack_import(num_neib)
            jnod = item_import(jnum)
            if(    irank_home(jnod) .eq. irank_recv_add(inum)           &
     &       .and. inod_local(jnod) .eq. inod_recv_add(inum)) then
              iflag_recv_2_del(inum) = -1
              exit
            end if
          end do
        end do
      end do
!
      end subroutine mark_added_nod_import_to_del
!
!  ---------------------------------------------------------------------
!
      subroutine mark_added_ele_import_to_del                           &
     &         (num_pe, numele, iele_local, irank_home,                 &
     &          num_neib_add, ntot_import_add, istack_import_add,       &
     &          iele_recv_add, irank_recv_add, iflag_recv_2_del)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: iele_local(numele)
      integer(kind = kint), intent(in) :: irank_home(numele)
!
      integer(kind = kint), intent(in) :: num_neib_add, ntot_import_add
      integer(kind = kint), intent(in)                                  &
     &            :: istack_import_add(0:num_neib_add)
!
      integer(kind = kint), intent(in)                                  &
     &            :: iele_recv_add(ntot_import_add)
      integer(kind = kint), intent(in)                                  &
     &            :: irank_recv_add(ntot_import_add)
!
      integer(kind = kint), intent(inout)                               &
     &            :: iflag_recv_2_del(ntot_import_add)
!
      integer(kind = kint), allocatable :: istack_ele_ip(:)
      integer(kind = kint), allocatable :: iele_by_ip(:)
!
      integer(kind = kint) :: i, ist, ied, icou, iele, ip
      integer(kind = kint) :: jst, jed, jnum, jele
!
!
      allocate(istack_ele_ip(0:num_pe))
      allocate(iele_by_ip(numele))
      istack_ele_ip = 0
      iele_by_ip =    0
!
      icou = 0
      do ip = 1, num_pe
        do iele = 1, numele
          if(irank_home(iele) .eq. (ip-1)) then
            icou = icou + 1
            iele_by_ip(icou) = iele
          end if
        end do
        istack_ele_ip(ip) = icou
      end do
!
      do i = 1, num_neib_add
        ist = istack_import_add(i-1) + 1
        ied = istack_import_add(i)
        do iele = ist, ied
          ip = irank_recv_add(iele) + 1
          jst = istack_ele_ip(ip-1) + 1
          jed = istack_ele_ip(ip)
          do jnum = jst, jed
            jele = iele_by_ip(jnum)
            if(iele_recv_add(iele) .eq. iele_local(jele)) then
              iflag_recv_2_del(iele) = -1
              exit
            end if
          end do
!
        end do
      end do
!
      do i = 1, num_neib_add
        ist = istack_import_add(i-1) + 1
        ied = istack_import_add(i)
        do iele = ist, ied
          if(iflag_recv_2_del(iele) .lt. 0) cycle
!
          do jele = 1, istack_import_add(num_neib_add)
            if(iele .eq. jele) cycle
            if(   irank_recv_add(jele) .eq. irank_recv_add(iele)        &
     &       .and. iele_recv_add(jele) .eq. iele_recv_add(iele)         &
     &       .and. iflag_recv_2_del(jele) .eq. 0) then
              iflag_recv_2_del(iele) = -1
              exit
            end if
          end do
        end do
      end do
!
      deallocate(istack_ele_ip, iele_by_ip)
!
      end subroutine mark_added_ele_import_to_del
!
!  ---------------------------------------------------------------------
!
      end module mark_export_nod_ele_extend
