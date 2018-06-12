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
!!@endverbatim
!
      module mark_export_nod_ele_extend
!
      use m_precision
      use calypso_mpi
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mark_next_node_of_export(id_neib, num_neib,            &
     &         istack_import, item_import, istack_export, item_export,  &
     &         numnod, ntot_next, istack_next, inod_next, iflag_node)
!
      integer(kind = kint), intent(in) :: id_neib, num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                     :: item_import(istack_import(num_neib))
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                     :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: ntot_next
      integer(kind = kint), intent(in) :: istack_next(0:numnod)
      integer(kind = kint), intent(in) :: inod_next(ntot_next)
!
      integer(kind = kint), intent(inout) :: iflag_node(numnod)
!
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, jed, jnum, jnod
!
!
!$omp parallel workshare
      iflag_node(1:numnod) = 0
!$omp end parallel workshare
!
      ist = istack_export(id_neib-1) + 1
      ied = istack_export(id_neib)
      do inum = ist, ied
        inod = item_export(inum)
        jst = istack_next(inod-1) + 1
        jed = istack_next(inod)
        do jnum = jst, jed
          jnod = inod_next(jnum)
          iflag_node(jnod) = 1
        end do
      end do
!
      do inum = ist, ied
        inod = item_export(inum)
        iflag_node(inod) = 0
      end do
!
      ist = istack_import(id_neib-1) + 1
      ied = istack_import(id_neib)
      do inum = ist, ied
        inod = item_import(inum)
        iflag_node(inod) = 0
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
     &          numele, nnod_4_ele, ie, iflag_node, iflag_ele)
!
      integer(kind = kint), intent(in) :: id_neib, num_neib
      integer(kind = kint), intent(in) :: istack_export_new(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                 :: item_export_new(istack_export_new(num_neib))
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                     :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: ntot_ele
      integer(kind = kint), intent(in) :: istack_4_node(0:numnod)
      integer(kind = kint), intent(in) :: iele_4_node(ntot_ele)
!
      integer(kind = kint), intent(inout) :: iflag_node(numnod)
      integer(kind = kint), intent(inout) :: iflag_ele(numele)
!
      integer(kind = kint) :: ist, ied, inum, inod, iflag
      integer(kind = kint) :: jst, jed, jnum, jele, k1
!
!
!$omp parallel workshare
      iflag_node(1:numnod) = 0
!$omp end parallel workshare
!$omp parallel workshare
      iflag_ele(1:numele) = 0
!$omp end parallel workshare
!
      ist = istack_export_new(id_neib-1) + 1
      ied = istack_export_new(id_neib)
      do inum = ist, ied
        inod = item_export_new(inum)
        iflag_node(inod) = 1
      end do
!
      ist = istack_export(id_neib-1) + 1
      ied = istack_export(id_neib)
      do inum = ist, ied
        inod = item_export(inum)
        iflag_node(inod) = 0
      end do
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
      return
!
!$omp parallel do private(jele,k1,inod,iflag)
      do jele = 1, numele
        if(iflag_ele(jele) .eq. 0) cycle
!
        iflag = 0
        do k1 = 1, nnod_4_ele
          inod = ie(jele,k1)
          if(my_rank .eq. 0 .and. jele .eq. 1)  &
     &         write(*,*) 'test', k1, inod, iflag_node(inod)
          if(iflag_node(inod) .gt. 0) iflag = 1
        end do
        iflag_ele(jele) = iflag_ele(jele) * iflag
      end do
!$omp end parallel do
!
      end subroutine mark_used_ele_of_export
!
!  ---------------------------------------------------------------------
!
      subroutine mark_added_import_to_delete                            &
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
      end subroutine mark_added_import_to_delete
!
!  ---------------------------------------------------------------------
!
      end module mark_export_nod_ele_extend
