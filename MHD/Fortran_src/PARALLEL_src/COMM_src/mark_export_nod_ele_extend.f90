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
!!     &         istack_import, item_import, istack_export, item_export,&
!!     &         numnod, ntot_ele, istack_4_node, iele_4_node,          &
!!     &         numele, nnod_4_ele, ie, iflag_node, iflag_ele)
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
     &         istack_export_new, item_export_new, istack_export, item_export,  &
     &         numnod, ntot_ele, istack_4_node, iele_4_node,            &
     &         numele, nnod_4_ele, ie, iflag_node, iflag_ele)
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
      end module mark_export_nod_ele_extend
