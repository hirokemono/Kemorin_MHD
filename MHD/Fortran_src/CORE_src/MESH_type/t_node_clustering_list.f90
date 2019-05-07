!>@file  t_node_clustering_list.f90
!!       module t_node_clustering_list
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Neighbouring node and element list for each node
!!
!!@verbatim
!!      subroutine alloc_istack_grouped(cluster)
!!      subroutine alloc_node_clustering_list(internal_node, cluster)
!!      subroutine dealloc_node_clustering_list(cluster)
!!      subroutine dup_node_clustering_list                             &
!!     &         (internal_node, org_cluster, new_cluster)
!!      subroutine expand_node_clustering                               &
!!     &         (node, neib_nod, cluster, extended)
!!@endverbatim
!
      module t_node_clustering_list
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type node_clustering_list
        integer(kind = kint) :: num_gruped_nod
        integer(kind = kint), allocatable :: istack_grouped(:)
        integer(kind = kint), allocatable :: inod_grouped(:)
!
        integer(kind = kint), allocatable :: inod_list(:)
        integer(kind = kint), allocatable :: igrp_by_nod(:)
      end type node_clustering_list
!
      private :: expand_grouping_to_next
      private :: count_expanded_group_num
      private :: count_expanded_group_stack
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_istack_grouped(cluster)
!
      type(node_clustering_list), intent(inout) :: cluster
!
!
      allocate(cluster%istack_grouped(0:cluster%num_gruped_nod))
      allocate(cluster%inod_grouped(cluster%num_gruped_nod))
!
!
      cluster%istack_grouped(0) = 0
      if(cluster%num_gruped_nod .le. 0) return
!$omp parallel workshare
      cluster%istack_grouped(1:cluster%num_gruped_nod) = 0
      cluster%inod_grouped(1:cluster%num_gruped_nod) = 0
!$omp end parallel workshare
!
      end subroutine alloc_istack_grouped
!
!-----------------------------------------------------------------------
!
      subroutine alloc_node_clustering_list(internal_node, cluster)
!
      integer(kind = kint), intent(in) :: internal_node
      type(node_clustering_list), intent(inout) :: cluster
!
!
      allocate(cluster%inod_list(internal_node))
      allocate(cluster%igrp_by_nod(internal_node))
!
      if(internal_node .le. 0) return
!$omp parallel workshare
      cluster%inod_list(0:internal_node) = 0
      cluster%igrp_by_nod(0:internal_node) = 0
!$omp end parallel workshare
!
      end subroutine alloc_node_clustering_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_node_clustering_list(cluster)
!
      type(node_clustering_list), intent(inout) :: cluster
!
!
      deallocate(cluster%inod_list, cluster%igrp_by_nod)
      deallocate(cluster%istack_grouped, cluster%inod_grouped)
!
      end subroutine dealloc_node_clustering_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dup_node_clustering_list                               &
     &         (internal_node, org_cluster, new_cluster)
!
      integer(kind = kint), intent(in) :: internal_node
      type(node_clustering_list), intent(in) :: org_cluster
      type(node_clustering_list), intent(inout) :: new_cluster
!
      new_cluster%num_gruped_nod = org_cluster%num_gruped_nod
!
      call alloc_istack_grouped(new_cluster)
!$omp parallel workshare
      new_cluster%istack_grouped(0:new_cluster%num_gruped_nod)          &
     &      = org_cluster%istack_grouped(0:new_cluster%num_gruped_nod)
!$omp end parallel workshare
!
      call alloc_node_clustering_list(internal_node, new_cluster)
!
      if(internal_node .le. 0) return
!$omp parallel workshare
      new_cluster%inod_list(1:internal_node)                            &
     &        = org_cluster%inod_list(internal_node)
      new_cluster%igrp_by_nod(1:internal_node)                          &
     &        = org_cluster%igrp_by_nod(internal_node)
!$omp end parallel workshare
!
      end subroutine dup_node_clustering_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine expand_node_clustering                                 &
     &         (node, neib_nod, cluster, extended)
!
      use t_geometry_data
      use t_next_node_ele_4_node
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(node_clustering_list), intent(in) :: cluster
!
      type(node_clustering_list), intent(inout) :: extended
!
      integer(kind = kint), allocatable :: istack_tmp(:)
!
!
      call alloc_node_clustering_list(node%internal_node, extended)
!
      allocate(istack_tmp(0:cluster%num_gruped_nod))
!
      call expand_grouping_to_next(node%internal_node,                  &
     &    neib_nod%ntot, neib_nod%istack_next, neib_nod%inod_next,      &
     &    cluster%num_gruped_nod, cluster%inod_grouped,                 &
     &    cluster%istack_grouped, cluster%inod_list,                    &
     &    cluster%igrp_by_nod,  istack_tmp, extended%inod_list,         &
     &    extended%igrp_by_nod)
      call sort_grouping_list                                           &
     &   (node%internal_node, cluster%num_gruped_nod,                   &
     &    istack_tmp, extended%inod_list)
!
      call count_expanded_group_num                                     &
     &   (cluster%num_gruped_nod, istack_tmp, extended%num_gruped_nod)
!
      call alloc_istack_grouped(extended)
!
      call count_expanded_group_stack(node%internal_node,               &
     &    cluster%num_gruped_nod, istack_tmp, extended%num_gruped_nod,  &
     &    extended%inod_list, extended%inod_grouped,                    &
     &    extended%istack_grouped)
!
      deallocate(istack_tmp)
!
      end subroutine expand_node_clustering
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine expand_grouping_to_next                                &
     &        (internal_node, ntot_next, istack_next, inod_next,        &
     &         num_gruped_nod, inod_grouped, istack_grouped,            &
     &         inod_list, igrp_by_nod, istack_tmp,                      &
     &         inod_list2, igrp_by_nod2)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: ntot_next
      integer (kind=kint), intent(in) :: istack_next(0:internal_node)
      integer (kind=kint), intent(in) :: inod_next(ntot_next)
!
      integer(kind = kint), intent(in) :: num_gruped_nod
      integer(kind = kint), intent(in) :: inod_grouped(num_gruped_nod)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_grouped(0:num_gruped_nod)
      integer(kind = kint), intent(in) :: inod_list(internal_node)
      integer(kind = kint), intent(in) :: igrp_by_nod(internal_node)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_tmp(0:num_gruped_nod)
      integer(kind = kint), intent(inout) :: inod_list2(internal_node)
      integer(kind = kint), intent(inout)                               &
     &              :: igrp_by_nod2(internal_node)
!
      integer(kind = kint), allocatable :: iflag_nod(:)
!
      integer(kind = kint) :: inum, inod, icou
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: knum, knod, kst, ked, kgrp
      integer(kind = kint) :: lnum, lnod, lst, led
!
!
      allocate(iflag_nod(internal_node))
!
!$omp parallel workshare
      iflag_nod(1:internal_node) = 0
!$omp end parallel workshare
!
      icou = 0
      do inum = 1, num_gruped_nod
        inod = inod_grouped(inum)
        if(iflag_nod(inod) .ne. 0) then
          istack_tmp(inum) = icou
          cycle
        end if
!
        jst = istack_grouped(inum-1) + 1
        jed = istack_grouped(inum  )
        do jnum = jst, jed
          jnod = inod_list(jnum)
          if(iflag_nod(jnod) .gt. 0) cycle
!
          icou = icou + 1
          inod_list2(icou) = jnod
          igrp_by_nod2(jnod) = inum
          iflag_nod(jnod) = 1
        end do
!
        do jnum = jst, jed
          jnod = inod_list(jnum)
          if(iflag_nod(jnod) .gt. 0) cycle
          if(jnod .gt. internal_node) cycle
!
          kst = istack_next(jnod-1) + 1
          ked = istack_next(jnod  )
          do knum = kst, ked
            knod = inod_next(knum)
            if(knod .gt. internal_node) cycle
            if(iflag_nod(knod) .gt. 0) cycle
!
            kgrp = igrp_by_nod(knod)
            lst = istack_grouped(kgrp-1) + 1
            led = istack_grouped(kgrp  )
            do lnum = lst, led
              lnod = inod_list(lnum)
!
              if(lnod .gt. internal_node) cycle
              if(iflag_nod(lnod) .gt. 0) cycle

              icou = icou + 1
              inod_list2(icou) = lnod
              igrp_by_nod2(lnod) = inum
              iflag_nod(lnod) = 1
            end do
          end do
        end do
!
        istack_tmp(inum) = icou
      end do
!
      deallocate(iflag_nod)
!
      end subroutine expand_grouping_to_next
!
!-----------------------------------------------------------------------
!
      subroutine sort_grouping_list(internal_node, num_gruped_nod,      &
     &          istack_tmp, inod_list2)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: num_gruped_nod
      integer(kind = kint), intent(in) :: istack_tmp(0:num_gruped_nod)
!
      integer(kind = kint), intent(inout) :: inod_list2(internal_node)
!
      integer(kind = kint) :: inum, ist, num
!
!$omp parallel do private(inum,ist,num)
      do inum = 1, num_gruped_nod
        ist = istack_tmp(inum-1)
        num = istack_tmp(inum) - istack_tmp(inum-1)
        if(num .gt. 0) then
          call quicksort_int(num, inod_list2(ist+1), ione, num)
        end if
      end do
!$omp end parallel do
!
      end subroutine sort_grouping_list
!
!-----------------------------------------------------------------------
!
      subroutine count_expanded_group_num                               &
     &         (num_gruped_nod, istack_tmp, num_gruped_nod2)
!
      integer(kind = kint), intent(in) :: num_gruped_nod
      integer(kind = kint), intent(in) :: istack_tmp(0:num_gruped_nod)
!
      integer(kind = kint), intent(inout) :: num_gruped_nod2
!
      integer(kind = kint) :: inum, icou, num
!
      icou = 0
      do inum = 1, num_gruped_nod
        num = istack_tmp(inum) - istack_tmp(inum-1)
        if(num .gt. 0) icou = icou + 1
      end do
      num_gruped_nod2 = icou
!
      end subroutine count_expanded_group_num
!
!-----------------------------------------------------------------------
!
      subroutine count_expanded_group_stack                             &
     &         (internal_node, num_gruped_nod, istack_tmp,              &
     &          num_gruped_nod2, inod_list2,                            &
     &          inod_grouped2, istack_grouped2)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: num_gruped_nod
      integer(kind = kint), intent(in) :: istack_tmp(0:num_gruped_nod)
      integer(kind = kint), intent(in) :: inod_list2(internal_node)
!
      integer(kind = kint), intent(inout) :: num_gruped_nod2
      integer(kind = kint), intent(inout)                               &
     &              :: inod_grouped2(num_gruped_nod2)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_grouped2(0:num_gruped_nod2)
!
      integer(kind = kint) :: inum, icou, ist, num
!
!
      icou = 0
      istack_grouped2(0) = 0
      do inum = 1, num_gruped_nod
        ist = istack_tmp(inum-1)
        num = istack_tmp(inum) - istack_tmp(inum-1)
        if(num .gt. 0) then
          icou = icou + 1
          inod_grouped2(icou) = inod_list2(ist+1)
          istack_grouped2(icou) = istack_tmp(inum)
        end if
      end do
!
      end subroutine count_expanded_group_stack
!
!-----------------------------------------------------------------------
!
      end module  t_node_clustering_list
