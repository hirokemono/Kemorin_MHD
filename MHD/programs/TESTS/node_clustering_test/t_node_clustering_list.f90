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
      use t_geometry_data
!
      implicit none
!
!
      type node_clustering_list
        integer(kind = kint) :: num_gruped_nod
        integer(kind = kint), allocatable :: istack_grouped(:)
        integer(kind = kint), allocatable :: irank_list(:)
!
        integer(kind = kint), allocatable :: inod_list(:)
        integer(kind = kint), allocatable :: igrp_by_nod(:)
!
        type(node_data) :: cluster_nod
        real(kind = kreal), allocatable :: volume_grp(:)
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
      allocate(cluster%irank_list(cluster%num_gruped_nod))
!
      cluster%istack_grouped(0) = 0
      if(cluster%num_gruped_nod .le. 0) return
!$omp parallel workshare
      cluster%istack_grouped(1:cluster%num_gruped_nod) = 0
!
      cluster%irank_list(1:cluster%num_gruped_nod) =  0
!      cluster%xx_group(1:cluster%num_gruped_nod) =    0
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
      cluster%inod_list(1:internal_node) =   0
      cluster%igrp_by_nod(1:internal_node) = 0
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
      deallocate(cluster%istack_grouped)
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
      new_cluster%istack_grouped(0) = org_cluster%istack_grouped(0)
!$omp parallel workshare
      new_cluster%istack_grouped(1:new_cluster%num_gruped_nod)          &
     &      = org_cluster%istack_grouped(1:new_cluster%num_gruped_nod)
!$omp end parallel workshare
!
      call alloc_node_clustering_list(internal_node, new_cluster)
!
      if(internal_node .le. 0) return
!$omp parallel workshare
      new_cluster%inod_list(1:internal_node)                            &
     &        = org_cluster%inod_list(1:internal_node)
      new_cluster%igrp_by_nod(1:internal_node)                          &
     &        = org_cluster%igrp_by_nod(1:internal_node)
!$omp end parallel workshare
!
      end subroutine dup_node_clustering_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine expand_node_clustering                                 &
     &         (node, volume_nod, neib_nod, cluster, extended)
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use cal_mesh_position
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(node_clustering_list), intent(in) :: cluster
      real(kind = kreal), intent(in) :: volume_nod(node%numnod)
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
     &    cluster%num_gruped_nod,  cluster%istack_grouped,              &
     &    cluster%inod_list, cluster%igrp_by_nod,                       &
     &    istack_tmp,  extended%inod_list)
      call sort_grouping_list                                           &
     &   (node%internal_node, cluster%num_gruped_nod,                   &
     &    istack_tmp, extended%inod_list)
!
      call count_expanded_group_num                                     &
     &   (cluster%num_gruped_nod, istack_tmp, extended%num_gruped_nod)
!
      call alloc_istack_grouped(extended)
!
      call count_expanded_group_stack                                   &
     &   (node%internal_node, cluster%num_gruped_nod, istack_tmp,       &
     &    extended%num_gruped_nod, extended%inod_list,                  &
     &    extended%istack_grouped, extended%igrp_by_nod)
!
      deallocate(istack_tmp)
!
      extended%cluster_nod%numnod = extended%num_gruped_nod
      extended%cluster_nod%internal_node = extended%num_gruped_nod
      call alloc_node_geometry_w_sph(extended%cluster_nod)
      allocate(extended%volume_grp(extended%num_gruped_nod))
      extended%volume_grp(1:extended%num_gruped_nod) = 0.0d0
!
      call set_clusterd_postiion(node, volume_nod,                      &
     &    extended%num_gruped_nod, extended%istack_grouped,             &
     &    extended%inod_list, extended%volume_grp, extended%cluster_nod)
!
      call set_spherical_position(extended%cluster_nod)
!
      end subroutine expand_node_clustering
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine expand_grouping_to_next                                &
     &        (internal_node, ntot_next, istack_next, inod_next,        &
     &         num_gruped_nod, istack_grouped, inod_list,               &
     &         igrp_by_nod, istack_tmp, inod_list2)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: ntot_next
      integer (kind=kint), intent(in) :: istack_next(0:internal_node)
      integer (kind=kint), intent(in) :: inod_next(ntot_next)
!
      integer(kind = kint), intent(in) :: num_gruped_nod
      integer(kind = kint), intent(in)                                  &
     &              :: istack_grouped(0:num_gruped_nod)
      integer(kind = kint), intent(in) :: inod_list(internal_node)
      integer(kind = kint), intent(in) :: igrp_by_nod(internal_node)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_tmp(0:num_gruped_nod)
      integer(kind = kint), intent(inout) :: inod_list2(internal_node)
!
      integer(kind = kint), allocatable :: iflag_grp(:)
!
      integer(kind = kint) :: igrp, icou
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: knum, knod, kst, ked, kgrp
      integer(kind = kint) :: lnum, lnod, lst, led
!
!
      allocate(iflag_grp(num_gruped_nod))
!$omp parallel workshare
      iflag_grp(1:num_gruped_nod) = 0
!$omp end parallel workshare
!
      istack_tmp(0) = 0
      icou = 0
      do igrp = 1, num_gruped_nod
        if(iflag_grp(igrp) .gt. 0) then
          istack_tmp(igrp) = icou
          cycle
        end if
!
        jst = istack_grouped(igrp-1) + 1
        jed = istack_grouped(igrp)
        do jnum = jed, jst, -1
          jnod = inod_list(jnum)
          kst = istack_next(jnod-1) + 1
          ked = istack_next(jnod  )
          do knum = kst, ked
            knod = inod_next(knum)
            kgrp = igrp_by_nod(knod)
            if(knod .gt. internal_node) cycle
            if(iflag_grp(kgrp) .gt. 0) cycle
!
            lst = istack_grouped(kgrp-1) + 1
            led = istack_grouped(kgrp)
            do lnum = lst, led
              icou = icou + 1
              lnod = inod_list(lnum)
              inod_list2(icou) = lnod
            end do
            iflag_grp(kgrp) = 1
          end do
        end do
        istack_tmp(igrp) = icou
      end do
!
      deallocate(iflag_grp)
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
!!$omp parallel do private(inum,ist,num)
      do inum = 1, num_gruped_nod
        ist = istack_tmp(inum-1)
        num = istack_tmp(inum) - istack_tmp(inum-1)
        if(num .gt. 0) then
          call quicksort_int(num, inod_list2(ist+1), ione, num)
        end if
      end do
!!$omp end parallel do
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
     &          num_gruped_nod2, inod_list2, istack_grouped2,           &
     &          igrp_by_nod2)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: num_gruped_nod
      integer(kind = kint), intent(in) :: istack_tmp(0:num_gruped_nod)
      integer(kind = kint), intent(in) :: inod_list2(internal_node)
!
      integer(kind = kint), intent(inout) :: num_gruped_nod2
      integer(kind = kint), intent(inout)                               &
     &              :: istack_grouped2(0:num_gruped_nod2)
      integer(kind = kint), intent(inout) :: igrp_by_nod2(internal_node)
!
      integer(kind = kint) :: inum, icou, ist, num, ied, inod
!
!
      icou = 0
      do inum = 1, num_gruped_nod
        ist = istack_tmp(inum-1)
        num = istack_tmp(inum) - istack_tmp(inum-1)
        if(num .gt. 0) then
          icou = icou + 1
          istack_grouped2(icou) = istack_tmp(inum)
        end if
      end do
!
      do icou = 1, num_gruped_nod2
        ist =  istack_grouped2(icou-1) + 1
        ied =  istack_grouped2(icou)
        do inum = ist, ied
          inod = inod_list2(inum)
          igrp_by_nod2(inod) = icou
        end do
      end do
!
      end subroutine count_expanded_group_stack
!
!-----------------------------------------------------------------------
!
      subroutine set_clusterd_postiion(node, volume_nod,                &
     &          num_gruped_nod2, istack_grouped2, inod_list2,           &
     &          volume_grp2, cluster_nod2)
!
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: volume_nod(node%numnod)
!
      integer(kind = kint), intent(in) :: num_gruped_nod2
      integer(kind = kint), intent(in)                                  &
     &              :: istack_grouped2(0:num_gruped_nod2)
      integer(kind = kint), intent(in)                                  &
     &              :: inod_list2(node%internal_node)
!
      real(kind = kreal), intent(inout) :: volume_grp2(num_gruped_nod2)
      type(node_data), intent(inout) :: cluster_nod2
!
      integer(kind = kint) :: icou, ist, ied, num, inum, inod
!
!
      do icou = 1, num_gruped_nod2
        ist = istack_grouped2(icou-1) + 1
        ied = istack_grouped2(icou)
        num = istack_grouped2(icou) - istack_grouped2(icou-1)
        do inum = 1, num
          inod = inod_list2(ist+inum)
          volume_grp2(icou) = volume_grp2(icou) + volume_nod(inod)
          cluster_nod2%inod_global(icou) = icou
          cluster_nod2%xx(icou,1)                                       &
     &        = cluster_nod2%xx(icou,1) + node%xx(inod,1)
          cluster_nod2%xx(icou,2)                                       &
     &        = cluster_nod2%xx(icou,2) + node%xx(inod,2)
          cluster_nod2%xx(icou,3)                                       &
     &        = cluster_nod2%xx(icou,3) + node%xx(inod,3)
        end do
        cluster_nod2%xx(icou,1) = cluster_nod2%xx(icou,1) / dble(num)
        cluster_nod2%xx(icou,2) = cluster_nod2%xx(icou,2) / dble(num)
        cluster_nod2%xx(icou,3) = cluster_nod2%xx(icou,3) / dble(num)
      end do
!
      end subroutine set_clusterd_postiion!
!
!-----------------------------------------------------------------------
!
      end module  t_node_clustering_list
