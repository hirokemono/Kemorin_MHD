!>@file  t_multi_node_clustering.f90
!!       module t_multi_node_clustering
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Neighbouring node and element list for each node
!!
!!@verbatim
!!      subroutine dealloc_mul_node_clusterings(clusters)
!!      subroutine const_multi_node_clusters                            &
!!     &         (ilevel_end, node, neib_nod, clusters)
!!        type(node_data), intent(in) :: node
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(mul_node_clusterings), intent(in) :: clusters
!!@endverbatim
!
      module t_multi_node_clustering
!
      use m_precision
      use m_constants
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_node_clustering_list
!
      implicit none
!
!
      type mul_node_clusterings
        integer(kind = kint) :: num_cluster
        type(node_clustering_list), allocatable :: cluster(:)
      end type mul_node_clusterings
!
      integer(kind = kint), parameter :: max_grp = 100
!
      private :: append_extended_cluster, init_grouping_list
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_mul_node_clusterings(clusters)
!
      type(mul_node_clusterings), intent(inout) :: clusters
!
!
      deallocate(clusters%cluster)
!
      end subroutine dealloc_mul_node_clusterings
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_multi_node_clusters                              &
     &         (ilevel_end, mesh, neib_nod, clusters)
!
      use int_volume_of_single_domain
      use copy_mesh_structures
!
      integer(kind = kint), intent(in) :: ilevel_end
      type(mesh_geometry), intent(in) :: mesh
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(mul_node_clusterings), intent(inout) :: clusters
!
      type(node_clustering_list) :: first_ctr
      real(kind = kreal), allocatable :: volume_nod(:)
      integer(kind = kint) :: i
!
!
      write(*,*) 'estimate node volume'
      allocate(volume_nod(mesh%node%numnod))
      call cal_node_volue                                               &
     &   (mesh%node, mesh%ele, volume_nod)
!
      call copy_node_geometry_types(mesh%node, first_ctr%cluster_nod)
!
      clusters%num_cluster = 1
      call alloc_mul_node_clusterings(clusters)
!
      first_ctr%num_gruped_nod = mesh%node%internal_node
      call alloc_node_clustering_list                                   &
     &   (mesh%node%internal_node, first_ctr)
      call alloc_istack_grouped(first_ctr)
!
      write(*,*) 'init_grouping_list'
      call init_grouping_list                                           &
     &   (first_ctr%num_gruped_nod, first_ctr%istack_grouped,           &
     &    first_ctr%inod_list, &
     &    first_ctr%igrp_by_nod)
!
      write(*,*) 'expand_node_clustering'
      call expand_node_clustering(mesh%node, volume_nod, neib_nod,      &
     &    first_ctr, clusters%cluster(1))
      call dealloc_node_clustering_list(first_ctr)
!
      do i = 2, ilevel_end
        if(clusters%cluster(clusters%num_cluster)%num_gruped_nod        &
     &       .le. max_grp) exit
        write(*,*) 'append_extended_cluster', i
        call append_extended_cluster                                    &
     &     (mesh%node, volume_nod, neib_nod, clusters)
      end do
      deallocate(volume_nod)
!
      end subroutine const_multi_node_clusters
!
!-----------------------------------------------------------------------
!
      subroutine append_extended_cluster                                &
     &         (node, volume_nod, neib_nod, clusters)
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      real(kind = kreal), intent(in) :: volume_nod(node%numnod)
!
      type(mul_node_clusterings), intent(inout) :: clusters
!
      type(mul_node_clusterings) :: tmp_clusters
!
      integer(kind = kint) :: i
!
!
      tmp_clusters%num_cluster = clusters%num_cluster
      call alloc_mul_node_clusterings(tmp_clusters)
!
      do i = clusters%num_cluster, 1, -1
        call dup_node_clustering_list(node%internal_node,               &
     &      clusters%cluster(i), tmp_clusters%cluster(i))
        call dealloc_node_clustering_list(clusters%cluster(i))
      end do
      call dealloc_mul_node_clusterings(clusters)
!
      clusters%num_cluster = clusters%num_cluster + 1
      call alloc_mul_node_clusterings(clusters)
!
      do i = 1, clusters%num_cluster-1
        call dup_node_clustering_list(node%internal_node,               &
     &      tmp_clusters%cluster(i), clusters%cluster(i))
        call dealloc_node_clustering_list(tmp_clusters%cluster(i))
      end do
      call dealloc_mul_node_clusterings(tmp_clusters)
!
      call expand_node_clustering(node, volume_nod, neib_nod,           &
     &    clusters%cluster(clusters%num_cluster-1),                     &
     &    clusters%cluster(clusters%num_cluster))
!
      end subroutine append_extended_cluster
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_mul_node_clusterings(clusters)
!
      type(mul_node_clusterings), intent(inout) :: clusters
!
!
      allocate(clusters%cluster(clusters%num_cluster))
!
      end subroutine alloc_mul_node_clusterings
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine init_grouping_list(num_gruped_nod,                     &
     &          istack_grouped, inod_list, igrp_by_nod)
!
      integer(kind = kint), intent(in) :: num_gruped_nod
      integer(kind = kint), intent(inout)                               &
     &              :: istack_grouped(0:num_gruped_nod)
      integer(kind = kint), intent(inout) :: inod_list(num_gruped_nod)
      integer(kind = kint), intent(inout)                               &
     &              :: igrp_by_nod(num_gruped_nod)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private(inod)
      do inod = 1, num_gruped_nod
        inod_list(inod) =      inod
        igrp_by_nod(inod) =    inod
        istack_grouped(inod) = inod
      end do
!$omp end parallel do
!
      end subroutine init_grouping_list
!
!-----------------------------------------------------------------------
!
      end module  t_multi_node_clustering
