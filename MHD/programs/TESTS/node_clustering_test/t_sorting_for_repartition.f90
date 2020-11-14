!>@file   t_sorting_for_repartition.f90
!!@brief  module t_sorting_for_repartition
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Sort and trim overlapped node and element for repartition
!!
!!@verbatim
!!      subroutine alloc_sorting_data(ntot_import, sort_data)
!!      subroutine dealloc_sorting_data(sort_data)
!!        type(sorting_data_for_repartition), intent(inout) :: sort_data
!!
!!      subroutine sort_node_by_domain_and_index(numnod, internal_node, &
!!     &          idomain_recv, inod_recv, ext_tbl, sort_nod)
!!        type(calypso_comm_table), intent(inout) :: ext_tbl
!!        type(sorting_data_for_repartition), intent(inout) :: sort_nod
!!      subroutine trim_overlapped_ele_by_repart                        &
!!     &         (mesh, idomain_new, inod_new, iele_domain, iele_local, &
!!     &          ele_tbl, new_numele)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(calypso_comm_table), intent(inout) :: ele_tbl
!!
!!      subroutine check_push_off_redundant_ele(ele, ele_tbl, idx_sort2,&
!!     &         iele_local, iele_domain, iele_recv_org, irank_recv_org)
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!@endverbatim
!
      module t_sorting_for_repartition
!
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_calypso_comm_table
!
      implicit none
!
      type sorting_data_for_repartition
        integer(kind = kint) :: nprocs_in
        integer(kind = kint), allocatable :: num_send(:)
        integer(kind = kint), allocatable :: num_recv(:)
!
        integer(kind = kint) :: ntot
        integer(kind = kint), allocatable :: irank_sorted(:)
        integer(kind = kint), allocatable :: id_sorted(:)
        integer(kind = kint), allocatable :: idx_sort(:)
        integer(kind = kint), allocatable :: iflag_dup(:)
      end type sorting_data_for_repartition
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sorting_data(ntot_import, sort_data)
!
      integer(kind = kint), intent(in) :: ntot_import
      type(sorting_data_for_repartition), intent(inout) :: sort_data
!
      sort_data%nprocs_in = nprocs
      allocate(sort_data%num_send(sort_data%nprocs_in))
      allocate(sort_data%num_recv(sort_data%nprocs_in))
!$omp parallel workshare
      sort_data%num_send(1:sort_data%nprocs_in) = 0
      sort_data%num_recv(1:sort_data%nprocs_in) = 0
!$omp end parallel workshare
!
      sort_data%ntot = ntot_import
      allocate(sort_data%irank_sorted(sort_data%ntot))
      allocate(sort_data%id_sorted(sort_data%ntot))
      allocate(sort_data%idx_sort(sort_data%ntot))
      allocate(sort_data%iflag_dup(sort_data%ntot))
!
!$omp parallel workshare
      sort_data%irank_sorted(1:sort_data%ntot) = 0
      sort_data%id_sorted(1:sort_data%ntot) =    0
      sort_data%idx_sort(1:sort_data%ntot) =     0
      sort_data%iflag_dup(1:sort_data%ntot) =    0
!$omp end parallel workshare
!
      end subroutine alloc_sorting_data
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sorting_data(sort_data)
!
      type(sorting_data_for_repartition), intent(inout) :: sort_data
!
      deallocate(sort_data%irank_sorted, sort_data%id_sorted)
      deallocate(sort_data%idx_sort,     sort_data%iflag_dup)
      deallocate(sort_data%num_send,     sort_data%num_recv)
!
      end subroutine dealloc_sorting_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sort_node_by_domain_and_index(numnod, internal_node,   &
     &          idomain_recv, inod_recv, ext_tbl, sort_nod)
!
      use sort_for_repartition
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: idomain_recv(numnod)
      integer(kind = kint), intent(in) :: inod_recv(numnod)
!
      type(calypso_comm_table), intent(inout) :: ext_tbl
      type(sorting_data_for_repartition), intent(inout) :: sort_nod
!
      integer(kind = kint) :: i, j
!
!
!$omp parallel do private(i)
      do i = 1, ext_tbl%ntot_import
        sort_nod%idx_sort(i) =  ext_tbl%item_import(i)
      end do
!$omp end parallel do
!
      call sort_by_domain_and_index_list                                &
     &   (nprocs, (my_rank+1), ext_tbl%ntot_import,                     &
     &    idomain_recv(internal_node+1), inod_recv(internal_node+1),    &
     &    sort_nod%irank_sorted, sort_nod%id_sorted,                    &
     &    sort_nod%idx_sort, sort_nod%num_recv)
!
!$omp parallel do private(i,j)
      do i = 1, ext_tbl%ntot_import
        j = sort_nod%idx_sort(i)
        ext_tbl%item_import(j) = i
        ext_tbl%irev_import(i) = j
      end do
!$omp end parallel do
!
      end subroutine sort_node_by_domain_and_index
!
! ----------------------------------------------------------------------
!
      subroutine trim_overlapped_ele_by_repart                          &
     &         (mesh, idomain_new, inod_new, iele_domain, iele_local,   &
     &          ele_tbl, new_numele)
!
      use calypso_SR_type
      use select_copy_from_recv
      use sort_for_repartition
!
      type(mesh_geometry), intent(in) :: mesh
      integer(kind = kint), intent(in) :: idomain_new(mesh%node%numnod)
      integer(kind = kint), intent(in) :: inod_new(mesh%node%numnod)
!
      integer(kind = kint), intent(in) :: iele_domain(mesh%ele%numele)
      integer(kind = kint), intent(in) :: iele_local(mesh%ele%numele)
!
      integer(kind = kint), intent(inout) :: new_numele
      type(calypso_comm_table), intent(inout) :: ele_tbl
!
      type(sorting_data_for_repartition) :: sort_ele
!
      integer(kind = kint), allocatable :: iele_recv(:)
      integer(kind = kint), allocatable :: idomain_recv(:)
!
      integer(kind = kint), allocatable :: idx_sort2(:)
!
      integer(kind = kint) :: iele
!
!
      allocate(iele_recv(ele_tbl%ntot_import))
      allocate(idomain_recv(ele_tbl%ntot_import))
!
!    Set local in external node
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    mesh%ele%numele, ele_tbl%ntot_import,                         &
     &    iele_domain, idomain_recv)
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    mesh%ele%numele, ele_tbl%ntot_import,                         &
     &    iele_local, iele_recv)
!
      call alloc_sorting_data(ele_tbl%ntot_import, sort_ele)
!
!$omp parallel do
      do iele = 1, ele_tbl%ntot_import
        sort_ele%idx_sort(iele) =   iele
      end do
!$omp end parallel do
!
      call sort_by_domain_and_index_list                                &
     &   (nprocs, izero, ele_tbl%ntot_import, idomain_recv, iele_recv,  &
     &    sort_ele%irank_sorted, sort_ele%id_sorted,                    &
     &    sort_ele%idx_sort, sort_ele%num_recv)
!
      call mark_overlapped_import_ele(nprocs, ele_tbl%ntot_import,      &
     &    sort_ele%num_recv, sort_ele%id_sorted, sort_ele%iflag_dup)
!
!
      allocate(idx_sort2(ele_tbl%ntot_import))
!$omp parallel workshare
      idx_sort2(1:ele_tbl%ntot_import) = 0
!$omp end parallel workshare
!
      call push_off_redundant_element(nprocs, sort_ele%num_recv,        &
     &    ele_tbl%ntot_import, sort_ele%idx_sort, sort_ele%iflag_dup,   &
     &    ele_tbl%item_import, ele_tbl%irev_import,                     &
     &    idx_sort2, new_numele)
!      call check_push_off_redundant_ele(mesh%ele, ele_tbl, idx_sort2,  &
!     &   iele_local, iele_domain, iele_recv, idomain_recv)
      call dealloc_sorting_data(sort_ele)
      deallocate(idx_sort2)
      deallocate(idomain_recv, iele_recv)
!
      end subroutine trim_overlapped_ele_by_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_push_off_redundant_ele(ele, ele_tbl, idx_sort2,  &
     &         iele_local, iele_domain, iele_recv_org, irank_recv_org)
!
      use calypso_SR_type
      use select_copy_from_recv
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      integer(kind = kint), intent(in) :: idx_sort2(ele_tbl%ntot_import)
!
      integer(kind = kint), intent(in) :: iele_local(ele%numele)
      integer(kind = kint), intent(in) :: iele_domain(ele%numele)
      integer(kind = kint), intent(in)                                  &
     &            :: iele_recv_org(ele_tbl%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &            :: irank_recv_org(ele_tbl%ntot_import)
!
      integer(kind = kint), allocatable :: iele_recv2(:)
      integer(kind = kint), allocatable :: idomain_recv2(:)
!
      integer(kind = kint) :: iele, icou
!
!
      allocate(iele_recv2(ele_tbl%ntot_import))
      allocate(idomain_recv2(ele_tbl%ntot_import))
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, iele_domain, idomain_recv2)
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, iele_local, iele_recv2)
!
      write(100+my_rank,*) 'irank_recv_org, iele_recv_org',             &
     &                    ele_tbl%ntot_import
      do icou = 1, ele_tbl%ntot_import
          iele = idx_sort2(icou)
          write(100+my_rank,*) icou, 'ele_tbl%item_import', iele,       &
     &       irank_recv_org(iele), iele_recv_org(iele)
        end do
!
      write(100+my_rank,*) 'check pushing off redundant element'
      do icou = 1, ele_tbl%ntot_import
        iele = idx_sort2(icou)
        if(idomain_recv2(icou) .eq. irank_recv_org(iele))  cycle
        if(iele_recv2(icou) .eq. iele_recv_org(iele))  cycle
!
        write(100+my_rank,*) my_rank, icou, idx_sort2(icou),            &
     &             idomain_recv2(icou)-irank_recv_org(iele),            &
     &             iele_recv2(icou)-iele_recv_org(iele)
      end do
      deallocate(idomain_recv2, iele_recv2)
!
      end subroutine check_push_off_redundant_ele
!
! ----------------------------------------------------------------------
!
      end module t_sorting_for_repartition
