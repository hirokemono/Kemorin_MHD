!> @file  t_trim_overlapped_import.f90
!!      module t_trim_overlapped_import
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine trim_overlapped_sleeve_ext(ntot_new_import,          &
!!     &          irank_nod_new_import, sort_import, ext_trim)
!!      subroutine dealloc_stack_to_trim_extend(ext_trim)
!!      subroutine dealloc_idx_trimed_to_sorted(ext_trim)
!!        integer(kind = kint), intent(in) :: ntot_new_import
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: irank_nod_new_import(ntot_new_import)
!!        type(sort_data_for_sleeve_trim), intent(in) :: sort_import
!!        type(data_for_trim_import), intent(inout) :: ext_trim
!!        type(data_for_trim_import), intent(inout) :: ext_trim
!!
!!      subroutine check_overlapped_sleeve_ext                          &
!!     &         (nod_comm, add_comm, sort_import, ext_trim)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(calypso_comm_table), intent(in) :: add_comm
!!        type(sort_data_for_sleeve_trim), intent(in) :: sort_import
!!        type(data_for_trim_import), intent(in) :: ext_trim
!!@endverbatim
!
      module t_trim_overlapped_import
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      type data_for_trim_import
        integer(kind = kint) :: ntot_trimmed
        integer(kind = kint), allocatable :: istack_trimmed_pe(:)
        integer(kind = kint), allocatable :: istack_trimmed_item(:)
!
        integer(kind = kint), allocatable :: idx_trimmed_to_sorted(:)
      end type data_for_trim_import
!
      type import_extend_to_trim
        integer(kind = kint), allocatable :: idx_extend_to_trim(:)
        integer(kind = kint), allocatable :: import_lc_trimmed(:)
      end type import_extend_to_trim
!
      private :: alloc_stack_to_trim_extend, alloc_idx_trimed_to_sorted
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine trim_overlap_expanded_import(ntot_new_import,          &
     &          irank_nod_new_import, sort_import, ext_trim)
!
      use calypso_mpi
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
!
      use calypso_mpi_int
      use trim_redundant_import_item
!
      integer(kind = kint), intent(in) :: ntot_new_import
      integer(kind = kint), intent(in)                                  &
     &                    :: irank_nod_new_import(ntot_new_import)
      type(sort_data_for_sleeve_trim), intent(in) :: sort_import
!
      type(data_for_trim_import), intent(inout) :: ext_trim
!
      integer(kind = kint) :: icou, ntot_gl
!
!
      ext_trim%ntot_trimmed                                             &
     &     = count_ntot_trimmed_import(nprocs, sort_import)
      call alloc_stack_to_trim_extend(nprocs, ext_trim)
!
      call count_trimmed_import_stack(nprocs, sort_import,              &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item)
!
      call alloc_idx_trimed_to_sorted(ext_trim)
      call trim_original_import_items                                   &
     &   (nprocs, sort_import%nitem_sort, sort_import%isorted_to_org,   &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item, ext_trim%idx_trimmed_to_sorted, &
     &    icou)
!
!      if(i_debug .gt. 0) then
        call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
        if(my_rank .eq. 0) write(*,*) 'Missing import from original:',  &
     &                               ntot_gl
!      end if
!
      call trim_internal_import_items(nprocs, ntot_new_import,          &
     &    irank_nod_new_import, sort_import%nitem_sort,                 &
     &    sort_import%isorted_to_org, sort_import%irank_orgin_pe,       &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item, ext_trim%idx_trimmed_to_sorted, &
     &    icou)
!
!      if(i_debug .gt. 0) then
        call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
        if(my_rank .eq. 0) write(*,*) 'Missing import from internal:',  &
     &                               ntot_gl
!      end if
!
      call trim_external_import_items                                   &
     &   (nprocs, ntot_new_import, irank_nod_new_import,                &
     &    sort_import%nitem_sort, sort_import%isorted_to_org,           &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item, ext_trim%idx_trimmed_to_sorted, &
     &    icou)
!
!      if(i_debug .gt. 0) then
        call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
        if(my_rank .eq. 0) write(*,*) 'Missing import from external:',  &
     &                               ntot_gl
!      end if
!
      call trim_orphaned_import_items                                   &
     &   (nprocs, sort_import%nitem_sort, sort_import%isorted_to_org,   &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item, ext_trim%idx_trimmed_to_sorted, &
     &    icou)
!
!      if(i_debug .gt. 0) then
        call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
       if(my_rank .eq. 0) write(*,*)                                    &
     &      'Missing import from other domain:', ntot_gl
!      end if
!
      end subroutine trim_overlap_expanded_import
!
! ----------------------------------------------------------------------
!
      subroutine trim_overlapped_sleeve_ext(ntot_new_import,            &
     &          irank_nod_new_import, sort_import, ext_trim)
!
      use calypso_mpi
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
!
      use calypso_mpi_int
      use trim_redundant_import_item
!
      integer(kind = kint), intent(in) :: ntot_new_import
      integer(kind = kint), intent(in)                                  &
     &                    :: irank_nod_new_import(ntot_new_import)
      type(sort_data_for_sleeve_trim), intent(in) :: sort_import
!
      type(data_for_trim_import), intent(inout) :: ext_trim
!
      integer(kind = kint) :: icou, ntot_gl
!
!
      ext_trim%ntot_trimmed                                             &
     &     = count_ntot_trimmed_import(nprocs, sort_import)
      call alloc_stack_to_trim_extend(nprocs, ext_trim)
!
      call count_trimmed_import_stack(nprocs, sort_import,              &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item)
!
      call alloc_idx_trimed_to_sorted(ext_trim)
      call trim_internal_import_items(nprocs, ntot_new_import,          &
     &    irank_nod_new_import, sort_import%nitem_sort,                 &
     &    sort_import%isorted_to_org, sort_import%irank_orgin_pe,       &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item, ext_trim%idx_trimmed_to_sorted, &
     &    icou)
!
!      if(i_debug .gt. 0) then
        call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
        if(my_rank .eq. 0) write(*,*) 'Missing import from internal:',  &
     &                               ntot_gl
!      end if
!
      call trim_external_import_items                                   &
     &   (nprocs, ntot_new_import, irank_nod_new_import,                &
     &    sort_import%nitem_sort, sort_import%isorted_to_org,           &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item, ext_trim%idx_trimmed_to_sorted, &
     &    icou)
!
!      if(i_debug .gt. 0) then
        call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
        if(my_rank .eq. 0) write(*,*) 'Missing import from external:',  &
     &                               ntot_gl
!      end if
!
      call trim_orphaned_import_items                                   &
     &   (nprocs, sort_import%nitem_sort, sort_import%isorted_to_org,   &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item, ext_trim%idx_trimmed_to_sorted, &
     &    icou)
!
!      if(i_debug .gt. 0) then
        call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
       if(my_rank .eq. 0) write(*,*)                                    &
     &      'Missing import from other domain:', ntot_gl
!      end if
!
      end subroutine trim_overlapped_sleeve_ext
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_stack_to_trim_extend(ext_trim)
!
      type(data_for_trim_import), intent(inout) :: ext_trim
!
      deallocate(ext_trim%istack_trimmed_pe)
      deallocate(ext_trim%istack_trimmed_item)
!
      end subroutine dealloc_stack_to_trim_extend
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_idx_trimed_to_sorted(ext_trim)
!
      type(data_for_trim_import), intent(inout) :: ext_trim
!
      deallocate(ext_trim%idx_trimmed_to_sorted)
!
      end subroutine dealloc_idx_trimed_to_sorted
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_overlapped_sleeve_ext                            &
     &         (nod_comm, add_comm, sort_import, ext_trim)
!
      use calypso_mpi
      use t_comm_table
      use t_calypso_comm_table
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
!
      type(communication_table), intent(in) :: nod_comm
      type(calypso_comm_table), intent(in) :: add_comm
      type(sort_data_for_sleeve_trim), intent(in) :: sort_import
      type(data_for_trim_import), intent(in) :: ext_trim
!
      integer(kind = kint) :: ip
!
      write(*,*) my_rank, nod_comm%num_neib,                            &
     &         'org_neib', nod_comm%id_neib
      write(*,*) my_rank, add_comm%nrank_import,                        &
     &         'new_irank_import', add_comm%irank_import
      write(*,*) my_rank, add_comm%nrank_export,                        &
     &         'new_irank_export', add_comm%irank_export
      write(*,*) my_rank, 'Totals', nod_comm%ntot_import,               &
     &                    sum(sort_import%num_sorted_by_pe),            &
     &                    ext_trim%ntot_trimmed
      do ip = 1, nprocs
        write(*,*) my_rank, ' to ', ip-1,                               &
     &             ' sort_import%num_sorted_by_pe ',                    &
     &              sort_import%num_sorted_by_pe(ip),                   &
     &              ext_trim%istack_trimmed_pe(ip)
      end do
!
      end subroutine check_overlapped_sleeve_ext
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_stack_to_trim_extend(nprocs, ext_trim)
!
      integer, intent(in) :: nprocs
      type(data_for_trim_import), intent(inout) :: ext_trim
!
!
      allocate(ext_trim%istack_trimmed_pe(0:nprocs))
      allocate(ext_trim%istack_trimmed_item(0:ext_trim%ntot_trimmed))
!
!$omp parallel workshare
      ext_trim%istack_trimmed_pe(0:nprocs) = 0
!$omp end parallel workshare
!$omp parallel workshare
      ext_trim%istack_trimmed_item(0:ext_trim%ntot_trimmed) = 0
!$omp end parallel workshare
!
      end subroutine alloc_stack_to_trim_extend
!
! ----------------------------------------------------------------------
!
      subroutine alloc_idx_trimed_to_sorted(ext_trim)
!
      type(data_for_trim_import), intent(inout) :: ext_trim
!
!
      allocate(ext_trim%idx_trimmed_to_sorted(ext_trim%ntot_trimmed))
!
      if(ext_trim%ntot_trimmed .gt. 0) then
!$omp parallel workshare
        ext_trim%idx_trimmed_to_sorted(1:ext_trim%ntot_trimmed) = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_idx_trimed_to_sorted
!
! ----------------------------------------------------------------------
!
      end module t_trim_overlapped_import
