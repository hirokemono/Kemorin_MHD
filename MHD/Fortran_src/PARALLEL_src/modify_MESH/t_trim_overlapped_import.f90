!> @file  t_trim_overlapped_import.f90
!!      module t_trim_overlapped_import
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine alloc_stack_to_trim_extend                           &
!!      subroutine alloc_idx_trimed_to_sorted(ext_trim)
!!      subroutine alloc_idx_extend_to_trimmed(ntot_w_overlap, ext_trim)
!!     &         (nprocs, ntot_trimmed, ext_trim)
!!        integer, intent(in) :: nprocs
!!        integer(kind = kint), intent(in) :: ntot_trimmed
!!        integer(kind = kint), intent(in) :: ntot_w_overlap
!!        type(data_for_trim_import), intent(inout) :: ext_trim
!!      subroutine dealloc_stack_to_trim_extend(ext_trim)
!!      subroutine dealloc_idx_trimed_to_sorted(ext_trim)
!!      subroutine dealloc_idx_extend_to_trimmed(ext_trim)
!!        type(data_for_trim_import), intent(inout) :: ext_trim
!!@endverbatim
!
      module t_trim_overlapped_import
!
      use m_precision
!
      type data_for_trim_import
        integer(kind = kint) :: ntot_trimmed
        integer(kind = kint) :: ntot_w_overlap
        integer(kind = kint), allocatable :: istack_trimmed_pe(:)
        integer(kind = kint), allocatable :: istack_trimmed_item(:)
!
        integer(kind = kint), allocatable :: idx_trimmed_to_sorted(:)
        integer(kind = kint), allocatable :: idx_extend_to_trimmed(:)
      end type data_for_trim_import
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_stack_to_trim_extend                             &
     &         (nprocs, ntot_trimmed, ext_trim)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_trimmed
      type(data_for_trim_import), intent(inout) :: ext_trim
!
!
      ext_trim%ntot_trimmed = ntot_trimmed
!
      allocate(ext_trim%istack_trimmed_pe(0:nprocs))
      allocate(ext_trim%istack_trimmed_item(0:ext_trim%ntot_trimmed))
!
!$omp parallel workshare
      ext_trim%istack_trimmed_pe(0:nprocs) = 0
!$omp end parallel workshare
!$omp parallel workshare
      ext_trim%istack_trimmed_pe(0:ext_trim%ntot_trimmed) = 0
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
        ext_trim%idx_trimmed_to_sorted(1:ext_trim%ntot_trimmed) = -1
!$omp end parallel workshare
      end if
!
      end subroutine alloc_idx_trimed_to_sorted
!
! ----------------------------------------------------------------------
!
      subroutine alloc_idx_extend_to_trimmed(ntot_w_overlap, ext_trim)
!
      integer(kind = kint), intent(in) :: ntot_w_overlap
      type(data_for_trim_import), intent(inout) :: ext_trim
!
!
      ext_trim%ntot_w_overlap = ntot_w_overlap
      allocate(ext_trim%idx_extend_to_trimmed(ext_trim%ntot_w_overlap))
!
      if(ext_trim%ntot_w_overlap .gt. 0) then
!$omp parallel workshare
        ext_trim%idx_extend_to_trimmed(1:ext_trim%ntot_w_overlap) = -1
!$omp end parallel workshare
      end if
!
      end subroutine alloc_idx_extend_to_trimmed
!
! ----------------------------------------------------------------------
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
!
      subroutine dealloc_idx_extend_to_trimmed(ext_trim)
!
      type(data_for_trim_import), intent(inout) :: ext_trim
!
      deallocate(ext_trim%idx_extend_to_trimmed)
!
      end subroutine dealloc_idx_extend_to_trimmed
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine trim_overlapped_sleeve_ext                             &
     &         (nprocs, sort_import, ext_trim)
!
      use t_mesh_for_sleeve_extend
      use trim_redundant_import_item
!
      integer, intent(in) :: nprocs
      type(sort_data_for_sleeve_trim), intent(in) :: sort_import
!
      type(data_for_trim_import), intent(inout) :: ext_trim
!
!
      ext_trim%ntot_trimmed                                             &
     &     = count_ntot_trimmed_import(nprocs, sort_import)
!
      allocate(ext_trim%istack_trimmed_pe(0:nprocs))
      allocate(ext_trim%istack_trimmed_item(0:ext_trim%ntot_trimmed))
      ext_trim%istack_trimmed_pe(:) = 0
      ext_trim%istack_trimmed_item(:) = 0
!
      call count_trimmed_import_stack(nprocs, sort_import,              &
     &    ext_trim%ntot_trimmed, ext_trim%istack_trimmed_pe,            &
     &    ext_trim%istack_trimmed_item)
!
      allocate(ext_trim%idx_trimmed_to_sorted(ext_trim%ntot_trimmed))
      ext_trim%idx_trimmed_to_sorted(1:ext_trim%ntot_trimmed) = -1
!
      end subroutine trim_overlapped_sleeve_ext
!
! ----------------------------------------------------------------------
!
      end module t_trim_overlapped_import
