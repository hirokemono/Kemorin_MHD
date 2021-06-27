!> @file  t_work_nod_import_extend.f90
!!      module t_work_nod_import_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine alloc_idx_extend_to_trim(num, trim_nod_to_ext)
!!      subroutine alloc_import_lc_trimmed(num, trim_nod_to_ext)
!!      subroutine dealloc_idx_extend_to_trim(trim_nod_to_ext)
!!      subroutine dealloc_import_lc_trimmed(trim_nod_to_ext)
!!        type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
!!@endverbatim
!
      module t_work_nod_import_extend
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      type work_nod_import_extend
        integer(kind = kint) :: num_extend_to_trim
        integer(kind = kint), allocatable :: idx_extend_to_trim(:)
        integer(kind = kint), allocatable :: inod_added_import(:)
!
        integer(kind = kint) :: num_import_lc_trimmed
        integer(kind = kint), allocatable :: import_lc_trimmed(:)
      end type work_nod_import_extend
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_idx_extend_to_trim(num, trim_nod_to_ext)
!
      integer(kind = kint), intent(in) :: num
      type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
!
!
      trim_nod_to_ext%num_extend_to_trim = num
      allocate(trim_nod_to_ext%idx_extend_to_trim(num))
      allocate(trim_nod_to_ext%inod_added_import(num))
!
      if(trim_nod_to_ext%num_extend_to_trim .le. 0) return
!$omp parallel workshare
      trim_nod_to_ext%idx_extend_to_trim(1:num) = 0
      trim_nod_to_ext%inod_added_import(1:num) = 0
!$omp end parallel workshare

!
      end subroutine alloc_idx_extend_to_trim
!
! ----------------------------------------------------------------------
!
      subroutine alloc_import_lc_trimmed(num, trim_nod_to_ext)
!
      integer(kind = kint), intent(in) :: num
      type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
!
!
      trim_nod_to_ext%num_import_lc_trimmed = num
      allocate(trim_nod_to_ext%import_lc_trimmed(num))
!
      if(trim_nod_to_ext%num_import_lc_trimmed .le. 0) return
!$omp parallel workshare
      trim_nod_to_ext%import_lc_trimmed(1:num) = 0
!$omp end parallel workshare
!
      end subroutine alloc_import_lc_trimmed
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_idx_extend_to_trim(trim_nod_to_ext)
!
      type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
!
!
      if(allocated(trim_nod_to_ext%idx_extend_to_trim)) then
        trim_nod_to_ext%num_extend_to_trim = 0
        deallocate(trim_nod_to_ext%inod_added_import)
        deallocate(trim_nod_to_ext%idx_extend_to_trim)
      end if
!
      end subroutine dealloc_idx_extend_to_trim
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_import_lc_trimmed(trim_nod_to_ext)
!
      type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
!
!
      if(allocated(trim_nod_to_ext%import_lc_trimmed)) then
        trim_nod_to_ext%num_import_lc_trimmed = 0
        deallocate(trim_nod_to_ext%import_lc_trimmed)
      end if
!
      end subroutine dealloc_import_lc_trimmed
!
! ----------------------------------------------------------------------
!
      end module t_work_nod_import_extend
