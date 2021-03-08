!> @file  t_trim_overlapped_import.f90
!!      module t_trim_overlapped_import
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine alloc_node_data_sleeve_ext(ntot_comm, comm_position)
!!      subroutine dealloc_node_data_sleeve_ext(comm_position)
!!        integer(kind = kint), intent(in) :: ntot_comm
!!        type(node_data_for_sleeve_ext), intent(inout) :: comm_position
!!
!!      subroutine alloc_ele_data_sleeve_ext(ntot_comm, nnod_4_ele,     &
!!     &                                     comm_connect)
!!      subroutine dealloc_ele_data_sleeve_ext(comm_connect)
!!        integer(kind = kint), intent(in) :: ntot_comm, nnod_4_ele
!!        type(ele_data_for_sleeve_ext), intent(inout) :: comm_connect
!!
!!      subroutine alloc_sort_data_sleeve_ext(ntot_comm, sorted_import)
!!      subroutine dealloc_sort_data_sleeve_ext(sorted_import)
!!        integer(kind = kint), intent(in) :: ntot_comm,
!!        type(sort_data_for_sleeve_trim), intent(inout) :: sorted_import
!!@endverbatim
!
      module t_trim_overlapped_import
!
      use m_precision
!
      type node_data_for_sleeve_ext
        integer(kind = kint) :: ntot_trimmed
        integer(kind = kint), allocatable :: istack_trimmed_pe(:)
        integer(kind = kint), allocatable :: istack_trimmed_item(:)
!
        integer(kind = kint), allocatable :: idx_trimed_to_sorted(:)
        integer(kind = kint), allocatable :: idx_trimed_to_extend(:)
      end type node_data_for_sleeve_ext
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      end module t_trim_overlapped_import
