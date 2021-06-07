!> @file  t_marks_for_sleeve_extend.f90
!!      module t_marks_for_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine alloc_marks_4_sleeve_extend(nprocs, marks_4_extend)
!!      subroutine dealloc_marks_4_sleeve_extend(marks_4_extend)
!!      subroutine init_empty_sleeve_ext_marks(nprocs, marks_4_extend)
!!        integer, intent(in) :: nprocs
!!        type(marks_for_sleeve_extend), intent(inout) :: marks_4_extend
!!@endverbatim
!
      module t_marks_for_sleeve_extend
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_mark_node_ele_to_extend
!
      implicit none
!
!
      type marks_for_sleeve_extend
        type(mark_for_each_comm), allocatable :: mark_nod_done(:)
        type(mark_for_each_comm), allocatable :: mark_nod_check(:)
        type(mark_for_each_comm), allocatable :: mark_ele(:)
      end type marks_for_sleeve_extend
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_marks_4_sleeve_extend(nprocs, marks_4_extend)
!
      integer, intent(in) :: nprocs
      type(marks_for_sleeve_extend), intent(inout) :: marks_4_extend
!
      integer(kind = kint) :: ip
!
!
      allocate(marks_4_extend%mark_nod_done(nprocs))
      allocate(marks_4_extend%mark_nod_check(nprocs))
      allocate(marks_4_extend%mark_ele(nprocs))
!
!$omp parallel do
      do ip = 1, nprocs
        marks_4_extend%mark_nod_done(ip)%num_marked =  -1
        marks_4_extend%mark_nod_check(ip)%num_marked = -1
        marks_4_extend%mark_ele(ip)%num_marked =       -1
      end do
!$omp end parallel do
!
      end subroutine alloc_marks_4_sleeve_extend
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_marks_4_sleeve_extend(marks_4_extend)
!
      type(marks_for_sleeve_extend), intent(inout) :: marks_4_extend
!
!
      deallocate(marks_4_extend%mark_nod_done)
      deallocate(marks_4_extend%mark_nod_check)
      deallocate(marks_4_extend%mark_ele)
!
      end subroutine dealloc_marks_4_sleeve_extend
!
!  ---------------------------------------------------------------------
!
      subroutine init_empty_sleeve_ext_marks(nprocs, marks_4_extend)
!
      integer, intent(in) :: nprocs
!
      type(marks_for_sleeve_extend), intent(inout) :: marks_4_extend
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, nprocs
        if(marks_4_extend%mark_nod_done(ip)%num_marked .lt. 0) then
          call alloc_mark_for_each_comm                                 &
     &       (izero, marks_4_extend%mark_nod_done(ip))
        end if
        if(marks_4_extend%mark_nod_check(ip)%num_marked .lt. 0) then
          call alloc_mark_for_each_comm                                 &
     &       (izero, marks_4_extend%mark_nod_check(ip))
        end if
        if(marks_4_extend%mark_ele(ip)%num_marked .lt. 0) then
          call alloc_mark_for_each_comm                                 &
     &       (izero, marks_4_extend%mark_ele(ip))
        end if
      end do
!$omp end parallel do
!
      end subroutine init_empty_sleeve_ext_marks
!
!  ---------------------------------------------------------------------
!
      end module t_marks_for_sleeve_extend
