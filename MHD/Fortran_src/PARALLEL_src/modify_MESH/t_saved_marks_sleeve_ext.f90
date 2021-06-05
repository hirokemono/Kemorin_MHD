!> @file  t_saved_marks_sleeve_ext.f90
!!      module t_saved_marks_sleeve_ext
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine init_saved_sleeve_ext_mark                           &
!!     &         (nprocs, nod_comm, marks_4_nod, marks_4_save)
!!        integer, intent(in) :: nprocs
!!        type(communication_table), intent(in) :: nod_comm
!!        type(marks_for_sleeve_extension), intent(in) :: marks_4_nod
!!        type(saved_sleeve_ext_mark), intent(inout) :: marks_4_save
!!      subroutine dealloc_saved_sleeve_ext_mark(nprocs, marks_4_save)
!!        integer, intent(in) :: nprocs
!!        type(saved_sleeve_ext_mark), intent(inout) :: marks_4_save
!!      subroutine copy_from_saved_sleeve_mark                          &
!!     &         (nod_comm, marks_4_nod, marks_4_save)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(saved_sleeve_ext_mark), intent(in) :: marks_4_save
!!        type(marks_for_sleeve_extension), intent(inout)               &
!!     &                                            :: marks_4_nod
!!@endverbatim
!
      module t_saved_marks_sleeve_ext
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_mark_node_ele_to_extend
      use const_nod_ele_to_extend
!
      implicit none
!
!
      type saved_sleeve_ext_mark
        type(mark_for_each_comm), allocatable :: mark_comm(:)
      end type saved_sleeve_ext_mark
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_saved_sleeve_ext_mark                             &
     &         (nprocs, nod_comm, marks_4_nod, marks_4_save)
!
      integer, intent(in) :: nprocs
      type(communication_table), intent(in) :: nod_comm
      type(marks_for_sleeve_extension), intent(in) :: marks_4_nod
!
      type(saved_sleeve_ext_mark), intent(inout) :: marks_4_save
!
      integer(kind = kint) :: ip, inum
!
!
      allocate(marks_4_save%mark_comm(nprocs))
!
      do ip = 1, nprocs
        marks_4_save%mark_comm(ip)%num_marked = -1
      end do
!
      do inum = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(inum) + 1
        call alloc_mark_for_each_comm                                   &
     &     (marks_4_nod%mark_nod(inum)%num_marked,                      &
     &      marks_4_save%mark_comm(ip))
        call copy_mark_for_each_comm                                    &
     &     (marks_4_nod%mark_nod(inum), marks_4_save%mark_comm(ip))
      end do
!
      do ip = 1, nprocs
        if(marks_4_save%mark_comm(ip)%num_marked .lt. 0) then
          call alloc_mark_for_each_comm                                 &
     &       (izero, marks_4_save%mark_comm(ip))
        end if
      end do
!
      end subroutine init_saved_sleeve_ext_mark
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_saved_sleeve_ext_mark(nprocs, marks_4_save)
!
      integer, intent(in) :: nprocs
      type(saved_sleeve_ext_mark), intent(inout) :: marks_4_save
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs
        call dealloc_mark_for_each_comm(marks_4_save%mark_comm(ip))
      end do
      deallocate(marks_4_save%mark_comm)
!
      end subroutine  dealloc_saved_sleeve_ext_mark
!
!  ---------------------------------------------------------------------
!
      subroutine copy_from_saved_sleeve_mark                            &
     &         (nod_comm, marks_4_nod, marks_4_save)
!
      type(communication_table), intent(in) :: nod_comm
      type(saved_sleeve_ext_mark), intent(in) :: marks_4_save
!
      type(marks_for_sleeve_extension), intent(inout) :: marks_4_nod
!
      integer(kind = kint) :: ip, inum
!
!
      call alloc_sleeve_extension_marks(nod_comm, marks_4_nod)
!
      do inum = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(inum) + 1
        call alloc_mark_for_each_comm                                   &
     &     (marks_4_save%mark_comm(ip)%num_marked,                      &
     &      marks_4_nod%mark_nod(inum))
        call copy_mark_for_each_comm                                    &
     &     (marks_4_save%mark_comm(ip), marks_4_nod%mark_nod(inum))
      end do
!
      end subroutine copy_from_saved_sleeve_mark
!
!  ---------------------------------------------------------------------
!
      end module t_saved_marks_sleeve_ext
