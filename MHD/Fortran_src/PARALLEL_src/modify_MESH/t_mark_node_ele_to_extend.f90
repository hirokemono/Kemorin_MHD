!> @file  t_mark_node_ele_to_extend.f90
!!      module t_mark_node_ele_to_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Structure of marking and distance for sleeveextension
!!
!!@verbatim
!!      subroutine alloc_istack_mark_ecomm_smp(mark_comm)
!!      subroutine alloc_mark_for_each_comm(mark_comm)
!!      subroutine dealloc_istack_mark_ecomm_smp(mark_comm)
!!      subroutine dealloc_mark_for_each_comm(mark_comm)
!!        integer(kind = kint), intent(in) :: num
!!        type(mark_for_each_comm), intent(inout) :: mark_comm
!!      subroutine copy_mark_for_each_comm(org_mark_comm, new_mark_comm)
!!        type(mark_for_each_comm), intent(in) :: org_mark_comm
!!        type(mark_for_each_comm), intent(inout) :: new_mark_comm
!!
!!      subroutine alloc_istack_marked_export(nod_comm, marked_export)
!!      subroutine alloc_items_marked_export(marked_export)
!!      subroutine dealloc_mark_in_export(marked_export)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(mark_in_export), intent(inout) :: marked_export
!!
!!      subroutine check_missing_connect_to_extend                      &
!!    &          (node, ele, mark_ele, iflag_node, icou_nod, icou_ele)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(mark_for_each_comm), intent(inout) :: mark_ele
!!        integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!!        integer(kind = kint), intent(inout) :: icou_nod, icou_ele
!!@endverbatim
!
      module t_mark_node_ele_to_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_comm_table
!
      implicit none
!
      type mark_for_each_comm
        integer(kind = kint) :: num_marked = 0
        integer(kind = kint), allocatable :: istack_marked_smp(:)
        integer(kind = kint), allocatable :: idx_marked(:)
        real(kind = kreal), allocatable :: dist_marked(:)
      end type mark_for_each_comm
!
      type mark_in_export
        integer(kind= kint) :: ntot_marked_export
        integer(kind= kint), allocatable :: istack_marked_export(:)
        integer(kind= kint), allocatable :: item_marked_export(:)
        real(kind = kreal), allocatable :: dist_marked_export(:)
      end type mark_in_export
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_istack_mark_ecomm_smp(mark_comm)
!
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
!
      allocate(mark_comm%istack_marked_smp(0:np_smp))
!
!$omp parallel workshare
      mark_comm%istack_marked_smp(0:np_smp) = 0
!$omp end parallel workshare
!
      end subroutine alloc_istack_mark_ecomm_smp
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_mark_for_each_comm(mark_comm)
!
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
!
      allocate(mark_comm%idx_marked(mark_comm%num_marked))
      allocate(mark_comm%dist_marked(mark_comm%num_marked))
!
      if(mark_comm%num_marked .le. 0) return
!$omp parallel workshare
      mark_comm%idx_marked(1:mark_comm%num_marked) = 0
      mark_comm%dist_marked(1:mark_comm%num_marked) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_mark_for_each_comm
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_istack_mark_ecomm_smp(mark_comm)
!
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
!
      deallocate(mark_comm%istack_marked_smp)
!
      end subroutine dealloc_istack_mark_ecomm_smp
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_mark_for_each_comm(mark_comm)
!
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
      deallocate(mark_comm%idx_marked, mark_comm%dist_marked)
!
      end subroutine dealloc_mark_for_each_comm
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mark_for_each_comm(org_mark_comm, new_mark_comm)
!
      type(mark_for_each_comm), intent(in) :: org_mark_comm
      type(mark_for_each_comm), intent(inout) :: new_mark_comm
!
!
      if(new_mark_comm%num_marked .le. 0) return
!
!$omp parallel workshare
      new_mark_comm%istack_marked_smp(0:np_smp)                         &
     &   = org_mark_comm%istack_marked_smp(0:np_smp)
!$omp end parallel workshare
!
!$omp parallel workshare
      new_mark_comm%idx_marked(1:new_mark_comm%num_marked)              &
     &   = org_mark_comm%idx_marked(1:new_mark_comm%num_marked)
      new_mark_comm%dist_marked(1:new_mark_comm%num_marked)             &
     &   = org_mark_comm%dist_marked(1:new_mark_comm%num_marked)
!$omp end parallel workshare
!
      end subroutine copy_mark_for_each_comm
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_istack_marked_export(nod_comm, marked_export)
!
      type(communication_table), intent(in) :: nod_comm
      type(mark_in_export), intent(inout) :: marked_export
!
!
      allocate(marked_export%istack_marked_export(0:nod_comm%num_neib))
!
!$omp parallel workshare
      marked_export%istack_marked_export(0:nod_comm%num_neib) = 0
!$omp end parallel workshare
!
      end subroutine alloc_istack_marked_export
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_items_marked_export(marked_export)
!
      type(mark_in_export), intent(inout) :: marked_export
!
      integer(kind = kint) :: ntot
!
      ntot = marked_export%ntot_marked_export
      allocate(marked_export%item_marked_export(ntot))
      allocate(marked_export%dist_marked_export(ntot))
!
      if(marked_export%ntot_marked_export .le. 0) return
!$omp parallel workshare
      marked_export%istack_marked_export(1:ntot) = 0
!$omp end parallel workshare
!
      end subroutine alloc_items_marked_export
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_mark_in_export(marked_export)
!
      type(mark_in_export), intent(inout) :: marked_export
!
!
      deallocate(marked_export%item_marked_export)
      deallocate(marked_export%dist_marked_export)
      deallocate(marked_export%istack_marked_export)
!
      end subroutine dealloc_mark_in_export
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_missing_connect_to_extend                        &
    &          (node, ele, mark_ele, iflag_node, icou_nod, icou_ele)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(mark_for_each_comm), intent(inout) :: mark_ele
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!
      integer(kind = kint), intent(inout) :: icou_nod, icou_ele
!
      integer(kind = kint) :: inum, iele, k1, kcou, inod
!
      do inum = 1, mark_ele%num_marked
        iele = mark_ele%idx_marked(inum)
        kcou = 0
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          if(iflag_node(inod) .ge. 0) kcou = kcou + 1
        end do
        icou_nod = icou_nod + kcou
        if(kcou .gt. 0) then
          icou_ele = icou_ele + 1
!          write(*,*) iele, ele%ie(iele,1:ele%nnod_4_ele),              &
!     &                iflag_node(ele%ie(iele,1:ele%nnod_4_ele))
        end if
      end do
!
      end subroutine check_missing_connect_to_extend
!
!  ---------------------------------------------------------------------
!
      end module t_mark_node_ele_to_extend
