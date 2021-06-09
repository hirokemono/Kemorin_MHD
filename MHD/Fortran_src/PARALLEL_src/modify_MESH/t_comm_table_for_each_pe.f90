!> @file  t_comm_table_for_each_pe.f90
!!      module t_comm_table_for_each_pe
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Structure of communication table list 
!!
!!@verbatim
!!      subroutine alloc_comm_table_for_each(node, each_comm)
!!      subroutine dealloc_comm_table_for_each(each_comm)
!!        type(node_data), intent(in) ::                 node
!!        type(comm_table_for_each_pe), intent(inout) :: each_comm
!!
!!      subroutine init_comm_table_for_each(ineib, nod_comm, each_comm)
!!        integer(kind = kint), intent(in) :: ineib
!!        type(node_data), intent(in) ::                 node
!!        type(communication_table), intent(in) ::       nod_comm
!!        type(comm_table_for_each_pe), intent(inout) :: each_comm
!!@endverbatim
!
      module t_comm_table_for_each_pe
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_comm_table
!
      type comm_table_for_each_pe
        integer(kind = kint) :: num_each_export = 0
        integer(kind = kint), allocatable :: item_each_export(:)
      end type comm_table_for_each_pe
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_comm_table_for_each(node, each_comm)
!
      type(node_data), intent(in) ::                 node
      type(comm_table_for_each_pe), intent(inout) :: each_comm
!
      allocate(each_comm%item_each_export(node%numnod))
!
!$omp parallel workshare
      each_comm%item_each_export(1:node%numnod) =  0
!$omp end parallel workshare
!
      end subroutine alloc_comm_table_for_each
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_comm_table_for_each(each_comm)
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
!
      deallocate(each_comm%item_each_export)
!
      end subroutine dealloc_comm_table_for_each
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_comm_table_for_each(ineib, nod_comm, each_comm)
!
      integer(kind = kint), intent(in) :: ineib
      type(communication_table), intent(in) :: nod_comm
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
!
      integer(kind = kint) :: ist, i, inod
!
!
      each_comm%num_each_export = nod_comm%istack_export(ineib)         &
     &                           - nod_comm%istack_export(ineib-1)
!
      ist = nod_comm%istack_export(ineib-1) 
!$omp parallel do private(i,inod)
      do i = 1, each_comm%num_each_export
        inod = nod_comm%item_export(i+ist)
        each_comm%item_each_export(i) = inod
      end do
!$omp end parallel do
!
      end subroutine init_comm_table_for_each
!
!  ---------------------------------------------------------------------
!
      end module t_comm_table_for_each_pe
