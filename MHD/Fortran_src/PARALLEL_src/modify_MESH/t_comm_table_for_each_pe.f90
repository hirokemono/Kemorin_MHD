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
!!      subroutine alloc_dist_from_wall_export(ntot_export, dist)
!!      subroutine dealloc_dist_from_wall_export(dist)
!!        type(node_data), intent(in) :: ntot_export
!!        type(dist_from_wall_in_export), intent(inout) :: dist
!!
!!      subroutine init_comm_table_for_each(ineib, node, nod_comm,      &
!!     &          dist_4_comm, each_comm, distance)
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
!
        integer(kind = kint) :: num_each_import = 0
        integer(kind = kint), allocatable :: item_each_import(:)
      end type comm_table_for_each_pe
!
      type dist_from_wall_in_export
        integer(kind = kint) :: ntot = 0
        real(kind = kreal), allocatable :: distance_in_export(:)
      end type dist_from_wall_in_export
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
      allocate(each_comm%item_each_import(node%numnod))
!
!$omp parallel workshare
      each_comm%item_each_export(1:node%numnod) =  0
      each_comm%item_each_import(1:node%numnod) =  0
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
      deallocate(each_comm%item_each_import)
      deallocate(each_comm%item_each_export)
!
      end subroutine dealloc_comm_table_for_each
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dist_from_wall_export(ntot_export, dist)
!
      integer(kind = kint), intent(in) :: ntot_export
      type(dist_from_wall_in_export), intent(inout) :: dist
!
      dist%ntot = ntot_export
      allocate(dist%distance_in_export(dist%ntot))
!
!$omp parallel workshare
      dist%distance_in_export(1:dist%ntot) =  0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_dist_from_wall_export
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dist_from_wall_export(dist)
!
      type(dist_from_wall_in_export), intent(inout) :: dist
!
      deallocate(dist%distance_in_export)
!
      end subroutine dealloc_dist_from_wall_export
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_comm_table_for_each(ineib, node, nod_comm,        &
     &          dist_4_comm, each_comm, distance)
!
      integer(kind = kint), intent(in) :: ineib
      type(node_data), intent(in) ::                 node
      type(communication_table), intent(in) ::       nod_comm
      type(dist_from_wall_in_export), intent(in) :: dist_4_comm
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
      real(kind = kreal), intent(inout) :: distance(node%numnod)
!
      integer(kind = kint) :: ist, i, inod
!
!
      each_comm%num_each_export = nod_comm%istack_export(ineib)         &
     &                           - nod_comm%istack_export(ineib-1)
!
!$omp parallel workshare
      distance(1:node%numnod) = 0.0d0
!$omp end parallel workshare
!
      ist = nod_comm%istack_export(ineib-1) 
!$omp parallel do private(i,inod)
      do i = 1, each_comm%num_each_export
        inod = nod_comm%item_export(i+ist)
        each_comm%item_each_export(i) = inod
        distance(inod) = dist_4_comm%distance_in_export(i+ist)
      end do
!$omp end parallel do
!
      each_comm%num_each_import = nod_comm%istack_import(ineib)         &
     &                           - nod_comm%istack_import(ineib-1)
!
      ist = nod_comm%istack_import(ineib-1) 
!$omp parallel do private(i)
      do i = 1, each_comm%num_each_import
        each_comm%item_each_import(i) = nod_comm%item_import(i+ist)
      end do
!$omp end parallel do
!
      end subroutine init_comm_table_for_each
!
!  ---------------------------------------------------------------------
!
      end module t_comm_table_for_each_pe
