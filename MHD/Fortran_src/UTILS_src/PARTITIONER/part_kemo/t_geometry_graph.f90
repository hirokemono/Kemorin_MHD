!>@file  t_geometry_graph.f90
!!       module t_geometry_graph
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2007
!
!> @brief  Structure of graph information
!!
!!@verbatim
!!      subroutine alloc_num_geometry_graph(nnod, graph)
!!      subroutine alloc_geometry_graph(graph)
!!
!!      subroutine dealloc_geometry_graph(graph)
!!        type(geometry_graph), intent(inout) :: graph
!!@endverbatim
!
      module t_geometry_graph
!
      use m_precision
!
      implicit  none
!
!>      structure of graph
      type geometry_graph
!>        number of node
        integer(kind=kint) :: nnod_graph
!>        total number of graph
        integer(kind=kint) :: ntot_graph_nod
!>        number of graph of each node
        integer(kind=kint), allocatable :: num_graph_nod(:)
!>        graph stack of each node
        integer(kind=kint), allocatable :: istack_graph_nod(:)
!>        graph address of each node
        integer(kind=kint), allocatable :: igraph_nod(:)
!>        maximum number of graph
        integer(kind=kint) :: nmax_graph_nod
!>        minimum number of graph
        integer(kind=kint) :: nmin_graph_nod
      end type
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_num_geometry_graph(nnod, graph)
!
      integer(kind = kint), intent(in) :: nnod
      type(geometry_graph), intent(inout) :: graph
!
!
      graph%nnod_graph = nnod
      allocate(graph%num_graph_nod(graph%nnod_graph))
      allocate(graph%istack_graph_nod(0:graph%nnod_graph))
      if(graph%nnod_graph .gt. 0) graph%num_graph_nod = 0
      graph%istack_graph_nod = 0
!
      end subroutine alloc_num_geometry_graph
!
! ----------------------------------------------------------------------
!
      subroutine alloc_geometry_graph(graph)
!
      type(geometry_graph), intent(inout) :: graph
!
!
      allocate(graph%igraph_nod(graph%ntot_graph_nod))
      graph%igraph_nod = 0
!
      end subroutine alloc_geometry_graph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_geometry_graph(graph)
!
      type(geometry_graph), intent(inout) :: graph
!
      deallocate(graph%num_graph_nod)
      deallocate(graph%istack_graph_nod)
      deallocate(graph%igraph_nod)
!
      end subroutine dealloc_geometry_graph
!
! ----------------------------------------------------------------------
!
      end module t_geometry_graph
