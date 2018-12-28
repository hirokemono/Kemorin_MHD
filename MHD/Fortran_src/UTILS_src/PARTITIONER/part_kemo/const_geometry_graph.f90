!
!      module const_geometry_graph
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine s_const_geometry_graph(numnod, edge, node_graph)
!!        type(geometry_graph), intent(inout) :: node_graph
!!      subroutine const_internal_geometry_graph                        &
!!     &         (numnod, internal_node, node_graph, intr_graph)
!!        type(geometry_graph), intent(in) :: node_graph
!!        type(geometry_graph), intent(inout) :: intr_graph
!
      module const_geometry_graph
!
      use m_precision
!
      use m_constants
      use t_geometry_graph
      use set_geometry_graph
      use cal_minmax_and_stacks
!
      implicit  none
!
      private :: const_geometry_graph_quad, const_geometry_graph_linear
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_geometry_graph(numnod, edge, node_graph)
!
      use m_geometry_constants
      use t_edge_data
!
      integer(kind = kint), intent(in) :: numnod
      type(edge_data), intent(in) :: edge
      type(geometry_graph), intent(inout) :: node_graph
!
!
      if (edge%nnod_4_edge .eq. num_quad_edge) then
        call const_geometry_graph_quad(numnod, edge, node_graph)
      else
        call const_geometry_graph_linear(numnod, edge, node_graph)
      end if
!
      end subroutine s_const_geometry_graph
!
! ----------------------------------------------------------------------
!
      subroutine const_geometry_graph_linear(numnod, edge, node_graph)
!
      use t_edge_data
!
      integer(kind = kint), intent(in) :: numnod
      type(edge_data), intent(in) :: edge
      type(geometry_graph), intent(inout) :: node_graph
!
!
      call alloc_num_geometry_graph(numnod, node_graph)
!
      call count_graph_4_linear                                         &
     &   (numnod, edge%numedge, edge%ie_edge, node_graph%num_graph_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (numnod, node_graph%num_graph_nod, izero,                      &
     &    node_graph%istack_graph_nod, node_graph%ntot_graph_nod,       &
     &    node_graph%nmax_graph_nod, node_graph%nmin_graph_nod)
!
      call alloc_geometry_graph(node_graph)
!
      call set_graph_4_linear(numnod, edge%numedge, edge%ie_edge,       &
     &    node_graph%ntot_graph_nod, node_graph%num_graph_nod,          &
     &    node_graph%istack_graph_nod, node_graph%igraph_nod)
!
      end subroutine const_geometry_graph_linear
!
! ----------------------------------------------------------------------
!
      subroutine const_geometry_graph_quad(numnod, edge, node_graph)
!
      use t_edge_data
!
      integer(kind = kint), intent(in) :: numnod
      type(edge_data), intent(in) :: edge
      type(geometry_graph), intent(inout) :: node_graph
!
!
      call alloc_num_geometry_graph(numnod, node_graph)
!
      call count_graph_4_quad                                           &
     &   (numnod, edge%numedge, edge%ie_edge, node_graph%num_graph_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (numnod, node_graph%num_graph_nod, izero,                      &
     &    node_graph%istack_graph_nod, node_graph%ntot_graph_nod,       &
     &    node_graph%nmax_graph_nod, node_graph%nmin_graph_nod)
!
      call alloc_geometry_graph(node_graph)
!
      call set_graph_4_quad(numnod, edge%numedge, edge%ie_edge,         &
     &    node_graph%ntot_graph_nod, node_graph%num_graph_nod,          &
     &    node_graph%istack_graph_nod, node_graph%igraph_nod)
!
      end subroutine const_geometry_graph_quad
!
! ----------------------------------------------------------------------
!
      subroutine const_internal_geometry_graph                          &
     &         (numnod, internal_node, node_graph, intr_graph)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(geometry_graph), intent(in) :: node_graph
      type(geometry_graph), intent(inout) :: intr_graph
!
!
      call alloc_num_geometry_graph(internal_node, intr_graph)
!
      call count_internal_graph(numnod, internal_node,                  &
     &    node_graph%ntot_graph_nod, node_graph%istack_graph_nod,       &
     &    node_graph%igraph_nod, intr_graph%num_graph_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (internal_node, intr_graph%num_graph_nod, izero,               &
     &    intr_graph%istack_graph_nod, intr_graph%ntot_graph_nod,       &
     &    intr_graph%nmax_graph_nod, intr_graph%nmin_graph_nod)
!
      call alloc_geometry_graph(intr_graph)
!
      call set_internal_graph                                           &
     &   (numnod, internal_node, node_graph%ntot_graph_nod,             &
     &    node_graph%istack_graph_nod, node_graph%igraph_nod,           &
     &    intr_graph%ntot_graph_nod, intr_graph%num_graph_nod,          &
     &    intr_graph%istack_graph_nod, intr_graph%igraph_nod)
!
      end subroutine const_internal_geometry_graph
!
! ----------------------------------------------------------------------
!
      end module const_geometry_graph
