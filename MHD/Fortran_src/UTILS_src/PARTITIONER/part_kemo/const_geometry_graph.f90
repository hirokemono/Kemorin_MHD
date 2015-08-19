!
!      module const_geometry_graph
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine s_const_geometry_graph(numnod, edge)
!      subroutine const_internal_geometry_graph(numnod, internal_node)
!
      module const_geometry_graph
!
      use m_precision
!
      use m_constants
      use m_geometry_graph
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
      subroutine s_const_geometry_graph(numnod, edge)
!
      use m_geometry_constants
      use t_edge_data
!
      integer(kind = kint), intent(in) :: numnod
      type(edge_data), intent(in) :: edge
!
!
      if (edge%nnod_4_edge .eq. num_quad_edge) then
        call const_geometry_graph_quad(numnod, edge)
      else
        call const_geometry_graph_linear(numnod, edge)
      end if
!
      end subroutine s_const_geometry_graph
!
! ----------------------------------------------------------------------
!
      subroutine const_geometry_graph_linear(numnod, edge)
!
      use t_edge_data
!
      integer(kind = kint), intent(in) :: numnod
      type(edge_data), intent(in) :: edge
!
!
      call allocate_num_geometry_graph(numnod)
!
      call count_graph_4_linear                                         &
     &   (numnod, edge%numedge, edge%ie_edge, num_graph_nod)
!
      call s_cal_minmax_and_stacks(numnod, num_graph_nod, izero,        &
     &    istack_graph_nod, ntot_graph_nod,                             &
     &    nmax_graph_nod, nmin_graph_nod)
!
      call allocate_geometry_graph
!
      call set_graph_4_linear                                           &
     &   (numnod, edge%numedge, edge%ie_edge, ntot_graph_nod,           &
     &    num_graph_nod, istack_graph_nod, igraph_nod)
!
      end subroutine const_geometry_graph_linear
!
! ----------------------------------------------------------------------
!
      subroutine const_geometry_graph_quad(numnod, edge)
!
      use t_edge_data
!
      integer(kind = kint), intent(in) :: numnod
      type(edge_data), intent(in) :: edge
!
!
      call allocate_num_geometry_graph(numnod)
!
      call count_graph_4_quad                                           &
     &   (numnod, edge%numedge, edge%ie_edge, num_graph_nod)
!
      call s_cal_minmax_and_stacks(numnod, num_graph_nod, izero,        &
     &    istack_graph_nod, ntot_graph_nod,                             &
     &    nmax_graph_nod, nmin_graph_nod)
!
      call allocate_geometry_graph
!
      call set_graph_4_quad                                             &
     &   (numnod, edge%numedge, edge%ie_edge, ntot_graph_nod,           &
     &    num_graph_nod, istack_graph_nod, igraph_nod)
!
      end subroutine const_geometry_graph_quad
!
! ----------------------------------------------------------------------
!
      subroutine const_internal_geometry_graph(numnod, internal_node)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
!
!
      call allocate_num_internod_graph(internal_node)
!
      call count_internal_graph(numnod, internal_node, ntot_graph_nod,  &
     &    istack_graph_nod, igraph_nod, num_graph_inter_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (internal_node, num_graph_inter_nod, izero,                    &
     &    istack_graph_inter_nod, ntot_graph_inter_nod,                 &
     &    nmax_graph_inter_nod, nmin_graph_inter_nod)
!
      call allocate_internod_graph
!
      call set_internal_graph(numnod, internal_node,                    &
     &    ntot_graph_nod, istack_graph_nod, igraph_nod,                 &
     &    ntot_graph_inter_nod, num_graph_inter_nod,                    &
     &    istack_graph_inter_nod, igraph_inter_nod)
!
      end subroutine const_internal_geometry_graph
!
! ----------------------------------------------------------------------
!
      end module const_geometry_graph
