!
!      module const_geometry_graph
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine s_const_geometry_graph
!
!      subroutine const_geometry_graph_linear
!      subroutine const_geometry_graph_quad
!
!      subroutine const_internal_geometry_graph
!
      module const_geometry_graph
!
      use m_precision
!
      use m_constants
      use m_geometry_data
      use m_geometry_graph
      use set_geometry_graph
      use cal_minmax_and_stacks
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_geometry_graph
!
      use m_geometry_constants
!
!
      if (edge1%nnod_4_edge .eq. num_quad_edge) then
        call const_geometry_graph_quad
      else
        call const_geometry_graph_linear
      end if
!
      end subroutine s_const_geometry_graph
!
! ----------------------------------------------------------------------
!
      subroutine const_geometry_graph_linear
!
!
      call allocate_num_geometry_graph(node1%numnod)
!
      call count_graph_4_linear(node1%numnod,                           &
     &    edge1%numedge, edge1%ie_edge, num_graph_nod)
!
      call s_cal_minmax_and_stacks(node1%numnod, num_graph_nod, izero,  &
     &    istack_graph_nod, ntot_graph_nod,                             &
     &    nmax_graph_nod, nmin_graph_nod)
!
      call allocate_geometry_graph
!
      call set_graph_4_linear                                           &
     &   (node1%numnod, edge1%numedge, edge1%ie_edge, ntot_graph_nod,   &
     &    num_graph_nod, istack_graph_nod, igraph_nod)
!
      end subroutine const_geometry_graph_linear
!
! ----------------------------------------------------------------------
!
      subroutine const_geometry_graph_quad
!
!
      call allocate_num_geometry_graph(node1%numnod)
!
      call count_graph_4_quad                                           &
     &   (node1%numnod, edge1%numedge, edge1%ie_edge, num_graph_nod)
!
      call s_cal_minmax_and_stacks(node1%numnod, num_graph_nod, izero,  &
     &    istack_graph_nod, ntot_graph_nod,                             &
     &    nmax_graph_nod, nmin_graph_nod)
!
      call allocate_geometry_graph
!
      call set_graph_4_quad                                             &
     &   (node1%numnod, edge1%numedge, edge1%ie_edge, ntot_graph_nod,   &
     &    num_graph_nod, istack_graph_nod, igraph_nod)
!
      end subroutine const_geometry_graph_quad
!
! ----------------------------------------------------------------------
!
      subroutine const_internal_geometry_graph
!
!
      call allocate_num_internod_graph(node1%internal_node)
!
      call count_internal_graph(node1%numnod, node1%internal_node,      &
     &    ntot_graph_nod, istack_graph_nod, igraph_nod,                 &
     &    num_graph_inter_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (node1%internal_node, num_graph_inter_nod,                     &
     &    izero, istack_graph_inter_nod, ntot_graph_inter_nod,          &
     &    nmax_graph_inter_nod, nmin_graph_inter_nod)
!
      call allocate_internod_graph
!
      call set_internal_graph(node1%numnod, node1%internal_node,        &
     &    ntot_graph_nod, istack_graph_nod, igraph_nod,                 &
     &    ntot_graph_inter_nod, num_graph_inter_nod,                    &
     &    istack_graph_inter_nod, igraph_inter_nod)
!
      end subroutine const_internal_geometry_graph
!
! ----------------------------------------------------------------------
!
      end module const_geometry_graph
