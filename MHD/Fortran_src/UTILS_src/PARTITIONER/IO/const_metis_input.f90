!
!     module const_metis_input
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine copy_graph_4_metis_IO(intr_graph)
!!        type(geometry_graph), intent(in) :: graph
!
      module const_metis_input
!
      use m_precision
      use t_geometry_graph
!
      implicit  none
!
      type(geometry_graph), private :: node_graph
      type(geometry_graph), private :: intr_graph
!
      private :: copy_graph_4_metis_IO
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_metis_input(numnod, internal_node, edge)
!
      use t_edge_data
      use m_metis_IO
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(edge_data), intent(in) :: edge
!
!
      call s_const_geometry_graph(numnod, edge, node_graph)
      call const_internal_geometry_graph                                &
     &   (numnod, internal_node, node_graph, intr_graph)
!
      call dealloc_geometry_graph(node_graph)
!
      call copy_graph_4_metis_IO(intr_graph)
      call dealloc_geometry_graph(intr_graph)
!
      call output_graph_4_metis
!
      end subroutine s_const_metis_input
!
! ----------------------------------------------------------------------
!
      subroutine copy_graph_4_metis_IO(intr_graph)
!
      use t_geometry_graph
      use m_metis_IO
!
      type(geometry_graph), intent(in) :: intr_graph
!
!
      nnod_metis_IO =  intr_graph%nnod_graph
      ntot_metis_IO =  intr_graph%ntot_graph_nod
      nedge_metis_IO = intr_graph%ntot_graph_nod / 2
!
      call allocate_metis_grp_stack_IO
      call allocate_metis_graph_IO
!
      istack_metis_IO(0:nnod_metis_IO)                                  &
     &      = intr_graph%istack_graph_nod(0:nnod_metis_IO)
      igraph_metis_IO(1:intr_graph%ntot_graph_nod)                      &
     &      = intr_graph%igraph_nod(1:intr_graph%ntot_graph_nod)
!
      end subroutine copy_graph_4_metis_IO
!
!   --------------------------------------------------------------------
!
      end module const_metis_input
