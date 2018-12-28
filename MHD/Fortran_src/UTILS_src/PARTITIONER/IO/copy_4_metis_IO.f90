!
!     module copy_4_metis_IO
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine copy_graph_4_metis_IO(intr_graph)
!!        type(geometry_graph), intent(in) :: graph
!
      module copy_4_metis_IO
!
      use m_precision
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
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
      end module copy_4_metis_IO
