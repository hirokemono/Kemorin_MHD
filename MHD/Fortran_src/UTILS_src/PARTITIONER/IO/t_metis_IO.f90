!>@file   t_metis_IO.f90
!!@brief  module t_metis_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief IO for MeTiS data
!!
!!@verbatim
!!      subroutine const_metis_input                                    &
!!     &         (file_name, numnod, internal_node, edge)
!!        type(edge_data), intent(in) :: edge
!!@endverbatim
!
      module t_metis_IO
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint), parameter, private :: id_metis_file = 22
!
      type metis_IO
        integer(kind=kint) :: nnod_metis_IO
        integer(kind=kint) :: nedge_metis_IO
        integer(kind=kint) :: ntot_metis_IO
        integer(kind=kint), allocatable :: istack_metis_IO(:)
        integer(kind=kint), allocatable :: igraph_metis_IO(:)
        integer(kind=kint), allocatable :: iweight_metis_IO(:)
      end type metis_IO
!
      private :: alloc_metis_grp_stack_IO, alloc_metis_graph_IO
      private :: dealloc_metis_graph_IO
      private :: output_graph_4_metis, copy_graph_4_metis_IO
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_metis_input                                      &
     &         (file_name, numnod, internal_node, edge)
!
      use t_edge_data
      use t_geometry_graph
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(edge_data), intent(in) :: edge
!
      type(metis_IO) :: m_IO
      type(geometry_graph) :: node_graph
      type(geometry_graph) :: intr_graph
!
!
      call s_const_geometry_graph(numnod, edge, node_graph)
      call const_internal_geometry_graph                                &
     &   (numnod, internal_node, node_graph, intr_graph)
!
      call dealloc_geometry_graph(node_graph)
!
      call copy_graph_4_metis_IO(intr_graph, m_IO)
      call dealloc_geometry_graph(intr_graph)
!
      call output_graph_4_metis(file_name, m_IO)
      call dealloc_metis_graph_IO(m_IO)
!
      end subroutine const_metis_input
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_metis_grp_stack_IO(m_IO)
!
      type(metis_IO), intent(inout) :: m_IO
!
      allocate(m_IO%istack_metis_IO(0:m_IO%nnod_metis_IO))
      m_IO%istack_metis_IO = 0
!
      end subroutine alloc_metis_grp_stack_IO
!
!   --------------------------------------------------------------------
!
      subroutine alloc_metis_graph_IO(m_IO)
!
      type(metis_IO), intent(inout) :: m_IO
!
      allocate(m_IO%igraph_metis_IO(m_IO%ntot_metis_IO))
      allocate(m_IO%iweight_metis_IO(m_IO%ntot_metis_IO))
!
      if(m_IO%ntot_metis_IO .le. 0) return
      m_IO%igraph_metis_IO =  0
      m_IO%iweight_metis_IO = 0
!
      end subroutine alloc_metis_graph_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_metis_graph_IO(m_IO)
!
      type(metis_IO), intent(inout) :: m_IO
!
      deallocate(m_IO%istack_metis_IO)
      deallocate(m_IO%igraph_metis_IO)
      deallocate(m_IO%iweight_metis_IO)
!
      end subroutine dealloc_metis_graph_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine output_graph_4_metis(file_name, m_IO)
!
      character(len=kchara), intent(in) :: file_name
      type(metis_IO), intent(in) :: m_IO
!
      integer(kind = kint) :: inod, ist, ied
!
!
      open(id_metis_file, file = file_name, status='unknown')
!
      write (id_metis_file,'(2i12)')                                    &
     &     m_IO%nnod_metis_IO, m_IO%nedge_metis_IO
!
      do inod = 1, m_IO%nnod_metis_IO
        ist = m_IO%istack_metis_IO(inod-1) + 1
        ied = m_IO%istack_metis_IO(inod)
        write (id_metis_file,'(5120i12)') m_IO%igraph_metis_IO(ist:ied)
      end do
      close (id_metis_file)
!
      end subroutine output_graph_4_metis
!
!   --------------------------------------------------------------------
!
      subroutine copy_graph_4_metis_IO(intr_graph, m_IO)
!
      use t_geometry_graph
!
      type(geometry_graph), intent(in) :: intr_graph
      type(metis_IO), intent(inout) :: m_IO
!
!
      m_IO%nnod_metis_IO =  intr_graph%nnod_graph
      m_IO%ntot_metis_IO =  intr_graph%ntot_graph_nod
      m_IO%nedge_metis_IO = intr_graph%ntot_graph_nod / 2
!
      call alloc_metis_grp_stack_IO(m_IO)
      call alloc_metis_graph_IO(m_IO)
!
      m_IO%istack_metis_IO(0:m_IO%nnod_metis_IO)                        &
     &      = intr_graph%istack_graph_nod(0:m_IO%nnod_metis_IO)
      m_IO%igraph_metis_IO(1:intr_graph%ntot_graph_nod)                 &
     &      = intr_graph%igraph_nod(1:intr_graph%ntot_graph_nod)
!
      end subroutine copy_graph_4_metis_IO
!
!   --------------------------------------------------------------------
!
      end module t_metis_IO
