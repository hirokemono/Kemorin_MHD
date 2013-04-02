!
!     module m_metis_IO
!
      module m_metis_IO
!
!     Written by H. Matsui on Aug., 2007
!
      use m_precision
!
      implicit  none
!
      character(len=kchara) :: metis_file_name = 'graph.in'
      integer(kind=kint), parameter :: id_metis_file = 22
!
      integer(kind=kint) :: nnod_metis_IO
      integer(kind=kint) :: nedge_metis_IO
      integer(kind=kint) :: ntot_metis_IO
      integer(kind=kint), allocatable :: istack_metis_IO(:)
      integer(kind=kint), allocatable :: igraph_metis_IO(:)
      integer(kind=kint), allocatable :: iweight_metis_IO(:)
!
!      subroutine allocate_metis_grp_stack_IO
!      subroutine allocate_metis_graph_IO
!
!      subroutine deallocate_metis_graph_IO
!      subroutine output_graph_4_metis
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_metis_grp_stack_IO
!
      allocate(istack_metis_IO(0:nnod_metis_IO))
      istack_metis_IO = 0
!
      end subroutine allocate_metis_grp_stack_IO
!
!   --------------------------------------------------------------------
!
      subroutine allocate_metis_graph_IO
!
      allocate(igraph_metis_IO(ntot_metis_IO))
      allocate(iweight_metis_IO(ntot_metis_IO))
      igraph_metis_IO =  0
      iweight_metis_IO = 0
!
      end subroutine allocate_metis_graph_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_metis_graph_IO
!
      deallocate(istack_metis_IO)
      deallocate(igraph_metis_IO)
      deallocate(iweight_metis_IO)
!
      end subroutine deallocate_metis_graph_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine output_graph_4_metis
!
      integer(kind = kint) :: inod, ist, ied
!
      open  (id_metis_file, file=metis_file_name, status='unknown')
!
      write (id_metis_file,'(2i12)') nnod_metis_IO, nedge_metis_IO
!
      do inod = 1, nnod_metis_IO
        ist = istack_metis_IO(inod-1) + 1
        ied = istack_metis_IO(inod)
        write (id_metis_file,'(5120i12)') igraph_metis_IO(ist:ied)
      end do
      close (id_metis_file)
!
      call deallocate_metis_graph_IO
!
      end subroutine output_graph_4_metis
!
!   --------------------------------------------------------------------
!
      end module m_metis_IO
