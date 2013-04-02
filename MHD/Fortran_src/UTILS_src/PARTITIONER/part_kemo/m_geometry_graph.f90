!
!      module m_geometry_graph
!
      module m_geometry_graph
!
!     Written by H. Matsui on Aug., 2007
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint) :: ntot_graph_nod
      integer(kind=kint), allocatable, target :: num_graph_nod(:)
      integer(kind=kint), allocatable, target :: istack_graph_nod(:)
      integer(kind=kint), allocatable, target :: igraph_nod(:)
      integer(kind=kint) :: nmax_graph_nod, nmin_graph_nod
!
      integer(kind=kint) :: ntot_graph_inter_nod
      integer(kind=kint), allocatable, target :: num_graph_inter_nod(:)
      integer(kind=kint), allocatable, target                           &
     &                   :: istack_graph_inter_nod(:)
      integer(kind=kint), allocatable, target :: igraph_inter_nod(:)
      integer(kind=kint) :: nmax_graph_inter_nod, nmin_graph_inter_nod
!
!      subroutine allocate_num_geometry_graph(nnod)
!      subroutine allocate_geometry_graph
!      subroutine allocate_num_internod_graph(inter_nod)
!      subroutine allocate_internod_graph
!
!      subroutine deallocate_geometry_graph
!      subroutine deallocate_internod_graph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_num_geometry_graph(nnod)
!
      integer(kind = kint), intent(in) :: nnod
!
      allocate(num_graph_nod(nnod))
      allocate(istack_graph_nod(0:nnod))
      num_graph_nod = 0
      istack_graph_nod = 0
!
      end subroutine allocate_num_geometry_graph
!
! ----------------------------------------------------------------------
!
      subroutine allocate_geometry_graph
!
      allocate(igraph_nod(ntot_graph_nod))
      igraph_nod = 0
!
      end subroutine allocate_geometry_graph
!
! ----------------------------------------------------------------------
!
      subroutine allocate_num_internod_graph(inter_nod)
!
      integer(kind = kint), intent(in) :: inter_nod
!
      allocate(num_graph_inter_nod(inter_nod))
      allocate(istack_graph_inter_nod(0:inter_nod))
      num_graph_inter_nod = 0
      istack_graph_inter_nod = 0
!
      end subroutine allocate_num_internod_graph
!
! ----------------------------------------------------------------------
!
      subroutine allocate_internod_graph
!
      allocate(igraph_inter_nod(ntot_graph_inter_nod))
      igraph_inter_nod = 0
!
      end subroutine allocate_internod_graph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_geometry_graph
!
      deallocate(num_graph_nod)
      deallocate(istack_graph_nod)
      deallocate(igraph_nod)
!
      end subroutine deallocate_geometry_graph
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_internod_graph
!
      deallocate(num_graph_inter_nod)
      deallocate(istack_graph_inter_nod)
      deallocate(igraph_inter_nod)
!
      end subroutine deallocate_internod_graph
!
! ----------------------------------------------------------------------
!
      end module m_geometry_graph
