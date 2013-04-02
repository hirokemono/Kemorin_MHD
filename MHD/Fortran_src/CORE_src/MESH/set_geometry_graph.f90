!
!      module set_geometry_graph
!
!     Written by H. Matsui on Aug., 2007
!
      module set_geometry_graph
!
      use m_precision
!
      implicit  none
!
!      subroutine count_graph_4_linear(nnod, nedge, ie_edge,            &
!     &          num_graph_nod)
!      subroutine set_graph_4_linear(nnod, nedge, ie_edge,              &
!     &          ntot_graph_nod, num_graph_nod, istack_graph_nod,       &
!     &          igraph_nod)
!
!      subroutine count_graph_4_quad(nnod, nedge, ie_edge,              &
!     &          num_graph_nod)
!      subroutine set_graph_4_quad(nnod, nedge, ie_edge, ntot_graph_nod,&
!     &          num_graph_nod, istack_graph_nod, igraph_nod)
!
!      subroutine count_internal_graph(nnod, internal_node,             &
!     &          ntot_graph_nod, istack_graph_nod, igraph_nod,          &
!     &          num_graph_inter_nod)
!      subroutine set_internal_graph(nnod, internal_node,               &
!     &          ntot_graph_nod, istack_graph_nod, igraph_nod,          &
!     &          ntot_graph_inter_nod, num_graph_inter_nod,             &
!     &          istack_graph_inter_nod, igraph_inter_nod)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_graph_4_linear(nnod, nedge, ie_edge,             &
     &          num_graph_nod)
!
      integer(kind = kint), intent(in) :: nnod, nedge
      integer(kind = kint), intent(in) :: ie_edge(nedge,2)
!
      integer(kind = kint), intent(inout) :: num_graph_nod(nnod)
!
      integer(kind = kint) :: iedge, inod1, inod2
!
!
      num_graph_nod(1:nnod) = 0
      do iedge = 1, nedge
        inod1 = ie_edge(iedge,1)
        inod2 = ie_edge(iedge,2)
        num_graph_nod(inod1) = num_graph_nod(inod1) + 1
        num_graph_nod(inod2) = num_graph_nod(inod2) + 1
      end do
!
      end subroutine count_graph_4_linear
!
! ----------------------------------------------------------------------
!
      subroutine set_graph_4_linear(nnod, nedge, ie_edge,               &
     &          ntot_graph_nod, num_graph_nod, istack_graph_nod,        &
     &          igraph_nod)
!
      integer(kind = kint), intent(in) :: nnod, nedge
      integer(kind = kint), intent(in) :: ie_edge(nedge,2)
      integer(kind = kint), intent(in) :: ntot_graph_nod
      integer(kind = kint), intent(in) :: istack_graph_nod(0:nnod)
!
      integer(kind = kint), intent(inout) :: num_graph_nod(nnod)
      integer(kind = kint), intent(inout) :: igraph_nod(ntot_graph_nod)
!
      integer(kind = kint) :: iedge, inod1, inod2, icou
!
!
      num_graph_nod(1:nnod) = 0
      do iedge = 1, nedge
        inod1 = ie_edge(iedge,1)
        inod2 = ie_edge(iedge,2)
        num_graph_nod(inod1) = num_graph_nod(inod1) + 1
        num_graph_nod(inod2) = num_graph_nod(inod2) + 1
        icou = istack_graph_nod(inod1-1) + num_graph_nod(inod1)
        igraph_nod(icou) = inod2
        icou = istack_graph_nod(inod2-1) + num_graph_nod(inod2)
        igraph_nod(icou) = inod1
      end do
!
      end subroutine set_graph_4_linear
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_graph_4_quad(nnod, nedge, ie_edge,               &
     &          num_graph_nod)
!
      integer(kind = kint), intent(in) :: nnod, nedge
      integer(kind = kint), intent(in) :: ie_edge(nedge,3)
!
      integer(kind = kint), intent(inout) :: num_graph_nod(nnod)
!
      integer(kind = kint) :: iedge, inod1, inod2, inod3
!
!
      num_graph_nod(1:nnod) = 0
      do iedge = 1, nedge
        inod1 = ie_edge(iedge,1)
        inod2 = ie_edge(iedge,2)
        inod3 = ie_edge(iedge,3)
        num_graph_nod(inod1) = num_graph_nod(inod1) + 1
        num_graph_nod(inod2) = num_graph_nod(inod2) + 2
        num_graph_nod(inod3) = num_graph_nod(inod3) + 1
      end do
!
      end subroutine count_graph_4_quad
!
! ----------------------------------------------------------------------
!
      subroutine set_graph_4_quad(nnod, nedge, ie_edge, ntot_graph_nod, &
     &          num_graph_nod, istack_graph_nod, igraph_nod)
!
      integer(kind = kint), intent(in) :: nnod, nedge
      integer(kind = kint), intent(in) :: ie_edge(nedge,3)
      integer(kind = kint), intent(in) :: ntot_graph_nod
      integer(kind = kint), intent(in) :: istack_graph_nod(0:nnod)
!
      integer(kind = kint), intent(inout) :: num_graph_nod(nnod)
      integer(kind = kint), intent(inout) :: igraph_nod(ntot_graph_nod)
!
      integer(kind = kint) :: iedge, inod1, inod2, inod3, icou
!
!
      num_graph_nod(1:nnod) = 0
      do iedge = 1, nedge
        inod1 = ie_edge(iedge,1)
        inod2 = ie_edge(iedge,2)
        inod3 = ie_edge(iedge,3)
        num_graph_nod(inod1) = num_graph_nod(inod1) + 1
        num_graph_nod(inod2) = num_graph_nod(inod2) + 2
        num_graph_nod(inod3) = num_graph_nod(inod3) + 1
        icou = istack_graph_nod(inod1-1) + num_graph_nod(inod1)
        igraph_nod(icou) = inod2
        icou = istack_graph_nod(inod2-1) + num_graph_nod(inod2) - 1
        igraph_nod(icou) = inod1
        icou = istack_graph_nod(inod2-1) + num_graph_nod(inod2)
        igraph_nod(icou) = inod3
        icou = istack_graph_nod(inod3-1) + num_graph_nod(inod3)
        igraph_nod(icou) = inod2
      end do
!
      end subroutine set_graph_4_quad
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_internal_graph(nnod, internal_node,              &
     &          ntot_graph_nod, istack_graph_nod, igraph_nod,           &
     &          num_graph_inter_nod)
!
      integer(kind = kint), intent(in) :: nnod, internal_node
      integer(kind = kint), intent(in) :: ntot_graph_nod
      integer(kind = kint), intent(in) :: istack_graph_nod(0:nnod)
      integer(kind = kint), intent(in) :: igraph_nod(ntot_graph_nod)
!
      integer(kind = kint), intent(inout)                               &
     &            :: num_graph_inter_nod(internal_node)
!
      integer(kind = kint) :: inod, inum, ist, ied
!
      num_graph_inter_nod(1:internal_node) = 0
      do inod = 1, internal_node
        ist = istack_graph_nod(inod-1) + 1
        ied = istack_graph_nod(inod)
        do inum = ist, ied
          if ( igraph_nod(inum) .le. internal_node) then
            num_graph_inter_nod(inod) = num_graph_inter_nod(inod) + 1
          end if
        end do
      end do
!
      end subroutine count_internal_graph
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_graph(nnod, internal_node,                &
     &          ntot_graph_nod, istack_graph_nod, igraph_nod,           &
     &          ntot_graph_inter_nod, num_graph_inter_nod,              &
     &          istack_graph_inter_nod, igraph_inter_nod)
!
      integer(kind = kint), intent(in) :: nnod, internal_node
      integer(kind = kint), intent(in) :: ntot_graph_nod
      integer(kind = kint), intent(in) :: istack_graph_nod(0:nnod)
      integer(kind = kint), intent(in) :: igraph_nod(ntot_graph_nod)
      integer(kind = kint), intent(in) :: ntot_graph_inter_nod
      integer(kind = kint), intent(in)                                  &
     &            :: istack_graph_inter_nod(0:internal_node)
!
      integer(kind = kint), intent(inout)                               &
     &            :: num_graph_inter_nod(internal_node)
      integer(kind = kint), intent(inout)                               &
     &            :: igraph_inter_nod(ntot_graph_inter_nod)
!
!
      integer(kind = kint) :: inod, inum, ist, ied, icou
!
      num_graph_inter_nod(1:internal_node) = 0
      do inod = 1, internal_node
        ist = istack_graph_nod(inod-1) + 1
        ied = istack_graph_nod(inod)
        do inum = ist, ied
          if ( igraph_nod(inum) .le. internal_node) then
            num_graph_inter_nod(inod) = num_graph_inter_nod(inod) + 1
            icou = istack_graph_inter_nod(inod-1)                       &
     &            + num_graph_inter_nod(inod)
            igraph_inter_nod(icou) = igraph_nod(inum)
          end if
        end do
      end do
!
      end subroutine set_internal_graph
!
! ----------------------------------------------------------------------
!
      end module set_geometry_graph
