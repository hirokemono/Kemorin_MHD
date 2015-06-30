!
!      module m_near_edge_id_4_node
!
!      Written by H. Matsui on Aug., 2007
!
!     subroutine allocate_num_4_near_edge(numnod)
!     subroutine allocate_num_4_near_edge_w(numnod)
!
!     subroutine allocate_near_edge
!     subroutine allocate_near_edge_w
!
!     subroutine deallocate_num_4_near_edge
!     subroutine deallocate_num_4_near_edge_w
!
!     subroutine deallocate_near_edge
!     subroutine deallocate_near_edge_w
!
!      subroutine check_near_edge_4_node(my_rank,numnod)
!
      module m_near_edge_id_4_node
!
      use m_precision
      use t_near_mesh_id_4_node
!
      implicit none
!
!> structure of surrounded edge for each node
        type(near_mesh), save :: near_edge_tbl
!> structure of surrounded edge for each node
        type(near_mesh), save :: near_edge_wide
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_edge(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      call alloc_num_4_near_nod(numnod, near_edge_tbl)
!
      end subroutine allocate_num_4_near_edge
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_edge_w(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      call alloc_num_4_near_nod(numnod, near_edge_wide)
!
      end subroutine allocate_num_4_near_edge_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_edge
!
      call alloc_near_node(near_edge_tbl)
!
      end subroutine allocate_near_edge
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_edge_w
!
      call alloc_near_node(near_edge_wide)
!
      end subroutine allocate_near_edge_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_edge
!
      call dealloc_num_4_near_node(near_edge_tbl)
!
      end subroutine deallocate_num_4_near_edge
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_edge_w
!
      call dealloc_num_4_near_node(near_edge_wide)
!
      end subroutine deallocate_num_4_near_edge_w
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_edge
!
      call dealloc_near_node(near_edge_tbl)
!
      end subroutine deallocate_near_edge
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_edge_w
!
      call dealloc_near_node(near_edge_wide)
!
      end subroutine deallocate_near_edge_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_near_edge_4_node(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
!
      call check_near_4_nod_t(my_rank, numnod, near_edge_tbl)
!
      end subroutine check_near_edge_4_node
!
! -----------------------------------------------------------------------
!
      end module m_near_edge_id_4_node
