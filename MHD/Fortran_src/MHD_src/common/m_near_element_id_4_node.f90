!
!      module m_near_element_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
!      subroutine const_next_nod_id_w_hang
!      subroutine overwrite_next_nod_by_hanged
!      subroutine deallocate_next_nod_w_hang
!      subroutine check_next_node_id_nod_hang(my_rank, numnod)
!
      module m_near_element_id_4_node
!
      use m_precision
      use t_near_mesh_id_4_node
      use t_hanging_mesh_data
      use t_next_node_ele_4_node
!
      implicit none
!
!
!> structure of surrounded node, element, surface, edge for each node
      type(fem_near_mesh), save :: near_mesh1
!
!>  Structure for hanging nodes on mesh
      type(hanging_mesh), save :: hang1
!>  Structure of neighbouring node list for hanging node
      type(next_nod_id_4_nod), save :: neib_hang1
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_next_nod_id_w_hang
!
      use m_geometry_data
      use m_element_id_4_node
      use expand_next_nod_hang_type
!
!
      call const_next_nod_hang_type(node1, hang1%nod_hang,              &
     &    next_tbl1%neib_nod, neib_hang1)
!
!
      end subroutine const_next_nod_id_w_hang
!
!-----------------------------------------------------------------------
!
      subroutine overwrite_next_nod_by_hanged
!
      use m_geometry_data
      use m_element_id_4_node
      use expand_next_nod_hang_type
!
!
      call overwrt_next_nod_by_hang_type(node1, neib_hang1,             &
     &    next_tbl1%neib_nod)
!
      end subroutine overwrite_next_nod_by_hanged
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_next_nod_w_hang
!
      call dealloc_inod_next_node(neib_hang1)
!
      end subroutine deallocate_next_nod_w_hang
!
!-----------------------------------------------------------------------
!
      subroutine check_next_node_id_nod_hang(my_rank, numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
!
      call check_next_node_id_4_node(my_rank, numnod, neib_hang1)
!
      end subroutine check_next_node_id_nod_hang
!
!-----------------------------------------------------------------------
!
      end module m_near_element_id_4_node
