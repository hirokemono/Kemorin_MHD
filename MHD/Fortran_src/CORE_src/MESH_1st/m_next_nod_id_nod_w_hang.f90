!m_next_nod_id_nod_w_hang.f90
!      module m_next_nod_id_nod_w_hang
!
!> @brief Neighbouring node list for each node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine const_next_nod_id_w_hang
!      subroutine overwrite_next_nod_by_hanged
!      subroutine deallocate_next_nod_w_hang
!      subroutine check_next_node_id_nod_hang(my_rank, numnod)
!
      module m_next_nod_id_nod_w_hang
!
      use m_precision
      use t_next_node_ele_4_node
!
      implicit none
!
!>   Structure of neighbouring node list for hanging node
      type(next_nod_id_4_nod), save :: neib_hang1
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
      use m_hanging_mesh_data
      use m_element_id_4_node
      use expand_next_nod_hang_type
!
!
      call const_next_nod_hang_type(node1, hang1%nod_hang, neib_nod1,   &
     &    neib_hang1)
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
      call overwrt_next_nod_by_hang_type(node1, neib_hang1,         &
     &    neib_nod1)
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
      integer(kind = kint) :: inod, ist, ied
!
      call check_next_node_id_4_node(my_rank, numnod, neib_hang1)
!
      end subroutine check_next_node_id_nod_hang
!
!-----------------------------------------------------------------------
!
      end module m_next_nod_id_nod_w_hang
