!copy_near_node_and_element.f90
!      module copy_near_node_and_element
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine copy_next_element_id_2_near
!
!      subroutine deallocate_near_node
!      subroutine deallocate_near_element
!
      module copy_near_node_and_element
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_next_element_id_2_near
!
      use m_geometry_parameter
      use m_element_id_4_node
      use m_near_element_id_4_node
!
!
      call alloc_num_4_near_nod(numnod, near_ele1_tbl)
!
      near_ele1_tbl%num_nod(1:numnod) =    nele_4_node(1:numnod)
      near_ele1_tbl%istack_nod(0:numnod) = iele_stack_4_node(0:numnod)
!
      near_ele1_tbl%ntot = ntot_ele_4_node
      near_ele1_tbl%nmax = nmax_ele_4_node
      near_ele1_tbl%nmin = nmin_ele_4_node
!
      call alloc_near_element(near_ele1_tbl)
!
      near_ele1_tbl%id_near_nod(1:ntot_ele_4_node)                      &
     &     = iele_4_node(1:ntot_ele_4_node)
!
      end subroutine copy_next_element_id_2_near
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_near_node
!
      use m_near_node_id_4_node
!
      call dealloc_near_node(near_node1_tbl)
      call dealloc_num_4_near_node(near_node1_tbl)
!
      end subroutine deallocate_near_node
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_element
!
      use m_near_element_id_4_node
!
      call dealloc_num_4_near_node(near_ele1_tbl)
      call dealloc_near_node(near_ele1_tbl)
!
      end subroutine deallocate_near_element
!
!-----------------------------------------------------------------------
!
      end module copy_near_node_and_element
