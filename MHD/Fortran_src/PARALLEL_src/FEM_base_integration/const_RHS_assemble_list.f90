!const_RHS_assemble_list.f90
!     module const_RHS_assemble_list
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on July, 2006
!
!      subroutine set_connect_RHS_assemble
!
      module const_RHS_assemble_list
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
      subroutine set_connect_RHS_assemble
!
      use m_element_id_4_node
      use m_next_node_id_4_node
      use m_sorted_node
!
!
!      Search surrounding node and element
!
      call set_belonged_ele_and_next_nod
!
!      set RHS assemble table
!
      call sort_node_index
!
      end subroutine set_connect_RHS_assemble
!
!-----------------------------------------------------------------------
!
      subroutine set_belonged_ele_and_next_nod
!
      use m_geometry_data
      use m_element_id_4_node
      use m_next_node_id_4_node
!
!      Search surrounding node and element
!
      call set_ele_id_4_node
      call const_next_nod_id_4_node
!
      end subroutine set_belonged_ele_and_next_nod
!
!-----------------------------------------------------------------------
!
      end module const_RHS_assemble_list
