!const_RHS_assemble_list.f90
!     module const_RHS_assemble_list
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on July, 2006
!
!      subroutine set_connect_RHS_assemble
!      subroutine set_connect_for_fieldline
!
      module const_RHS_assemble_list
!
      use m_precision
!
      use set_element_id_4_node
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
      use ordering_4_rhs_assemble
!
!
!  found surrounding node and element
!
      call set_ele_id_4_node
!
      call const_next_nod_id_4_node
!
!      set RHS assemble table
!
      call sort_node_index
!
      end subroutine set_connect_RHS_assemble
!
!-----------------------------------------------------------------------
!
      subroutine set_connect_for_fieldline
!
!
!  found surrounding node and element
!
      call set_ele_id_4_node
!
      end subroutine set_connect_for_fieldline
!
!-----------------------------------------------------------------------
!
      end module const_RHS_assemble_list
