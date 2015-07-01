!copy_near_node_and_element.f90
!      module copy_near_node_and_element
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine copy_next_element_id_2_near
!      subroutine copy_next_node_id_2_near
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
!
      subroutine copy_next_node_id_2_near
!
      use m_geometry_parameter
      use m_next_node_id_4_node
      use m_near_node_id_4_node
!
      integer(kind = kint) :: inod, i
!
!
     call alloc_num_4_near_nod(numnod, near_node1_tbl)
!
      near_node1_tbl%num_nod(1:numnod) = neib_nod1%nnod_next(1:numnod)
      near_node1_tbl%istack_nod(0:numnod)                               &
     &    = neib_nod1%istack_next(0:numnod)
!
      near_node1_tbl%ntot = neib_nod1%ntot
      near_node1_tbl%nmax = neib_nod1%nmax
      near_node1_tbl%nmin = neib_nod1%nmin
!
      call alloc_near_node(near_node1_tbl)
!
      near_node1_tbl%id_near_nod(1:neib_nod1%ntot)                      &
     &           = neib_nod1%inod_next(1:neib_nod1%ntot)
!
      near_node1_tbl%idist(1:neib_nod1%ntot) = 1
      near_node1_tbl%iweight(1:neib_nod1%ntot)                          &
     &           = neib_nod1%iweight_next(1:neib_nod1%ntot)
!
      do inod = 1, numnod
        i = neib_nod1%istack_next(inod-1) + 1
        near_node1_tbl%idist(i) = 0
      end do
!
      end subroutine copy_next_node_id_2_near
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
