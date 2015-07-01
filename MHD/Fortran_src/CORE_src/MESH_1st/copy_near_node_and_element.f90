!copy_near_node_and_element.f90
!      module copy_near_node_and_element
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine copy_next_element_id_2_near
!      subroutine copy_next_node_id_2_near
!      subroutine copy_wider_node_id_2_near
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
      near_node1_tbl%num_nod(1:numnod) = nnod_next_4_node(1:numnod)
      near_node1_tbl%istack_nod(0:numnod)                               &
     &    = inod_next_stack_4_node(0:numnod)
!
      near_node1_tbl%ntot = ntot_next_nod_4_node
      near_node1_tbl%nmax = nmax_next_nod_4_node
      near_node1_tbl%nmin = nmin_next_nod_4_node
!
      call alloc_near_node(near_node1_tbl)
!
      near_node1_tbl%id_near_nod(1:ntot_next_nod_4_node)                &
     &           = inod_next_4_node(1:ntot_next_nod_4_node)
!
      near_node1_tbl%idist(1:ntot_next_nod_4_node) = 1
      near_node1_tbl%iweight(1:ntot_next_nod_4_node)                    &
     &           = iweight_next_4_node(1:ntot_next_nod_4_node)
!
      do inod = 1, numnod
        i = inod_next_stack_4_node(inod-1) + 1
        near_node1_tbl%idist(i) = 0
      end do
!
      end subroutine copy_next_node_id_2_near
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_wider_node_id_2_near
!
      use m_geometry_parameter
      use m_near_node_id_4_node
!
!
      call dealloc_near_node(near_node1_tbl)
!
      near_node1_tbl%num_nod(1:numnod) = near_node1_wide%num_nod(1:numnod)
      near_node1_tbl%istack_nod(0:numnod)                               &
     &     = near_node1_wide%istack_nod(0:numnod)
!
      near_node1_tbl%ntot = near_node1_wide%ntot
      near_node1_tbl%nmax = near_node1_wide%nmax
      near_node1_tbl%nmin = near_node1_wide%nmin
!
      call alloc_near_node(near_node1_tbl)
!
      near_node1_tbl%id_near_nod(1:near_node1_tbl%ntot)                 &
     &           = near_node1_wide%id_near_nod(1:near_node1_tbl%ntot)
!
      near_node1_tbl%idist(1:near_node1_tbl%ntot)                       &
     &           = near_node1_wide%idist(1:near_node1_tbl%ntot)
      near_node1_tbl%iweight(1:near_node1_tbl%ntot)                     &
     &           = near_node1_wide%iweight(1:near_node1_tbl%ntot)
!
      call dealloc_near_node(near_node1_wide)
      call dealloc_num_4_near_node(near_node1_wide)
!
      end subroutine copy_wider_node_id_2_near
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
