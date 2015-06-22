!
!      module m_near_element_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
!     subroutine allocate_num_4_near_ele(numnod)
!
!     subroutine allocate_near_element
!     subroutine allocate_near_element_w
!
!     subroutine deallocate_num_4_near_ele
!
!     subroutine deallocate_near_element
!     subroutine deallocate_near_element_w
!
      module m_near_element_id_4_node
!
      use m_precision
      use  t_near_mesh_id_4_node
!
      implicit none
!
!> structure of surrounded element for each node
        type(near_mesh), save :: near_elef1_tbl
!> structure of surrounded element for each node
        type(near_mesh), save :: near_ele1_wide
!
!     element informations surrounded elements for each node
!
      integer(kind = kint) :: ntot_ele_near_nod
      integer(kind = kint) :: nmax_ele_near_nod, nmin_ele_near_nod
      integer(kind = kint), allocatable :: nele_near_nod(:)
!
!      near_elef1_tbl%num_nod
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_ele(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(near_elef1_tbl%num_nod(numnod))
      allocate(near_elef1_tbl%istack_nod(0:numnod))
!
      nmax_ele_near_nod = 0
      nmin_ele_near_nod = 0
      near_elef1_tbl%num_nod = 0
      near_elef1_tbl%istack_nod = 0
!
      end subroutine allocate_num_4_near_ele
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_element
!
      allocate(near_elef1_tbl%id_near_nod(ntot_ele_near_nod))
      near_elef1_tbl%id_near_nod = 0
!
      end subroutine allocate_near_element
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_ele
!
      deallocate(near_elef1_tbl%num_nod)
      deallocate(near_elef1_tbl%istack_nod)
!
      end subroutine deallocate_num_4_near_ele
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_element
!
      deallocate(near_elef1_tbl%id_near_nod)
!
      end subroutine deallocate_near_element
!
! -----------------------------------------------------------------------
!
      end module m_near_element_id_4_node
