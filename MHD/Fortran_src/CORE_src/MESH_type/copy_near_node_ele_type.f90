!copy_near_node_ele_type.f90
!      module copy_near_node_ele_type
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine copy_next_ele_id_2_near_type(node, next_ele, near_ele)
!        type(node_data), intent(in) :: node
!        type(element_around_node), intent(in) :: next_ele
!        type(near_mesh), intent(inout) :: near_ele
!> Substitution of
!> @n      (subroutine  copy_next_element_id_2_near)
!
!      subroutine copy_next_nod_id_2_near_type(node, next_nod, near_nod)
!        type(node_data), intent(in) :: node
!        type(next_nod_id_4_nod), intent(in) :: next_nod
!        type(near_mesh), intent(inout) :: near_nod
!> Substitution of
!> @n      (subroutine  copy_next_node_id_2_near)
!
!      subroutine copy_wider_id_2_near_type(node,                       &
!     &          near_tbl, near_tbl_wide)
!        type(node_data), intent(in) :: node
!        type(near_mesh), intent(inout) :: near_tbl_wide
!        type(near_mesh), intent(inout) :: near_tbl
!> Substitution of
!> @n      (subroutine  copy_wider_node_id_2_near)
!
!!      subroutine copy_extended_ele_id(numnod, near_tbl, near_tbl_wide)
!!        integer(kind = kint), intent(in) :: numnod
!!        type(near_mesh), intent(inout) :: near_tbl_wide
!!        type(near_mesh), intent(inout) :: near_tbl
!
      module copy_near_node_ele_type
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
      subroutine copy_next_ele_id_2_near_type(node, next_ele, near_ele)
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_near_mesh_id_4_node
!
      type(node_data), intent(in) :: node
      type(element_around_node), intent(in) :: next_ele
      type(near_mesh), intent(inout) :: near_ele
!
!
      call alloc_num_4_near_nod(node%numnod, near_ele)
!
      near_ele%num_nod(1:node%numnod)                                   &
     &    = next_ele%nele_4_node(1:node%numnod)
      near_ele%istack_nod(0:node%numnod)                                &
     &    = next_ele%istack_4_node(0:node%numnod)
!
      near_ele%ntot = next_ele%ntot
      near_ele%nmax = next_ele%nmax
      near_ele%nmin = next_ele%nmin
!
      call alloc_near_element(near_ele)
!
      near_ele%id_near_nod(1:near_ele%ntot)                             &
     &    = next_ele%iele_4_node(1:near_ele%ntot)
!
      end subroutine copy_next_ele_id_2_near_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_next_nod_id_2_near_type(node, next_nod, near_nod)
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_near_mesh_id_4_node
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: next_nod
      type(near_mesh), intent(inout) :: near_nod
!
      integer(kind = kint) :: inod, i
!
!
      call alloc_num_4_near_nod(node%numnod, near_nod)
!
      near_nod%num_nod(1:node%numnod)                                   &
     &     = next_nod%nnod_next(1:node%numnod)
      near_nod%istack_nod(0:node%numnod)                                &
     &     = next_nod%istack_next(0:node%numnod)
!
      near_nod%ntot = next_nod%ntot
      near_nod%nmax = next_nod%nmax
      near_nod%nmin = next_nod%nmin
!
      call alloc_near_node(near_nod)
!
      near_nod%id_near_nod(1:near_nod%ntot)                             &
     &           = next_nod%inod_next(1:near_nod%ntot)
!
      near_nod%idist(1:near_nod%ntot) = 1
      near_nod%iweight(1:near_nod%ntot)                                 &
     &           = next_nod%iweight_next(1:near_nod%ntot)
!
      do inod = 1, node%numnod
        i = next_nod%istack_next(inod-1) + 1
        near_nod%idist(i) = 0
      end do
!
      end subroutine copy_next_nod_id_2_near_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_wider_id_2_near_type(numnod,                      &
     &          near_tbl, near_tbl_wide)
!
      use t_near_mesh_id_4_node
!
      integer(kind = kint), intent(in) :: numnod
      type(near_mesh), intent(inout) :: near_tbl_wide
      type(near_mesh), intent(inout) :: near_tbl
!
!
      call dealloc_near_node(near_tbl)
!
      near_tbl%num_nod(1:numnod) = near_tbl_wide%num_nod(1:numnod)
      near_tbl%istack_nod(0:numnod)                                     &
     &      = near_tbl_wide%istack_nod(0:numnod)
!
      near_tbl%ntot = near_tbl_wide%ntot
      near_tbl%nmax = near_tbl_wide%nmax
      near_tbl%nmin = near_tbl_wide%nmin
!
      call alloc_near_node(near_tbl)
!
      near_tbl%id_near_nod(1:near_tbl%ntot)                             &
     &           = near_tbl_wide%id_near_nod(1:near_tbl%ntot)
!
      near_tbl%idist(1:near_tbl%ntot)                                   &
     &           = near_tbl_wide%idist(1:near_tbl%ntot)
      near_tbl%iweight(1:near_tbl%ntot)                                 &
     &           = near_tbl_wide%iweight(1:near_tbl%ntot)
!
      end subroutine copy_wider_id_2_near_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_extended_ele_id(numnod, near_tbl, near_tbl_wide)
!
      use t_near_mesh_id_4_node
!
      integer(kind = kint), intent(in) :: numnod
      type(near_mesh), intent(inout) :: near_tbl_wide
      type(near_mesh), intent(inout) :: near_tbl
!
!
      call dealloc_near_node(near_tbl)
!
      near_tbl%num_nod(1:numnod) = near_tbl_wide%num_nod(1:numnod)
      near_tbl%istack_nod(0:numnod)                                     &
     &      = near_tbl_wide%istack_nod(0:numnod)
!
      near_tbl%ntot = near_tbl_wide%ntot
      near_tbl%nmax = near_tbl_wide%nmax
      near_tbl%nmin = near_tbl_wide%nmin
!
      call alloc_near_element(near_tbl)
!
      near_tbl%id_near_nod(1:near_tbl%ntot)                             &
     &           = near_tbl_wide%id_near_nod(1:near_tbl%ntot)
!
      end subroutine copy_extended_ele_id
!
!-----------------------------------------------------------------------
!
      end module copy_near_node_ele_type
