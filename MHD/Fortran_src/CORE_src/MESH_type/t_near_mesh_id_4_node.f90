!t_near_mesh_id_4_node.f90
!      module t_near_mesh_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
!> @brief structure of surrounded node, element, surface, edge
!>      for each node
!
!> Substitution of
!> @n      (module m_near_node_id_4_node)
!> @n      (module m_near_element_id_4_node)
!> @n      (module m_near_surface_id_4_node)
!> @n      (module m_near_edge_id_4_node)
!
!      subroutine alloc_num_4_near_nod(node, near_tbl)
!      subroutine alloc_near_node(near_tbl)
!
!      subroutine dealloc_num_4_near_node(near_tbl)
!      subroutine dealloc_near_node(near_tbl)
!
!      subroutine check_near_nod_4_node(my_rank, node, near_tbl)
!        integer(kind = kint), intent(in) :: my_rank
!        type(node_data), intent(in) :: node
!        type(near_mesh), intent(in) :: near_tbl
!
!
      module t_near_mesh_id_4_node
!
      use m_precision
!
      implicit none
!
!> structure of surrounded table for each node
      type near_mesh
!>       Total number of surrounding data
        integer(kind = kint) :: ntot
!>       Maximum number of surrounding data
        integer(kind = kint) :: nmax
!>       Mimimum number of surrounding data
        integer(kind = kint) :: nmin
!>       Number of surrounding data for each node
        integer(kind = kint), pointer :: num_nod(:)
!>       Stack number of surrounding data for each node
        integer(kind = kint), pointer :: istack_nod(:)
!>       Surrounding data ID for each node
        integer(kind = kint), pointer :: id_near_nod(:)
!>       distance from center node
        integer(kind = kint), pointer :: idist(:)
!>       weighting of surrounded node
        integer(kind = kint), pointer :: iweight(:)
      end type near_mesh
!
!
!> structure of surrounded node, element, surface, edge for each node
      type fem_near_mesh
!> structure of surrounded node for each node
        type(near_mesh) :: near_node_tbl
!> structure of surrounded node for each node
        type(near_mesh) :: near_node_wide
!
!> structure of surrounded element for each node
        type(near_mesh) :: near_ele_tbl
!> structure of surrounded element for each node
        type(near_mesh) :: near_ele_wide
!
!> structure of surrounded surface for each node
        type(near_mesh) :: near_surf_tbl
!> structure of surrounded surface for each node
        type(near_mesh) :: near_surf_wide
!
!> structure of surrounded edge for each node
        type(near_mesh) :: near_edge_tbl
!> structure of surrounded edge for each node
        type(near_mesh) :: near_edge_wide
      end type fem_near_mesh
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_num_4_near_nod(node, near_tbl)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(near_mesh), intent(inout) :: near_tbl
!
!
      allocate(near_tbl%num_nod(node%numnod))
      allocate(near_tbl%istack_nod(0:node%numnod))
!
      near_tbl%nmax = 0
      near_tbl%nmin = 0
      near_tbl%num_nod =       0
      near_tbl%istack_nod =    0
!
      end subroutine alloc_num_4_near_nod
!
! -----------------------------------------------------------------------
!
      subroutine alloc_near_node(near_tbl)
!
      type(near_mesh), intent(inout) :: near_tbl
!
!
      allocate(near_tbl%id_near_nod(near_tbl%ntot))
      allocate(near_tbl%idist(near_tbl%ntot))
      allocate(near_tbl%iweight(near_tbl%ntot))
      near_tbl%id_near_nod = 0
      near_tbl%idist =       0
      near_tbl%iweight =     0
!
      end subroutine alloc_near_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_4_near_node(near_tbl)
!
      type(near_mesh), intent(inout) :: near_tbl
!
!
      deallocate(near_tbl%num_nod)
      deallocate(near_tbl%istack_nod)
!
      end subroutine dealloc_num_4_near_node
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_near_node(near_tbl)
!
      type(near_mesh), intent(inout) :: near_tbl
!
!
      deallocate(near_tbl%id_near_nod)
      deallocate(near_tbl%idist)
      deallocate(near_tbl%iweight)
!
      end subroutine dealloc_near_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_near_nod_4_node(my_rank, node, near_tbl)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(near_mesh), intent(in) :: near_tbl
!
      integer(kind = kint) :: inod, ist, ied
!
!
      write(50+my_rank,*) 'max and min. of near node ID for node ',     &
     &                    near_tbl%nmax, near_tbl%nmin
      do inod = 1, node%numnod
        ist = near_tbl%istack_nod(inod-1) + 1
        ied = near_tbl%istack_nod(inod)
        write(50+my_rank,*) 'near node ID for node num_nod',      &
     &                     inod, ist, ied, near_tbl%num_nod(inod)
        write(50+my_rank,'(8i16)') near_tbl%id_near_nod(ist:ied)
        write(50+my_rank,*) 'iweight '
        write(50+my_rank,'(8i16)') near_tbl%iweight(ist:ied)
        write(50+my_rank,*) 'idist '
        write(50+my_rank,'(8i16)') near_tbl%idist(ist:ied)
      end do
!
      end subroutine check_near_nod_4_node
!
! -----------------------------------------------------------------------
!
      end module t_near_mesh_id_4_node
