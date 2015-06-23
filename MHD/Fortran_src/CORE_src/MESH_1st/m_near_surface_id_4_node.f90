!
!      module m_near_surface_id_4_node
!
!      Written by H. Matsui on Aug., 2007
!
!     subroutine allocate_num_4_near_surf(numnod)
!     subroutine allocate_num_4_near_surf_w(numnod)
!
!     subroutine allocate_near_surface
!     subroutine allocate_near_surface_w
!
!     subroutine deallocate_num_4_near_surf
!     subroutine deallocate_num_4_near_surf_w
!
!     subroutine deallocate_near_surface
!     subroutine deallocate_near_surface_w
!
!      subroutine check_near_surf_4_node(my_rank,numnod)
!
      module m_near_surface_id_4_node
!
      use m_precision
      use t_near_mesh_id_4_node
!
      implicit none
!
!> structure of surrounded surface for each node
        type(near_mesh), save :: near_surf1_tbl
!> structure of surrounded surface for each node
        type(near_mesh), save :: near_surf1_wide
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_surf(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      call alloc_num_4_near_nod(numnod, near_surf1_tbl)
!
      end subroutine allocate_num_4_near_surf
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_surf_w(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      call alloc_num_4_near_nod(numnod, near_surf1_wide)
!
      end subroutine allocate_num_4_near_surf_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_surface
!
      call alloc_near_node(near_surf1_tbl)
!
      end subroutine allocate_near_surface
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_surface_w
!
      call alloc_near_node(near_surf1_wide)
!
      end subroutine allocate_near_surface_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_surf
!
      call dealloc_num_4_near_node(near_surf1_tbl)
!
      end subroutine deallocate_num_4_near_surf
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_surf_w
!
      call dealloc_num_4_near_node(near_surf1_wide)
!
      end subroutine deallocate_num_4_near_surf_w
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_surface
!
      call dealloc_near_node(near_surf1_tbl)
!
      end subroutine deallocate_near_surface
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_surface_w
!
      call dealloc_near_node(near_surf1_wide)
!
      end subroutine deallocate_near_surface_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_near_surf_4_node(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
!
      call check_near_4_nod_t(my_rank, numnod, near_surf1_tbl)
!
      end subroutine check_near_surf_4_node
!
! -----------------------------------------------------------------------
!
      end module m_near_surface_id_4_node
