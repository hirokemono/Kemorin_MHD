!m_hanging_mesh_data.f90
!      module m_hanging_mesh_data
!
!      Written by H.Matsui on May., 2012
!
!      subroutine allocate_iflag_hanging(nnod, nsurf, nedge)
!      subroutine deallocate_iflag_hanging
!
!      subroutine allocate_inod_hang
!      subroutine allocate_isurf_hang
!      subroutine allocate_iedge_hang
!
!      subroutine deallocate_inod_hang
!      subroutine deallocate_isurf_hang
!      subroutine deallocate_iedge_hang
!
      module m_hanging_mesh_data
!
      use m_precision
      use t_hanging_mesh_data
!
      implicit none
!
!>     Structure for hanging nodes on mesh
      type(hanging_mesh), save :: hang1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_iflag_hanging(nnod, nsurf, nedge)
!
      integer(kind = kint), intent(in) :: nnod, nsurf, nedge
!
      call alloc_iflag_hanging_type(nnod, nsurf, nedge, hang1)
!
      end subroutine allocate_iflag_hanging
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_iflag_hanging
!
!
      call dealloc_iflag_hanging_type(hang1)
!
      end subroutine deallocate_iflag_hanging
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_inod_hang
!
!
      call alloc_id_hang(hang1%nod_hang)
!
      end subroutine allocate_inod_hang
!
!-----------------------------------------------------------------------
!
      subroutine allocate_isurf_hang
!
!
      call alloc_id_hang(hang1%surf_hang)
!
      end subroutine allocate_isurf_hang
!
!-----------------------------------------------------------------------
!
      subroutine allocate_iedge_hang
!
!
      call alloc_id_hang(hang1%edge_hang)
!
      end subroutine allocate_iedge_hang
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_inod_hang
!
!
      call dealloc_id_hang(hang1%nod_hang)
!
      end subroutine deallocate_inod_hang
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_isurf_hang
!
!
      call dealloc_id_hang(hang1%surf_hang)
!
      end subroutine deallocate_isurf_hang
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_iedge_hang
!
!
      call dealloc_id_hang(hang1%edge_hang)
!
      end subroutine deallocate_iedge_hang
!
!-----------------------------------------------------------------------
!
      end module m_hanging_mesh_data
