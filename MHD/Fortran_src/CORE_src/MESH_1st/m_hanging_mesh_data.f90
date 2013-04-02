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
!
      implicit none
!
      integer(kind = kint), allocatable :: iflag_hang_nod(:,:)
      integer(kind = kint), allocatable :: iflag_hang_edge(:,:)
      integer(kind = kint), allocatable :: iflag_hang_surf(:,:)
!
      integer(kind = kint) :: nnod_hang4
      integer(kind = kint), allocatable :: inod_hang4(:,:)
      integer(kind = kint) :: nnod_hang2
      integer(kind = kint), allocatable :: inod_hang2(:,:)
!
      integer(kind = kint) :: nedge_hang2
      integer(kind = kint), allocatable :: iedge_hang2(:,:)
!
      integer(kind = kint) :: nsurf_hang4
      integer(kind = kint), allocatable :: isurf_hang4(:,:)
      integer(kind = kint) :: nsurf_hang2
      integer(kind = kint), allocatable :: isurf_hang2(:,:)
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
      allocate(iflag_hang_nod(2,nnod))
      allocate(iflag_hang_surf(2,nsurf))
      allocate(iflag_hang_edge(2,nedge))
!
      if(nnod .gt.  0) iflag_hang_nod =  0
      if(nsurf .gt. 0) iflag_hang_surf = 0
      if(nedge .gt. 0) iflag_hang_edge = 0
!
      end subroutine allocate_iflag_hanging
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_iflag_hanging
!
!
      deallocate(iflag_hang_nod)
      deallocate(iflag_hang_surf)
      deallocate(iflag_hang_edge)
!
      end subroutine deallocate_iflag_hanging
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_inod_hang
!
!
      allocate(inod_hang4(5,nnod_hang4))
      allocate(inod_hang2(3,nnod_hang2))
!
      if(nnod_hang4 .gt. 0) inod_hang4 = 0
      if(nnod_hang2 .gt. 0) inod_hang2 = 0
!
      end subroutine allocate_inod_hang
!
!-----------------------------------------------------------------------
!
      subroutine allocate_isurf_hang
!
!
      allocate(isurf_hang4(5,nsurf_hang4))
      allocate(isurf_hang2(3,nsurf_hang2))
      if(nsurf_hang4 .gt. 0) isurf_hang4 = 0
      if(nsurf_hang2 .gt. 0) isurf_hang2 = 0
!
      end subroutine allocate_isurf_hang
!
!-----------------------------------------------------------------------
!
      subroutine allocate_iedge_hang
!
!
      allocate(iedge_hang2(3,nedge_hang2))
      if(nedge_hang2 .gt. 0) iedge_hang2 = 0
!
      end subroutine allocate_iedge_hang
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_inod_hang
!
!
      deallocate(inod_hang4, inod_hang2)
!
      end subroutine deallocate_inod_hang
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_isurf_hang
!
!
      deallocate(isurf_hang4, isurf_hang2)
!
      end subroutine deallocate_isurf_hang
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_iedge_hang
!
!
      deallocate(iedge_hang2)
!
      end subroutine deallocate_iedge_hang
!
!-----------------------------------------------------------------------
!
      end module m_hanging_mesh_data
