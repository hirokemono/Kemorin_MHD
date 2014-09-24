!
!      module m_cubed_sph_mesh
!
      module m_cubed_sph_mesh
!
!      Written by H. Matsui on Apr., 2006
!
      use m_precision
!
      implicit none
!
!   num. of node, element
      integer(kind = kint) :: numnod, numele, numedge, numsurf
      integer(kind = kint) :: numnod_20, numele_20
!   position
      real(kind = kreal) :: xyz(3)
!   connectivity
      integer(kind = kint) :: ie(8), ie20(20)
      integer(kind = kint) :: isurf(4), isurf8(8)
      integer(kind = kint) :: iedge(2), iedge3(3)
!
      integer(kind = kint), allocatable :: inod_stack(:)
      integer(kind = kint), allocatable :: iele_stack(:)
      integer(kind = kint), allocatable :: iedge_stack(:)
      integer(kind = kint), allocatable :: isurf_stack(:)
!
!       subroutine allocate_coarse_mesh_stack(max_coarse_level)
!       subroutine deallocate_coarse_mesh_stack
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
       subroutine allocate_coarse_mesh_stack(max_coarse_level)
!
       integer(kind = kint), intent(in) :: max_coarse_level
!
       allocate( inod_stack(0:max_coarse_level) )
       allocate( iele_stack(0:max_coarse_level) )
       allocate( iedge_stack(0:max_coarse_level) )
       allocate( isurf_stack(0:max_coarse_level) )
!
       inod_stack = 0
       iele_stack = 0
       iedge_stack = 0
       isurf_stack = 0
!
       end subroutine allocate_coarse_mesh_stack
!
!   --------------------------------------------------------------------
!
       subroutine deallocate_coarse_mesh_stack
!
       deallocate( inod_stack  )
       deallocate( iele_stack  )
       deallocate( iedge_stack )
       deallocate( isurf_stack )
!
       end subroutine deallocate_coarse_mesh_stack
!
!   --------------------------------------------------------------------
!
      end module m_cubed_sph_mesh
