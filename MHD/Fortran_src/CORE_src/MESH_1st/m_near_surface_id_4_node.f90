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
!
      implicit none
!
!     surface informations surrounded surfaces for each node
!
      integer(kind = kint) :: ntot_surf_near_nod
      integer(kind = kint) :: nmax_surf_near_nod, nmin_surf_near_nod
      integer(kind = kint), allocatable :: nsurf_near_nod(:)
      integer(kind = kint), allocatable :: isurf_stack_near_nod(:)
      integer(kind = kint), allocatable :: isurf_near_nod(:)
!
!     surface informations surrounded surfaces for each node
!
      integer(kind = kint) :: ntot_surf_near_nod_w
      integer(kind = kint) :: nmax_surf_near_nod_w
      integer(kind = kint) :: nmin_surf_near_nod_w
      integer(kind = kint), allocatable :: nsurf_near_nod_w(:)
      integer(kind = kint), allocatable :: isurf_stack_near_nod_w(:)
      integer(kind = kint), allocatable :: isurf_near_nod_w(:)
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
      allocate(nsurf_near_nod(numnod))
      allocate(isurf_stack_near_nod(0:numnod))
!
      nmax_surf_near_nod = 0
      nmin_surf_near_nod = 0
      nsurf_near_nod = 0
      isurf_stack_near_nod = 0
!
      end subroutine allocate_num_4_near_surf
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_surf_w(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(nsurf_near_nod_w(numnod))
      allocate(isurf_stack_near_nod_w(0:numnod))
!
      nmax_surf_near_nod_w = 0
      nmin_surf_near_nod_w = 0
      nsurf_near_nod_w = 0
      isurf_stack_near_nod_w = 0
!
      end subroutine allocate_num_4_near_surf_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_surface
!
      allocate(isurf_near_nod(ntot_surf_near_nod))
      isurf_near_nod = 0
!
      end subroutine allocate_near_surface
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_surface_w
!
      allocate(isurf_near_nod_w(ntot_surf_near_nod_w))
      isurf_near_nod_w = 0
!
      end subroutine allocate_near_surface_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_surf
!
      deallocate(nsurf_near_nod)
      deallocate(isurf_stack_near_nod)
!
      end subroutine deallocate_num_4_near_surf
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_surf_w
!
      deallocate(nsurf_near_nod_w)
      deallocate(isurf_stack_near_nod_w)
!
      end subroutine deallocate_num_4_near_surf_w
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_surface
!
      deallocate(isurf_near_nod)
!
      end subroutine deallocate_near_surface
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_surface_w
!
      deallocate(isurf_near_nod_w)
!
      end subroutine deallocate_near_surface_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_near_surf_4_node(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'max and min. of near surface for node ',     &
     &                    nmax_surf_near_nod, nmin_surf_near_nod
      do inod = 1, numnod
        ist = isurf_stack_near_nod(inod-1) + 1
        ied = isurf_stack_near_nod(inod)
        write(50+my_rank,*) 'near surface ID for node ',                &
     &                     inod, ist, ied, nsurf_near_nod(inod)
        write(50+my_rank,'(8i10)') isurf_near_nod(ist:ied)
      end do
!
      end subroutine check_near_surf_4_node
!
! -----------------------------------------------------------------------
!
      end module m_near_surface_id_4_node
