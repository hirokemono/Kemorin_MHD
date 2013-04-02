!
!      module m_near_edge_id_4_node
!
!      Written by H. Matsui on Aug., 2007
!
!     subroutine allocate_num_4_near_edge(numnod)
!     subroutine allocate_num_4_near_edge_w(numnod)
!
!     subroutine allocate_near_edge
!     subroutine allocate_near_edge_w
!
!     subroutine deallocate_num_4_near_edge
!     subroutine deallocate_num_4_near_edge_w
!
!     subroutine deallocate_near_edge
!     subroutine deallocate_near_edge_w
!
!      subroutine check_near_edge_4_node(my_rank,numnod)
!
      module m_near_edge_id_4_node
!
      use m_precision
!
      implicit none
!
!     edge informations surrounded edges for each node
!
      integer(kind = kint) :: ntot_edge_near_nod
      integer(kind = kint) :: nmax_edge_near_nod, nmin_edge_near_nod
      integer(kind = kint), allocatable :: nedge_near_nod(:)
      integer(kind = kint), allocatable :: iedge_stack_near_nod(:)
      integer(kind = kint), allocatable :: iedge_near_nod(:)
!
!     edge informations surrounded edges for each node
!
      integer(kind = kint) :: ntot_edge_near_nod_w
      integer(kind = kint) :: nmax_edge_near_nod_w
      integer(kind = kint) :: nmin_edge_near_nod_w
      integer(kind = kint), allocatable :: nedge_near_nod_w(:)
      integer(kind = kint), allocatable :: iedge_stack_near_nod_w(:)
      integer(kind = kint), allocatable :: iedge_near_nod_w(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_edge(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(nedge_near_nod(numnod))
      allocate(iedge_stack_near_nod(0:numnod))
!
      nmax_edge_near_nod = 0
      nmin_edge_near_nod = 0
      nedge_near_nod = 0
      iedge_stack_near_nod = 0
!
      end subroutine allocate_num_4_near_edge
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_edge_w(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(nedge_near_nod_w(numnod))
      allocate(iedge_stack_near_nod_w(0:numnod))
!
      nmax_edge_near_nod_w = 0
      nmin_edge_near_nod_w = 0
      nedge_near_nod_w = 0
      iedge_stack_near_nod_w = 0
!
      end subroutine allocate_num_4_near_edge_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_edge
!
      allocate(iedge_near_nod(ntot_edge_near_nod))
      iedge_near_nod = 0
!
      end subroutine allocate_near_edge
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_edge_w
!
      allocate(iedge_near_nod_w(ntot_edge_near_nod_w))
      iedge_near_nod_w = 0
!
      end subroutine allocate_near_edge_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_edge
!
      deallocate(nedge_near_nod)
      deallocate(iedge_stack_near_nod)
!
      end subroutine deallocate_num_4_near_edge
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_edge_w
!
      deallocate(nedge_near_nod_w)
      deallocate(iedge_stack_near_nod_w)
!
      end subroutine deallocate_num_4_near_edge_w
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_edge
!
      deallocate(iedge_near_nod)
!
      end subroutine deallocate_near_edge
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_edge_w
!
      deallocate(iedge_near_nod_w)
!
      end subroutine deallocate_near_edge_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_near_edge_4_node(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'max and min. of near edge for node ',        &
     &                    nmax_edge_near_nod, nmin_edge_near_nod
      do inod = 1, numnod
        ist = iedge_stack_near_nod(inod-1) + 1
        ied = iedge_stack_near_nod(inod)
        write(50+my_rank,*) 'near edge ID for node ',                   &
     &                     inod, ist, ied, nedge_near_nod(inod)
        write(50+my_rank,'(8i10)') iedge_near_nod(ist:ied)
      end do
!
      end subroutine check_near_edge_4_node
!
! -----------------------------------------------------------------------
!
      end module m_near_edge_id_4_node
