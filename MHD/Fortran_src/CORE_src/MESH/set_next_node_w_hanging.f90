!       set_next_node_w_hanging.f90
!      module set_next_node_w_hanging
!
!      Written by H.Matsui on May., 2012
!
!      subroutine allocate_iflag_nod_hang(numnod)
!      subroutine deallocate_iflag_nod_hang
!
!      subroutine count_next_node_w_hanging(numnod, iflag_hang_nod,     &
!     &          nnod_hang4, inod_hang4, nnod_hang2, inod_hang2,        &
!     &          ntot_next_nod_4_node, inod_next_stack_4_node,          &
!     &          inod_next_4_node, nnod_next_node_hanged)
!      subroutine s_set_next_node_w_hanging(numnod, iflag_hang_nod,     &
!     &          nnod_hang4, inod_hang4, nnod_hang2, inod_hang2,        &
!     &          ntot_next_nod_4_node, inod_next_stack_4_node,          &
!     &          inod_next_4_node, ntot_next_node_hanged,               &
!     &          istack_next_node_hanged, inod_next_node_hanged)
!
      module set_next_node_w_hanging
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: iflag_nod(:)
      integer(kind = kint), parameter :: ihang_surf = 4
      integer(kind = kint), parameter :: ihang_edge = 2
!
      private :: iflag_nod
      private :: ihang_surf, ihang_edge
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_iflag_nod_hang(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( iflag_nod(numnod) )
      if(numnod .gt. 0)  iflag_nod(1:numnod) = 0
!
      end subroutine allocate_iflag_nod_hang
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_iflag_nod_hang
!
      deallocate( iflag_nod )
!
      end subroutine deallocate_iflag_nod_hang
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_next_node_w_hanging(numnod, iflag_hang_nod,      &
     &          nnod_hang4, inod_hang4, nnod_hang2, inod_hang2,         &
     &          ntot_next_nod_4_node, inod_next_stack_4_node,           &
     &          inod_next_4_node, nnod_next_node_hanged)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: iflag_hang_nod(2,numnod)
      integer(kind = kint), intent(in) :: nnod_hang4
      integer(kind = kint), intent(in) :: inod_hang4(5,nnod_hang4)
      integer(kind = kint), intent(in) :: nnod_hang2
      integer(kind = kint), intent(in) :: inod_hang2(3,nnod_hang2)
!
      integer(kind = kint), intent(in) :: ntot_next_nod_4_node
      integer(kind = kint), intent(in)                                  &
     &               :: inod_next_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &               :: inod_next_4_node(ntot_next_nod_4_node)
!
      integer(kind = kint), intent(inout)                               &
     &              :: nnod_next_node_hanged(numnod)
!
      integer(kind = kint) :: inod, ist, ied, inum, icou
      integer(kind = kint) :: jnod, jmin, jmax
      integer(kind = kint) :: knum, k1, knod
!
!
      do inod = 1, numnod
        iflag_nod(1:numnod) = 0
        iflag_nod(inod) =     1
!
        ist = inod_next_stack_4_node(inod-1) + 1
        ied = inod_next_stack_4_node(inod  )
        do inum = ist, ied
          jnod = inod_next_4_node(inum)
          iflag_nod(jnod) = 1
!
          if(iflag_hang_nod(1,jnod) .eq. ihang_surf) then
            knum = iflag_hang_nod(2,jnod)
            do k1 = 2, 5
              knod = inod_hang4(k1,inum)
              iflag_nod(knod) = 1
              jmin = min(jmin,knod)
              jmax = max(jmax,knod)
            end do
          else if(iflag_hang_nod(1,jnod) .eq. ihang_edge) then
            knum = iflag_hang_nod(2,jnod)
            do k1 = 2, 3
              knod = inod_hang2(k1,inum)
              iflag_nod(knod) = 1
              jmin = min(jmin,knod)
              jmax = max(jmax,knod)
            end do
          end if
!
        end do
!
        do jnod = jmin, jmax
          nnod_next_node_hanged(inod) = nnod_next_node_hanged(inod)     &
     &                                 + iflag_nod(inod)
        end do
      end do
!
      end subroutine count_next_node_w_hanging
!
!-----------------------------------------------------------------------
!
      subroutine s_set_next_node_w_hanging(numnod, iflag_hang_nod,      &
     &          nnod_hang4, inod_hang4, nnod_hang2, inod_hang2,         &
     &          ntot_next_nod_4_node, inod_next_stack_4_node,           &
     &          inod_next_4_node, ntot_next_node_hanged,                &
     &          istack_next_node_hanged, inod_next_node_hanged)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: iflag_hang_nod(2,numnod)
      integer(kind = kint), intent(in) :: nnod_hang4
      integer(kind = kint), intent(in) :: inod_hang4(5,nnod_hang4)
      integer(kind = kint), intent(in) :: nnod_hang2
      integer(kind = kint), intent(in) :: inod_hang2(3,nnod_hang2)
!
      integer(kind = kint), intent(in) :: ntot_next_nod_4_node
      integer(kind = kint), intent(in)                                  &
     &               :: inod_next_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &               :: inod_next_4_node(ntot_next_nod_4_node)
!
      integer(kind = kint), intent(in) :: ntot_next_node_hanged
      integer(kind = kint), intent(in)                                  &
     &              :: istack_next_node_hanged(0:numnod)
!
      integer(kind = kint), intent(inout)                               &
     &              :: inod_next_node_hanged(ntot_next_node_hanged)
!
      integer(kind = kint) :: inod, ist, ied, inum, icou
      integer(kind = kint) :: jnod, jmin, jmax
      integer(kind = kint) :: knum, k1, knod
!
!
      do inod = 1, numnod
        iflag_nod(1:numnod) = 0
        iflag_nod(inod) =     1
!
        ist = inod_next_stack_4_node(inod-1) + 1
        ied = inod_next_stack_4_node(inod  )
        do inum = ist, ied
          jnod = inod_next_4_node(inum)
          iflag_nod(jnod) = 1
!
          if(iflag_hang_nod(1,jnod) .eq. ihang_surf) then
            knum = iflag_hang_nod(2,jnod)
            do k1 = 2, 5
              knod = inod_hang4(k1,inum)
              iflag_nod(knod) = 1
              jmin = min(jmin,knod)
              jmax = max(jmax,knod)
            end do
          else if(iflag_hang_nod(1,jnod) .eq. ihang_edge) then
            knum = iflag_hang_nod(2,jnod)
            do k1 = 2, 3
              knod = inod_hang2(k1,inum)
              iflag_nod(knod) = 1
              jmin = min(jmin,knod)
              jmax = max(jmax,knod)
            end do
          end if
!
        end do
!
        icou = istack_next_node_hanged(inod-1) + 1
        inod_next_node_hanged(icou) = inod
!
        do jnod = jmin, inod-1
          if(iflag_nod(jnod) .gt. 0) then
            icou = icou + 1
            inod_next_node_hanged(icou) = jnod
          end if
        end do
        do jnod = inod+1, jmax
          if(iflag_nod(jnod) .gt. 0) then
            icou = icou + 1
            inod_next_node_hanged(icou) = jnod
          end if
        end do
      end do
!
      end subroutine s_set_next_node_w_hanging
!
!-----------------------------------------------------------------------
!
      end module set_next_node_w_hanging
