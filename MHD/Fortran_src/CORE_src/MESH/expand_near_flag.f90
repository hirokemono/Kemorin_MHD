!expand_near_flag.f90
!      module expand_near_flag
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine allocate_iflag_expand(numnod)
!      subroutine deallocate_iflag_expand
!
!      subroutine set_expand_flag(numnod, internal_node, nref_neib,     &
!     &          nnod_near_nod, iflag_expand, iflag_finish)
!
      module expand_near_flag
!
      use m_precision
!
      implicit none
!
      integer(kind= kint), allocatable :: iflag_expand(:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_iflag_expand(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(iflag_expand(numnod))
      iflag_expand = 1
!
      end subroutine allocate_iflag_expand
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_iflag_expand
!
      deallocate(iflag_expand)
!
      end subroutine deallocate_iflag_expand
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_expand_flag(numnod, internal_node, nref_neib,      &
     &          nnod_near_nod, iflag_expand, iflag_finish)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: nref_neib
      integer(kind = kint), intent(in) :: nnod_near_nod(numnod)
!
      integer(kind = kint), intent(inout) :: iflag_expand(numnod)
      integer(kind = kint), intent(inout) :: iflag_finish
!
      integer(kind = kint) :: inod
!
!
      iflag_finish = 0
      do inod = 1, internal_node
        if      (nnod_near_nod(inod) .eq. 0) then
          iflag_expand(inod) = 0
        else if (nnod_near_nod(inod) .eq. 1) then
          iflag_expand(inod) = 0
        else if (nnod_near_nod(inod) .ge. nref_neib) then
          iflag_expand(inod) = 0
        else
!          write(*,*) 'flag1', inod, nnod_near_nod(inod)
          iflag_expand(inod) = 1
          iflag_finish = 1
        end if
      end do
!
      do inod = internal_node+1, numnod
        iflag_expand(inod) = 0
      end do
!
      end subroutine set_expand_flag
!
!-----------------------------------------------------------------------
!
      end module expand_near_flag
