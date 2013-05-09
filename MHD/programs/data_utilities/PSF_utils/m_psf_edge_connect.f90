!
!      module m_psf_edge_connect
!
!      Written by H. Matsui
!
!      subroutine find_psf_edges
!      subroutine deallocate_psf_edge
!
      module m_psf_edge_connect
!
      use m_psf_results
!
      implicit none
!
      integer(kind = kint) :: numedge_psf
      integer(kind = kint), allocatable :: iedge_psf(:,:)
!
      integer(kind = kint) :: nmax_hash
      integer(kind = kint), allocatable :: nele_hash(:)
      integer(kind = kint), allocatable :: istack_hash(:)
!
      integer(kind = kint) :: nedge_psf_w_overlap
      integer(kind = kint), allocatable :: iedge_ovlp(:,:)
      integer(kind = kint), allocatable :: iflag_edge(:)
!
      private :: nmax_hash, nele_hash, istack_hash
      private :: nedge_psf_w_overlap, iedge_ovlp, iflag_edge
      private :: allocate_psf_edge
      private :: allocate_psf_edge_hash, allocate_psf_edge_w_overlap
      private :: deallocate_psf_edge_hash
      private :: count_max_psf_edge_hash, set_psf_edge_hash
      private :: copy_psf_edge_w_overlap, mark_referred_edge
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine find_psf_edges
!
      integer(kind = kint) :: inum, icou
!
!
      call count_max_psf_edge_hash
      call allocate_psf_edge_hash
!
      call set_psf_edge_hash
!
      nedge_psf_w_overlap = 3*numele_psf
      call allocate_psf_edge_w_overlap
      call copy_psf_edge_w_overlap
      call mark_referred_edge
!
      numedge_psf = 0
      do inum = 1, nedge_psf_w_overlap
        numedge_psf = numedge_psf + iflag_edge(inum)
      end do
!
      write(*,*) 'numedge_psf', numedge_psf, 3*numele_psf
      call allocate_psf_edge
!
      icou = 0
      do inum = 1, nedge_psf_w_overlap
        if(iflag_edge(inum) .gt. 0) then
          icou = icou + 1
          iedge_psf(icou,1) = iedge_ovlp(inum,1)
          iedge_psf(icou,2) = iedge_ovlp(inum,2)
!           write(*,*) 'iedge_psf', icou, iedge_ovlp(inum,1:2)
        end if
      end do
!
      call deallocate_psf_edge_hash
!
      end subroutine find_psf_edges
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_psf_edge
!
      deallocate(iedge_psf)
!
      end subroutine deallocate_psf_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_edge
!
!
      allocate(iedge_psf(numedge_psf,2))
      if(numedge_psf .gt. 0) iedge_psf = 0
!
      end subroutine allocate_psf_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_edge_hash
!
!
      allocate(nele_hash(nmax_hash))
      allocate(istack_hash(0:nmax_hash))
      if(nmax_hash .gt. 0) then
        nele_hash = 0
        istack_hash = 0
      end if
!
      end subroutine allocate_psf_edge_hash
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_edge_w_overlap
!
!
      allocate(iedge_ovlp(nedge_psf_w_overlap,2))
      allocate(iflag_edge(nedge_psf_w_overlap))
      if(nedge_psf_w_overlap .gt. 0) then
        iedge_ovlp = 0
        iflag_edge = 1
      end if
!
      end subroutine allocate_psf_edge_w_overlap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_psf_edge_hash
!
!
      deallocate(iedge_ovlp, iflag_edge)
      deallocate(nele_hash, istack_hash)
!
      end subroutine deallocate_psf_edge_hash
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_max_psf_edge_hash
!
      integer(kind = kint) :: isurf, i1, i2, i3
!
!
      nmax_hash = 0
      do isurf = 1, numele_psf
        i1 = ie_psf(isurf,1) + ie_psf(isurf,2)
        i2 = ie_psf(isurf,2) + ie_psf(isurf,3)
        i3 = ie_psf(isurf,3) + ie_psf(isurf,1)
        nmax_hash = max(nmax_hash,i1)
        nmax_hash = max(nmax_hash,i2)
        nmax_hash = max(nmax_hash,i3)
      end do
!
      end subroutine count_max_psf_edge_hash
!
!-----------------------------------------------------------------------
!
      subroutine set_psf_edge_hash
!
      integer(kind = kint) :: isurf, inum, i1, i2, i3
!
!
      nele_hash = 0
      do isurf = 1, numele_psf
        i1 = ie_psf(isurf,1) + ie_psf(isurf,2)
        i2 = ie_psf(isurf,2) + ie_psf(isurf,3)
        i3 = ie_psf(isurf,3) + ie_psf(isurf,1)
        nele_hash(i1) = nele_hash(i1) + 1
        nele_hash(i2) = nele_hash(i2) + 1
        nele_hash(i3) = nele_hash(i3) + 1
      end do
      istack_hash(0) = 0
      do inum = 1, nmax_hash
        istack_hash(inum) = istack_hash(inum-1) + nele_hash(inum)
      end do
!
      end subroutine set_psf_edge_hash
!
!-----------------------------------------------------------------------
!
      subroutine copy_psf_edge_w_overlap
!
      integer(kind = kint) :: isurf, icou, i1, i2, i3
!
!
      nele_hash = 0
      do isurf = 1, numele_psf
        i1 = ie_psf(isurf,1) + ie_psf(isurf,2)
        nele_hash(i1) = nele_hash(i1) + 1
        icou = istack_hash(i1-1) + nele_hash(i1)
        iedge_ovlp(icou,1) = ie_psf(isurf,1)
        iedge_ovlp(icou,2) = ie_psf(isurf,2)
!
        i2 = ie_psf(isurf,2) + ie_psf(isurf,3)
        nele_hash(i2) = nele_hash(i2) + 1
        icou = istack_hash(i2-1) + nele_hash(i2)
        iedge_ovlp(icou,1) = ie_psf(isurf,2)
        iedge_ovlp(icou,2) = ie_psf(isurf,3)
!
        i3 = ie_psf(isurf,3) + ie_psf(isurf,1)
        nele_hash(i3) = nele_hash(i3) + 1
        icou = istack_hash(i3-1) + nele_hash(i3)
        iedge_ovlp(icou,1) = ie_psf(isurf,3)
        iedge_ovlp(icou,2) = ie_psf(isurf,1)
      end do
!
      end subroutine copy_psf_edge_w_overlap
!
!-----------------------------------------------------------------------
!
      subroutine mark_referred_edge
!
      integer(kind = kint) :: inum, icou, ist, ied, jcou
!
!
      iflag_edge = 1
      do inum = 1, nmax_hash
        ist = istack_hash(inum-1) + 1
        ied = istack_hash(inum  )
        iflag_edge(ist) = 1
        do icou = ist+1, ied
          do jcou = ist, icou
            if(iflag_edge(jcou) .gt. 0) then
              if    (iedge_ovlp(icou,1).eq.iedge_ovlp(jcou,1)           &
     &         .and. iedge_ovlp(icou,2).eq.iedge_ovlp(jcou,2)) then
                iflag_edge(icou) = 0
                exit
              end if
              if    (iedge_ovlp(icou,1).eq.iedge_ovlp(jcou,2)           &
     &         .and. iedge_ovlp(icou,2).eq.iedge_ovlp(jcou,1)) then
                iflag_edge(icou) = 0
                exit
              end if
            end if
          end do
        end do
      end do
!
      end subroutine mark_referred_edge
!
!-----------------------------------------------------------------------
!
      end module m_psf_edge_connect
