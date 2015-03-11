!set_ele_list_4_itp_table.f90
!     module set_ele_list_4_itp_table
!
!     Written by H. Matsui on March, 2015
!
!>@file   set_ele_list_4_itp_table.f90
!!@brief  module set_ele_list_4_itp_table
!!
!!@author H. Matsui
!!@date        Written by H. Matsui in March, 2015
!!
!>     Construct search list for interpolation
!!
!!@verbatim
!!      subroutine set_block_boundary                                   &
!!     &         (nnod, xyz, nblock, xmin, xmax, x_block)
!!      subroutine set_block_list_4_target(nnod, xx, xmin, xmax,        &
!!     &          nblock, iblock)
!!
!!      subroutine count_ele_list_by_center(nele, x_ele, xmin, xmax,    &
!!     &          nblock, ntot_block, nele_list)
!!      subroutine set_ele_list_by_center(nele, x_ele, xmin, xmax,      &
!!     &          nblock, ntot_block, ntot_list, nele_list,             &
!!     &          istack_list, iele_list)
!!
!!      subroutine count_ele_list_with_range(nnod, nele, nnod_4_ele,    &
!!     &          xx, ie, xmin, xmax, xe_min, xe_max, nblock,           &
!!     &          ntot_block, nele_list)
!!      subroutine set_ele_list_with_range(nele, xmin, xmax,            &
!!     &          xe_min, xe_max, nblock, ntot_block, ntot_list,        &
!!     &          nele_list, istack_list, iele_list)
!!@endverbatim
!
      module set_ele_list_4_itp_table
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_block_boundary                                     &
     &         (nnod, xyz, nblock, xmin, xmax, x_block)
!
      integer(kind = kint), intent(in) :: nnod, nblock
      real(kind = kreal), intent(in) :: xyz(nnod)
      real(kind = kreal), intent(inout) :: x_block(0:nblock)
      real(kind = kreal), intent(inout) :: xmin, xmax
!
      integer(kind = kint) :: i
!
!
      xmax = maxval(xyz)
      xmin = minval(xyz)
!
      do i = 0, nblock - 1
        x_block(i) = xmin + (xmax - xmin) * dble(i) / dble(nblock)
      end do
      x_block(nblock) = xmax
!
      end subroutine set_block_boundary
!
!  ---------------------------------------------------------------------
!
      subroutine set_block_list_4_target(nnod, xx, xmin, xmax,          &
     &          nblock, iblock)
!
      integer(kind = kint), intent(in) :: nnod, nblock(3)
      real(kind = kreal), intent(in) :: xx(nnod,3)
      real(kind = kreal), intent(in) :: xmin(3), xmax(3)
!
      integer(kind = kint), intent(inout) :: iblock(nnod,3)
!
      integer(kind = kint) :: inod, nd, imax(3)
!
!
      imax = maxloc(xx, DIM=1)
      do nd = 1, 3
!
        do inod = 1, nnod
          iblock(inod,nd) = 1 + int(aint(dble(nblock(nd))               &
     &          * (xx(inod,nd) - xmin(nd)) / (xmax(nd) - xmin(nd))) )
        end do
        iblock(imax,nd) = nblock(nd)
      end do
!
      end subroutine set_block_list_4_target
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_ele_list_by_center(nele, x_ele, xmin, xmax,      &
     &          nblock, ntot_block, nele_list)
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in) :: x_ele(nele,3)
      real(kind = kreal), intent(in) :: xmin(3), xmax(3)
!
      integer(kind = kint), intent(in) :: ntot_block, nblock(3)
!
      integer(kind = kint), intent(inout) :: nele_list(ntot_block)
!
      integer(kind = kint) :: iele, nd, jblock
      integer(kind = kint) :: imin(3)
!
!
      nele_list(1:ntot_block) = 0
      do iele = 1, nele
        do nd = 1, 3
          imin(nd)  = 1 + int(aint(dble(nblock(nd))                     &
     &          * (x_ele(iele,nd) - xmin(nd)) / (xmax(nd) - xmin(nd))))
        end do
        if(imin(1).lt.1 .or. imin(1).gt.nblock(1)) cycle
        if(imin(2).lt.1 .or. imin(2).gt.nblock(2)) cycle
        if(imin(3).lt.1 .or. imin(3).gt.nblock(3)) cycle
!
        jblock = imin(1) + (imin(2)-1)*nblock(1)                        &
     &                   + (imin(3)-1)*nblock(1)*nblock(2)
        nele_list(jblock) = nele_list(jblock) + 1
      end do
!
      end subroutine count_ele_list_by_center
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_list_by_center(nele, x_ele, xmin, xmax,        &
     &          nblock, ntot_block, ntot_list, nele_list,               &
     &          istack_list, iele_list)
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in) :: x_ele(nele,3)
      real(kind = kreal), intent(in) :: xmin(3), xmax(3)
!
      integer(kind = kint), intent(in) :: ntot_block, nblock(3)
      integer(kind = kint), intent(in) :: ntot_list
!
      integer(kind = kint), intent(inout) :: nele_list(ntot_block)
      integer(kind = kint), intent(inout) :: istack_list(0:ntot_block)
      integer(kind = kint), intent(inout) :: iele_list(ntot_list)
!
      integer(kind = kint) :: iele, nd, jblock
      integer(kind = kint) :: icou
      integer(kind = kint) :: imin(3)
!
!
      nele_list(1:ntot_block) = 0
!
      do iele = 1, nele
        do nd = 1, 3
          imin(nd)  = 1 + int(aint(dble(nblock(nd))                     &
     &          * (x_ele(iele,nd) - xmin(nd)) / (xmax(nd) - xmin(nd))))
        end do
        if(imin(1).lt.1 .or. imin(1).gt.nblock(1)) cycle
        if(imin(2).lt.1 .or. imin(2).gt.nblock(2)) cycle
        if(imin(3).lt.1 .or. imin(3).gt.nblock(3)) cycle
!
        jblock = imin(1) + (imin(2)-1)*nblock(1)                        &
     &                   + (imin(3)-1)*nblock(1)*nblock(2)
        nele_list(jblock) = nele_list(jblock) + 1
        icou = nele_list(jblock) + istack_list(jblock-1)
        iele_list(icou) = iele
      end do
!
      end subroutine set_ele_list_by_center
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_ele_list_with_range(nnod, nele, nnod_4_ele,      &
     &          xx, ie, xmin, xmax, xe_min, xe_max, nblock,             &
     &          ntot_block, nele_list)
!
      integer(kind = kint), intent(in) :: nnod, nele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
      real(kind = kreal), intent(in) :: xmin(3), xmax(3)
!
      integer(kind = kint), intent(in) :: ntot_block, nblock(3)
!
      real(kind = kreal), intent(inout) ::  xe_min(nele,3)
      real(kind = kreal), intent(inout) ::  xe_max(nele,3)
      integer(kind = kint), intent(inout) :: nele_list(ntot_block)
!
      integer(kind = kint) :: inod, iele, k1, nd, jx, jy, jz, jblock
      integer(kind = kint) :: imin(3), imax(3)
!
!
      imax = maxloc(xx, DIM=1)
      nele_list(1:ntot_block) = 0
!
      do iele = 1, nele
        inod = ie(iele,1)
        xe_min(iele,1:3) = xx(inod,1:3)
        xe_max(iele,1:3) = xx(inod,1:3)
        do nd = 1, 3
          do k1 = 2, nnod_4_ele
            inod = ie(iele,k1)
            xe_min(iele,nd) = min(xe_min(iele,nd),xx(inod,nd))
            xe_max(iele,nd) = max(xe_max(iele,nd),xx(inod,nd))
          end do
          imin(nd)  = 1 + int(aint(dble(nblock(nd))                     &
     &        * (xe_min(iele,nd) - xmin(nd)) / (xmax(nd) - xmin(nd))))
          imax(nd)  = 1 + int(aint(dble(nblock(nd))                     &
     &        * (xe_max(iele,nd) - xmin(nd)) / (xmax(nd) - xmin(nd))))
          imin(nd) = max(imin(nd),1)
          imax(nd) = min(imax(nd),nblock(nd))
          if(xe_min(iele,nd) .gt. xmax(nd)) imin(nd) = nblock(nd) + 1
          if(xe_max(iele,nd) .gt. xmax(nd)) imax(nd) = nblock(nd) - 1
        end do
!
        do jx = imin(1), imax(1)
          do jy = imin(2), imax(2)
            do jz = imin(3), imax(3)
              jblock = jx + (jy-1)*nblock(1)                            &
     &                    + (jz-1)*nblock(1)*nblock(2)
              nele_list(jblock) = nele_list(jblock) + 1
            end do
          end do
        end do
!
      end do
!
      end subroutine count_ele_list_with_range
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_list_with_range(nele, xmin, xmax,              &
     &          xe_min, xe_max, nblock, ntot_block, ntot_list,          &
     &          nele_list, istack_list, iele_list)
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in) ::  xe_min(nele,3)
      real(kind = kreal), intent(in) ::  xe_max(nele,3)
      real(kind = kreal), intent(in) :: xmin(3), xmax(3)
!
      integer(kind = kint), intent(in) :: ntot_block, nblock(3)
      integer(kind = kint), intent(in) :: ntot_list
!
      integer(kind = kint), intent(inout) :: nele_list(ntot_block)
      integer(kind = kint), intent(inout) :: istack_list(0:ntot_block)
      integer(kind = kint), intent(inout) :: iele_list(ntot_list)
!
      integer(kind = kint) :: iele, nd, jx, jy, jz, jblock
      integer(kind = kint) :: icou
      integer(kind = kint) :: imin(3), imax(3)
!
!
      nele_list(1:ntot_block) = 0
!
      do iele = 1, nele
        do nd = 1, 3
          imin(nd)  = 1 + int(aint(dble(nblock(nd))                     &
     &        * (xe_min(iele,nd) - xmin(nd)) / (xmax(nd) - xmin(nd))))
          imax(nd)  = 1 + int(aint(dble(nblock(nd))                     &
     &        * (xe_max(iele,nd) - xmin(nd)) / (xmax(nd) - xmin(nd))))
          imin(nd) = max(imin(nd),1)
          imax(nd) = min(imax(nd),nblock(nd))
          if(xe_min(iele,nd) .gt. xmax(nd)) imin(nd) = nblock(nd) + 1
          if(xe_max(iele,nd) .gt. xmax(nd)) imax(nd) = nblock(nd) - 1
        end do
!
        do jx = imin(1), imax(1)
          do jy = imin(2), imax(2)
            do jz = imin(3), imax(3)
              jblock = jx + (jy-1)*nblock(1)                            &
     &                    + (jz-1)*nblock(1)*nblock(2)
              nele_list(jblock) = nele_list(jblock) + 1
              icou = nele_list(jblock) + istack_list(jblock-1)
              iele_list(icou) = iele
            end do
          end do
        end do
!
      end do
!
      end subroutine set_ele_list_with_range
!
!  ---------------------------------------------------------------------
!
      end module set_ele_list_4_itp_table
