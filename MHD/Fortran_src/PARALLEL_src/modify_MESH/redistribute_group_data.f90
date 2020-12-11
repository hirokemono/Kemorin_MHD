!>@file   redistribute_group_data.f90
!!@brief  module redistribute_group_data
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Routines to re-distribute group data
!!
!!@verbatim
!!      subroutine mark_org_group_repart                                &
!!     &         (igrp, n_point, org_grp, iflag_org)
!!        type(group_data), intent(inout) :: org_grp
!!      subroutine mark_org_surf_group_repart                           &
!!     &         (igrp, n_point, org_sf_grp, iflag_org)
!!        type(surface_group_data), intent(in) :: org_sf_grp
!!
!!      subroutine count_group_item_repart                              &
!!     &         (igrp, new_num, iflag_new, new_grp)
!!        type(group_data), intent(inout) :: new_grp
!!      subroutine count_surf_group_item_repart                         &
!!     &         (igrp, new_nele, iflag_new, new_sf_grp)
!!        type(surface_group_data), intent(inout) :: new_sf_grp
!!
!!      subroutine append_group_item_repart                             &
!!     &         (igrp, new_num, iflag_new, new_grp)
!!        type(group_data), intent(inout) :: new_grp
!!      subroutine append_surf_group_item_repart                        &
!!     &         (igrp, new_nele, iflag_new, new_sf_grp)
!!        type(surface_group_data), intent(inout) :: new_sf_grp
!!@endverbatim
!
      module redistribute_group_data
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_group_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine mark_org_group_repart                                  &
     &         (igrp, n_point, org_grp, iflag_org)
!
      integer(kind = kint), intent(in) :: igrp, n_point
      type(group_data), intent(in) :: org_grp
!
      integer(kind = kint), intent(inout) :: iflag_org(n_point)
!
      integer(kind = kint) :: inum, i, ist, ied
!
!
!$omp parallel workshare
      iflag_org(1:n_point) = 0
!$omp end parallel workshare
!
      ist = org_grp%istack_grp(igrp-1) + 1
      ied = org_grp%istack_grp(igrp)
!$omp parallel do private(inum,i)
      do inum = ist, ied
        i = org_grp%item_grp(inum)
        iflag_org(i) = 1
      end do
!$omp end parallel do
!
      end subroutine mark_org_group_repart
!
! ----------------------------------------------------------------------
!
      subroutine mark_org_surf_group_repart                             &
     &         (igrp, n_point, org_sf_grp, iflag_org)
!
      integer(kind = kint), intent(in) :: igrp, n_point
      type(surface_group_data), intent(in) :: org_sf_grp
!
      integer(kind = kint), intent(inout) :: iflag_org(n_point)
!
      integer(kind = kint) :: inum, i, ist, ied
!
!
!$omp parallel workshare
      iflag_org(1:n_point) = 0
!$omp end parallel workshare
!
      ist = org_sf_grp%istack_grp(igrp-1) + 1
      ied = org_sf_grp%istack_grp(igrp)
!$omp parallel do private(inum,i)
      do inum = ist, ied
        i = org_sf_grp%item_sf_grp(1,inum)
        iflag_org(i) = org_sf_grp%item_sf_grp(2,inum)
      end do
!$omp end parallel do
!
      end subroutine mark_org_surf_group_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_group_item_repart                                &
     &         (igrp, new_num, iflag_new, new_grp)
!
      integer(kind = kint), intent(in) :: igrp, new_num
      integer(kind = kint), intent(in) :: iflag_new(new_num)
!
      type(group_data), intent(inout) :: new_grp
!
      integer(kind = kint) :: inum, i
!
!
      new_grp%nitem_grp(igrp) = 0
      do i = 1, new_num
        new_grp%nitem_grp(igrp)                                         &
     &        = new_grp%nitem_grp(igrp) + iflag_new(i)
      end do
!
      new_grp%istack_grp(igrp) = new_grp%istack_grp(igrp-1)             &
     &                          + new_grp%nitem_grp(igrp)
      do inum = igrp+1, new_grp%num_grp
        new_grp%istack_grp(inum) = new_grp%istack_grp(inum-1)
      end do
!
      end subroutine count_group_item_repart
!
! ----------------------------------------------------------------------
!
      subroutine count_surf_group_item_repart                           &
     &         (igrp, new_nele, iflag_new, new_sf_grp)
!
      integer(kind = kint), intent(in) :: igrp, new_nele
      integer(kind = kint), intent(in) :: iflag_new(new_nele)
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint), allocatable :: item_tmp(:,:)
!
      integer(kind = kint) :: inum, iele
!
!
      new_sf_grp%nitem_grp(igrp) = 0
      do iele = 1, new_nele
        if(iflag_new(iele) .gt. 0) then
          new_sf_grp%nitem_grp(igrp) = new_sf_grp%nitem_grp(igrp) + 1
        end if
      end do
      new_sf_grp%istack_grp(igrp) = new_sf_grp%istack_grp(igrp-1)       &
     &                             + new_sf_grp%nitem_grp(igrp)
      do inum = igrp+1, new_sf_grp%num_grp
        new_sf_grp%istack_grp(inum)                                     &
     &        = new_sf_grp%istack_grp(inum-1)
      end do
!
      end subroutine count_surf_group_item_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine append_group_item_repart                               &
     &         (igrp, new_num, iflag_new, new_grp)
!
      integer(kind = kint), intent(in) :: igrp, new_num
      integer(kind = kint), intent(in) :: iflag_new(new_num)
!
      type(group_data), intent(inout) :: new_grp
!
      integer(kind = kint), allocatable :: item_tmp(:)
      integer(kind = kint) :: inum, i, ntot_prev
!
!
      ntot_prev = new_grp%istack_grp(igrp-1)
      allocate(item_tmp(ntot_prev))
!$omp parallel do private(inum)
      do inum = 1, ntot_prev
        item_tmp(inum) = new_grp%item_grp(inum)
      end do
!$omp end parallel do
!
      call dealloc_group_item(new_grp)
      call alloc_group_item(new_grp)
!$omp parallel do private(inum)
      do inum = 1, ntot_prev
        new_grp%item_grp(inum) = item_tmp(inum)
      end do
!$omp end parallel do
      deallocate(item_tmp)
!
      inum = ntot_prev
      do i = 1, new_num
        if(iflag_new(i) .gt. 0) then
          inum = inum + 1
          new_grp%item_grp(inum) = i
        end if
      end do
!
      end subroutine append_group_item_repart
!
! ----------------------------------------------------------------------
!
      subroutine append_surf_group_item_repart                          &
     &         (igrp, new_nele, iflag_new, new_sf_grp)
!
      integer(kind = kint), intent(in) :: igrp, new_nele
      integer(kind = kint), intent(in) :: iflag_new(new_nele)
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint), allocatable :: item_tmp(:,:)
!
      integer(kind = kint) :: inum, i, ntot_prev
!
!
      ntot_prev = new_sf_grp%istack_grp(igrp-1)
      allocate(item_tmp(2,ntot_prev))
!$omp parallel do private(inum)
      do inum = 1, ntot_prev
        item_tmp(1,inum) = new_sf_grp%item_sf_grp(1,inum)
        item_tmp(2,inum) = new_sf_grp%item_sf_grp(2,inum)
      end do
!$omp end parallel do
!
      call dealloc_sf_group_item(new_sf_grp)
      call alloc_sf_group_item(new_sf_grp)
!$omp parallel do private(inum)
      do inum = 1, ntot_prev
        new_sf_grp%item_sf_grp(1,inum) = item_tmp(1,inum)
        new_sf_grp%item_sf_grp(2,inum) = item_tmp(2,inum)
      end do
!$omp end parallel do
      deallocate(item_tmp)
!
      inum = ntot_prev
      do i = 1, new_nele
        if(iflag_new(i) .gt. 0) then
          inum = inum + 1
          new_sf_grp%item_sf_grp(1,inum) = i
          new_sf_grp%item_sf_grp(2,inum) = iflag_new(i)
        end if
      end do
!
      end subroutine append_surf_group_item_repart
!
! ----------------------------------------------------------------------
!
      end module redistribute_group_data
