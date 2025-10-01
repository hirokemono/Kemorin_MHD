!>@file   set_comm_tbl_to_new_part.f90
!!@brief  module set_comm_tbl_to_new_part
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Subroutines to set commnunication table to new partitionning
!!
!!@verbatim
!!      subroutine count_num_export_for_repart(my_rank, nprocs,         &
!!     &          num_send_tmp, iflag_self, nrank_export)
!!      subroutine count_num_import_for_repart                          &
!!     &         (nprocs, num_recv_tmp, nrank_import)
!!
!!      subroutine set_istack_export_for_repart(my_rank, nprocs,        &
!!     &          num_send_tmp, nrank_export, ntot_export, irank_export,&
!!     &          num_export, istack_export)
!!      subroutine set_istack_import_for_repart                         &
!!     &         (my_rank, nprocs, num_recv_tmp, nrank_import,          &
!!     &          ntot_import, irank_import, num_import, istack_import)
!!
!!      subroutine set_export_item_for_repart                           &
!!     &         (my_rank, nprocs, part_grp, num_send_tmp,              &
!!     &          nrank_export, ntot_export, istack_export, item_export)
!!      subroutine set_import_item_for_repart                           &
!!     &         (ntot_import, item_import, ntot_rev)
!!        integer(kind = kint), intent(in) :: ntot_import
!!        integer(kind = kint), intent(in) :: item_import(ntot_import)
!!        integer(kind = kint), intent(inout) :: irev_import(ntot_rev)
!!      subroutine set_import_rev_for_repart                            &
!!     &         (ntot_import, ntot_rev, item_import, irev_import)
!!        integer(kind = kint), intent(in) :: ntot_rev, ntot_import
!!        integer(kind = kint), intent(in) :: item_import(ntot_import)
!!        integer(kind = kint), intent(inout) :: irev_import(ntot_rev)
!!@endverbatim
!
      module set_comm_tbl_to_new_part
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_num_export_for_repart(my_rank, nprocs,           &
     &          num_send_tmp, iflag_self, nrank_export)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: num_send_tmp(nprocs)
!
      integer(kind = kint), intent(inout) :: iflag_self
      integer(kind = kint), intent(inout) :: nrank_export
!
      integer(kind = kint) :: i
!
      if(num_send_tmp(my_rank+1) .gt. 0) iflag_self = 1
!
      nrank_export = 0
      do i = 1, nprocs
        if(num_send_tmp(i) .gt. 0) nrank_export = nrank_export + 1
      end do
!
      end subroutine count_num_export_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine count_num_import_for_repart                            &
     &         (nprocs, num_recv_tmp, nrank_import)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in)  :: num_recv_tmp(nprocs)
!
      integer(kind = kint), intent(inout) :: nrank_import
!
      integer(kind = kint) :: i
!
!
      nrank_import = 0
      do i = 1, nprocs
        if(num_recv_tmp(i) .gt. 0)  nrank_import = nrank_import + 1
      end do
!
      end subroutine count_num_import_for_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_istack_export_for_repart(my_rank, nprocs,          &
     &          num_send_tmp, nrank_export, ntot_export, irank_export,  &
     &          num_export, istack_export)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: num_send_tmp(nprocs)
      integer(kind = kint), intent(in) :: nrank_export
!
      integer(kind = kint), intent(inout) :: ntot_export
      integer(kind = kint), intent(inout) :: irank_export(nrank_export)
      integer(kind = kint), intent(inout) :: num_export(nrank_export)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_export(0:nrank_export)
!
      integer(kind = kint) :: i, ip, icou
!
      icou = 0
      istack_export(0) = 0
      do i = 1, nprocs
        ip = 1 + mod(i+my_rank,nprocs)
        if(num_send_tmp(ip) .gt. 0) then
          icou = icou + 1
          irank_export(icou) =  ip-1
          num_export(icou) =    num_send_tmp(ip)
          istack_export(icou) = istack_export(icou-1)                   &
     &                          + num_send_tmp(ip)
        end if
      end do
      ntot_export = istack_export(nrank_export)
!
      end subroutine set_istack_export_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_istack_import_for_repart                           &
     &         (my_rank, nprocs, num_recv_tmp, nrank_import,            &
     &          ntot_import, irank_import, num_import, istack_import)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in)  :: num_recv_tmp(nprocs)
!
      integer(kind = kint), intent(in) :: nrank_import
!
      integer(kind = kint), intent(inout) :: ntot_import
      integer(kind = kint), intent(inout) :: irank_import(nrank_import)
      integer(kind = kint), intent(inout) :: num_import(nrank_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_import(0:nrank_import)
!
      integer(kind = kint) :: i, inum, icou, ip
!
!
      icou = 0
      istack_import(0) = 0
      do i = 1, nprocs
        ip = 1 + mod(i+my_rank,nprocs)
        if(num_recv_tmp(ip) .gt. 0) then
          icou = icou + 1
          irank_import(icou) =  ip-1
          num_import(icou) = num_recv_tmp(ip)
          istack_import(icou) = istack_import(icou-1)                   &
     &                         + num_import(icou)
        end if
      end do
      ntot_import = istack_import(nrank_import)
!
      end subroutine set_istack_import_for_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_export_item_for_repart                             &
     &         (my_rank, nprocs, part_grp, num_send_tmp,                &
     &          nrank_export, ntot_export, istack_export, item_export)
!
      use t_group_data
!
      type(group_data), intent(in) :: part_grp
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in)                                  &
     &                     :: num_send_tmp(nprocs)
!
      integer(kind = kint), intent(in) :: nrank_export, ntot_export
      integer(kind = kint), intent(in) :: istack_export(0:nrank_export)
!
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
!
      integer(kind = kint) :: i, ip, inum, icou, ist, num, jst
!
      icou = 0
      do i = 1, nprocs
        ip = 1 + mod(i+my_rank,nprocs)
        if(num_send_tmp(ip) .gt. 0) then
          icou = icou + 1
          ist = part_grp%istack_grp(ip-1)
          jst = istack_export(icou-1)
          num = istack_export(icou) - istack_export(icou-1)
!$omp parallel do
          do inum = 1, num
            item_export(jst+inum) = part_grp%item_grp(ist+inum)
          end do
!$omp end parallel do
        end if
      end do
!
      end subroutine set_export_item_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_import_item_for_repart                             &
     &         (ntot_import, item_import, ntot_rev)
!
      integer(kind = kint), intent(in) :: ntot_import
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
      integer(kind = kint), intent(inout) :: ntot_rev
!
      integer(kind = kint) :: inum
!
!
!$omp parallel do
      do inum = 1, ntot_import
        item_import(inum) = inum
      end do
!$omp end parallel do
      ntot_rev = ntot_import
!
      end subroutine set_import_item_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_import_rev_for_repart                              &
     &         (ntot_import, ntot_rev, item_import, irev_import)
!
      integer(kind = kint), intent(in) :: ntot_rev, ntot_import
      integer(kind = kint), intent(in) :: item_import(ntot_import)
!
      integer(kind = kint), intent(inout) :: irev_import(ntot_rev)
!
      integer(kind = kint) :: inum, inod
!
!
!$omp parallel do private(inum,inod)
      do inum = 1, ntot_import
        inod = item_import(inum)
        irev_import(inod) = inum
      end do
!$omp end parallel do
!
      end subroutine set_import_rev_for_repart
!
! ----------------------------------------------------------------------
!
      end module set_comm_tbl_to_new_part
