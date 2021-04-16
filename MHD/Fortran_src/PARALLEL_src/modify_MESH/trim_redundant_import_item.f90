!>@file   trim_redundant_import_item.f90
!!@brief  module trim_redundant_import_item
!!
!!@author H. Matsui
!!@date Programmed in March, 2021
!
!>@brief  Trim redundant impoert items from sorted import list
!!
!!@verbatim
!!      integer(kind = kint) function count_ntot_trimmed_import         &
!!     &                            (nprocs, sorted_import)
!!      subroutine count_trimmed_import_stack                           &
!!     &         (nprocs, ntot_new_import, inod_lc_import_tmp,          &
!!     &          num_import_tmp, istack_import_tmp,                    &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item)
!!        integer, intent(in) :: nprocs
!!        integer(kind= kint), intent(in) :: ntot_new_import
!!        integer(kind= kint), intent(in)                               &
!!     &                    :: inod_lc_import_tmp(ntot_new_import)
!!        integer(kind= kint), intent(in) :: num_import_tmp(nprocs)
!!        integer(kind= kint), intent(in) :: istack_import_tmp(0:nprocs)
!!        integer(kind= kint), intent(in) :: ntot_trimmed_nod_import
!!        integer(kind= kint), intent(inout)                            &
!!     &         :: istack_trimmed_import_pe(0:nprocs)
!!        integer(kind= kint), intent(inout)                            &
!!     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!!      subroutine trim_internal_import_items(nprocs,                   &
!!     &          ntot_new_import, irank_nod_new_import, nitem_sort,    &
!!     &          index_4_import_tmp, irank_origin_new_import,          &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item, idx_home_sorted_import,   &
!!     &          num_miss)
!!      subroutine trim_external_import_items(nprocs, ntot_new_import,  &
!!     &          irank_nod_new_import, nitem_sort, index_4_import_tmp, &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item, idx_home_sorted_import,   &
!!     &          num_miss)
!!      subroutine trim_orphaned_import_items                           &
!!     &         (nprocs, nitem_sort, index_4_import_tmp,               &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item, idx_home_sorted_import,   &
!!     &          num_miss)
!!        integer, intent(in) :: nprocs
!!        integer(kind = kint), intent(in) :: ntot_new_import
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: irank_nod_new_import(ntot_new_import)
!!        integer(kind = kint), intent(in)                              &
!!        integer(kind = kint), intent(in) :: nitem_sort
!!     &                    :: index_4_import_tmp(nitem_sort)
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: irank_origin_new_import(nitem_sort)
!!        integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
!!        integer(kind= kint), intent(in)                               &
!!     &         :: istack_trimmed_import_pe(0:nprocs)
!!        integer(kind= kint), intent(in)                               &
!!     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!!        integer(kind = kint), intent(inout)                           &
!!     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
!!        integer(kind = kint), intent(inout) :: num_miss
!!@endverbatim
      module trim_redundant_import_item
!
      use m_precision
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function count_ntot_trimmed_import           &
     &                            (nprocs, sorted_import)
!
      integer, intent(in) :: nprocs
      type(sort_data_for_sleeve_trim), intent(in) :: sorted_import
!
      integer(kind = kint) :: ist, inum, ip, ntot
!
!
      ntot = 0
      do ip = 1, nprocs
        ist = sorted_import%istack_sorted_by_pe(ip-1)
        do inum = 1, sorted_import%num_sorted_by_pe(ip) - 1
          if(sorted_import%iref_lc_import(ist+inum)                     &
     &        .ne. sorted_import%iref_lc_import(ist+inum+1)) then
            ntot = ntot + 1
          end if
        end do
        if(sorted_import%num_sorted_by_pe(ip) .gt. 0) ntot = ntot + 1
      end do
      count_ntot_trimmed_import = ntot
!
      end function count_ntot_trimmed_import
!
!  ---------------------------------------------------------------------
!
      subroutine count_trimmed_import_stack(nprocs, sorted_import,      &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item)
!
      type(sort_data_for_sleeve_trim), intent(in) :: sorted_import
      integer, intent(in) :: nprocs
!
      integer(kind= kint), intent(in) :: ntot_trimmed_nod_import
!
      integer(kind= kint), intent(inout)                                &
     &         :: istack_trimmed_import_pe(0:nprocs)
      integer(kind= kint), intent(inout)                                &
     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!
      integer(kind = kint) :: ist, inum, icou, ip
!
!
      icou = 0
      istack_trimmed_import_pe(0) =   0
      istack_trimmed_import_item(0) = 0
      do ip = 1, nprocs
        ist = sorted_import%istack_sorted_by_pe(ip-1)
        do inum = 1, sorted_import%num_sorted_by_pe(ip) - 1
          if(sorted_import%iref_lc_import(ist+inum)                   &
     &        .ne. sorted_import%iref_lc_import(ist+inum+1)) then
            icou = icou + 1
            istack_trimmed_import_item(icou) = ist + inum
          end if
        end do
        if(sorted_import%num_sorted_by_pe(ip) .gt. 0) then
          icou = icou + 1
          istack_trimmed_import_item(icou)                              &
     &         = ist + sorted_import%num_sorted_by_pe(ip)
        end if
        istack_trimmed_import_pe(ip) =    icou
      end do
!
      end subroutine count_trimmed_import_stack
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine trim_internal_import_items(nprocs,                     &
     &          ntot_new_import, irank_nod_new_import, nitem_sort,      &
     &          index_4_import_tmp, irank_origin_new_import,            &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item, idx_home_sorted_import,     &
     &          num_miss)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_new_import
      integer(kind = kint), intent(in)                                  &
     &                    :: irank_nod_new_import(ntot_new_import)
!
      integer(kind = kint), intent(in) :: nitem_sort
      integer(kind = kint), intent(in)                                  &
     &                    :: index_4_import_tmp(nitem_sort)
      integer(kind = kint), intent(in)                                  &
     &                    :: irank_origin_new_import(nitem_sort)
!
      integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_pe(0:nprocs)
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
      integer(kind = kint), intent(inout) :: num_miss
!
      integer(kind = kint) :: ist, ied, inum, ip
      integer(kind = kint) :: jst, jed, jnum, jrank, jsort
!
!
      do ip = 1, nprocs
        ist = istack_trimmed_import_pe(ip-1) + 1
        ied = istack_trimmed_import_pe(ip)
        do inum = ist, ied
          if(idx_home_sorted_import(inum) .ne. 0) cycle
!
          jst = istack_trimmed_import_item(inum-1) + 1
          jed = istack_trimmed_import_item(inum)
          do jnum = jst, jed
            jsort = abs(index_4_import_tmp(jnum))
            jrank = irank_origin_new_import(jsort)
            if(     irank_nod_new_import(jsort) .eq. jrank              &
     &        .and. irank_nod_new_import(jsort) .eq. ip-1               &
     &        .and. index_4_import_tmp(jnum) .gt. 0) then
              idx_home_sorted_import(inum) = index_4_import_tmp(jnum)
              exit
            end if
          end do
        end do
      end do
!
      num_miss = 0
      do inum = 1, ntot_trimmed_nod_import
        if(idx_home_sorted_import(inum) .eq. 0) num_miss = num_miss + 1
      end do
!
      end subroutine trim_internal_import_items
!
!  ---------------------------------------------------------------------
!
      subroutine trim_original_import_items                             &
     &         (nprocs, nitem_sort, index_4_import_tmp,                 &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item, idx_home_sorted_import,     &
     &          num_miss)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: nitem_sort
      integer(kind = kint), intent(in)                                  &
     &                    :: index_4_import_tmp(nitem_sort)
!
      integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_pe(0:nprocs)
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
      integer(kind = kint), intent(inout) :: num_miss
!
      integer(kind = kint) :: ist, ied, inum, ip
      integer(kind = kint) :: jst, jed, jnum
!
!
      do ip = 1, nprocs
        ist = istack_trimmed_import_pe(ip-1) + 1
        ied = istack_trimmed_import_pe(ip)
        do inum = ist, ied
          jst = istack_trimmed_import_item(inum-1) + 1
          jed = istack_trimmed_import_item(inum)
          do jnum = jst, jed
            if(index_4_import_tmp(jnum) .lt. 0) then
              idx_home_sorted_import(inum) = index_4_import_tmp(jnum)
              exit
            end if
          end do
        end do
      end do
!
      num_miss = 0
      do inum = 1, ntot_trimmed_nod_import
        if(idx_home_sorted_import(inum) .eq. 0) num_miss = num_miss + 1
      end do
!
      end subroutine trim_original_import_items
!
!  ---------------------------------------------------------------------
!
      subroutine trim_external_import_items(nprocs, ntot_new_import,    &
     &          irank_nod_new_import, nitem_sort, index_4_import_tmp,   &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item, idx_home_sorted_import,     &
     &          num_miss)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_new_import
      integer(kind = kint), intent(in)                                  &
     &                    :: irank_nod_new_import(ntot_new_import)
!
      integer(kind = kint), intent(in) :: nitem_sort
      integer(kind = kint), intent(in)                                  &
     &                    :: index_4_import_tmp(nitem_sort)
!
      integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_pe(0:nprocs)
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
      integer(kind = kint), intent(inout) :: num_miss
!
      integer(kind = kint) :: ist, ied, inum, ip
      integer(kind = kint) :: jst, jed, jnum, jsort
!
!
      do ip = 1, nprocs
        ist = istack_trimmed_import_pe(ip-1) + 1
        ied = istack_trimmed_import_pe(ip)
        do inum = ist, ied
          if(idx_home_sorted_import(inum) .ne. 0) cycle
!
          jst = istack_trimmed_import_item(inum-1) + 1
          jed = istack_trimmed_import_item(inum)
          do jnum = jst, jed
            jsort = abs(index_4_import_tmp(jnum))
            if(irank_nod_new_import(jsort) .eq. (ip-1)                  &
     &           .and. index_4_import_tmp(jnum) .gt. 0) then
              idx_home_sorted_import(inum) = jsort
              exit
            end if
          end do
        end do
      end do
!
      num_miss = 0
      do inum = 1, ntot_trimmed_nod_import
        if(idx_home_sorted_import(inum) .eq. 0) num_miss = num_miss + 1
      end do
!
      end subroutine trim_external_import_items
!
!  ---------------------------------------------------------------------
!
      subroutine trim_orphaned_import_items                             &
     &         (nprocs, nitem_sort, index_4_import_tmp,                 &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item, idx_home_sorted_import,     &
     &          num_miss)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: nitem_sort
      integer(kind = kint), intent(in)                                  &
     &                    :: index_4_import_tmp(nitem_sort)
!
      integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_pe(0:nprocs)
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
      integer(kind = kint), intent(inout) :: num_miss
!
      integer(kind = kint) :: ist, ied, inum, ip
      integer(kind = kint) :: jst, jed
!
!
      do ip = 1, nprocs
        ist = istack_trimmed_import_pe(ip-1) + 1
        ied = istack_trimmed_import_pe(ip)
        do inum = ist, ied
          if(idx_home_sorted_import(inum) .ne. 0) cycle
!
          jst = istack_trimmed_import_item(inum-1) + 1
          jed = istack_trimmed_import_item(inum)
          idx_home_sorted_import(inum) = abs(index_4_import_tmp(jst))
        end do
      end do
!
      num_miss = 0
      do inum = 1, ntot_trimmed_nod_import
        if(idx_home_sorted_import(inum) .eq. 0) num_miss = num_miss + 1
      end do
!
      end subroutine trim_orphaned_import_items
!
!  ---------------------------------------------------------------------
!
      end module trim_redundant_import_item
