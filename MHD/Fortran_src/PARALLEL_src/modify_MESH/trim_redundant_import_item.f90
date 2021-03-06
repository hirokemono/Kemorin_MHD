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
!!     &                   (nprocs, ntot_new_import, inod_lc_import_tmp,&
!!     &                    num_import_tmp, istack_import_tmp)
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
!!      subroutine trim_internal_import_items                           &
!!     &         (nprocs, ntot_new_import, irank_nod_new_import,        &
!!     &          index_4_import_tmp, irank_origin_new_import,          &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item, idx_home_sorted_import,   &
!!     &          num_miss)
!!      subroutine trim_external_import_items(nprocs, ntot_new_import,  &
!!     &          irank_nod_new_import, index_4_import_tmp,             &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item, idx_home_sorted_import,   &
!!     &          num_miss)
!!      subroutine trim_orphaned_import_items                           &
!!     &         (nprocs, ntot_new_import, index_4_import_tmp,          &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item, idx_home_sorted_import,   &
!!     &          num_miss)
!!        integer, intent(in) :: nprocs
!!        integer(kind = kint), intent(in) :: ntot_new_import
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: irank_nod_new_import(ntot_new_import)
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: index_4_import_tmp(ntot_new_import)
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: irank_origin_new_import(ntot_new_import)
!!        integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
!!        integer(kind= kint), intent(in)                               &
!!     &         :: istack_trimmed_import_pe(0:nprocs)
!!        integer(kind= kint), intent(in)                               &
!!     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!!        integer(kind = kint), intent(inout)                           &
!!     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
!!        integer(kind = kint), intent(inout) :: num_miss
!!      subroutine find_home_import_item_by_trim                        &
!!     &         (nprocs, ntot_new_import, index_4_import_tmp,          &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item, idx_home_sorted_import,   &
!!     &          idx_home_for_import, num_miss)
!!        integer, intent(in) :: nprocs
!!        integer(kind = kint), intent(in) :: ntot_new_import
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: index_4_import_tmp(ntot_new_import)
!!        integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
!!        integer(kind= kint), intent(in)                               &
!!     &         :: istack_trimmed_import_pe(0:nprocs)
!!        integer(kind= kint), intent(in)                               &
!!     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!!        integer(kind = kint), intent(in)                              &
!!     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
!!        integer(kind = kint), intent(inout)                           &
!!     &              :: idx_home_for_import(ntot_new_import)
!!        integer(kind = kint), intent(inout) :: num_miss
!!@endverbatim
      module trim_redundant_import_item
!
      use m_precision
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
     &                   (nprocs, ntot_new_import, inod_lc_import_tmp,  &
     &                    num_import_tmp, istack_import_tmp)
!
      integer, intent(in) :: nprocs
      integer(kind= kint), intent(in) :: ntot_new_import
      integer(kind= kint), intent(in)                                &
     &                    :: inod_lc_import_tmp(ntot_new_import)
!
      integer(kind= kint), intent(in) :: num_import_tmp(nprocs)
      integer(kind= kint), intent(in) :: istack_import_tmp(0:nprocs)
!
      integer(kind = kint) :: ist, inum, ip, ntot
!
!
      ntot = 0
      do ip = 1, nprocs
        ist = istack_import_tmp(ip-1)
        do inum = 1, num_import_tmp(ip) - 1
          if(inod_lc_import_tmp(ist+inum)                               &
     &        .ne. inod_lc_import_tmp(ist+inum+1)) ntot = ntot + 1
        end do
        if(num_import_tmp(ip) .gt. 0) ntot = ntot + 1
      end do
      count_ntot_trimmed_import = ntot
!
      end function count_ntot_trimmed_import
!
!  ---------------------------------------------------------------------
!
      subroutine count_trimmed_import_stack                             &
     &         (nprocs, ntot_new_import, inod_lc_import_tmp,            &
     &          num_import_tmp, istack_import_tmp,                      &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item)
!
      integer, intent(in) :: nprocs
      integer(kind= kint), intent(in) :: ntot_new_import
!
      integer(kind= kint), intent(in)                                   &
     &                    :: inod_lc_import_tmp(ntot_new_import)
!
      integer(kind= kint), intent(in) :: num_import_tmp(nprocs)
      integer(kind= kint), intent(in) :: istack_import_tmp(0:nprocs)
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
        ist = istack_import_tmp(ip-1)
        do inum = 1, num_import_tmp(ip)-1
          if(inod_lc_import_tmp(ist+inum)                               &
     &        .ne. inod_lc_import_tmp(ist+inum+1)) then
            icou = icou + 1
            istack_trimmed_import_item(icou) = ist + inum
          end if
        end do
        if(num_import_tmp(ip) .gt. 0) then
          icou = icou + 1
          istack_trimmed_import_item(icou) = ist + num_import_tmp(ip)
        end if
        istack_trimmed_import_pe(ip) =    icou
      end do
!
      end subroutine count_trimmed_import_stack
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine trim_internal_import_items                             &
     &         (nprocs, ntot_new_import, irank_nod_new_import,          &
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
      integer(kind = kint), intent(in)                                  &
     &                    :: index_4_import_tmp(ntot_new_import)
      integer(kind = kint), intent(in)                                  &
     &                    :: irank_origin_new_import(ntot_new_import)
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
      integer(kind = kint) :: ist, ied, inum, irank, ip
      integer(kind = kint) :: jst, jed, jnum, jrank, jsort
!
!
!$omp parallel workshare
      idx_home_sorted_import(1:ntot_trimmed_nod_import) = 0
!$omp end parallel workshare
!
      do ip = 1, nprocs
        ist = istack_trimmed_import_pe(ip-1) + 1
        ied = istack_trimmed_import_pe(ip)
        do inum = ist, ied
          jst = istack_trimmed_import_item(inum-1) + 1
          jed = istack_trimmed_import_item(inum)
          do jnum = jst, jed
            jsort = index_4_import_tmp(jnum)
            jrank = irank_origin_new_import(jsort)
            if(     irank_nod_new_import(jsort) .eq. jrank              &
     &        .and. irank_nod_new_import(jsort) .eq. ip-1) then
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
      end subroutine trim_internal_import_items
!
!  ---------------------------------------------------------------------
!
      subroutine trim_external_import_items(nprocs, ntot_new_import,    &
     &          irank_nod_new_import, index_4_import_tmp,               &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item, idx_home_sorted_import,     &
     &          num_miss)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_new_import
      integer(kind = kint), intent(in)                                  &
     &                    :: irank_nod_new_import(ntot_new_import)
!
      integer(kind = kint), intent(in)                                  &
     &                    :: index_4_import_tmp(ntot_new_import)
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
          if(idx_home_sorted_import(inum) .gt. 0) cycle
!
          jst = istack_trimmed_import_item(inum-1) + 1
          jed = istack_trimmed_import_item(inum)
          do jnum = jst, jed
            jsort =   index_4_import_tmp(jnum)
            if(irank_nod_new_import(jsort) .eq. ip-1) then
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
     &         (nprocs, ntot_new_import, index_4_import_tmp,            &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item, idx_home_sorted_import,     &
     &          num_miss)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_new_import
      integer(kind = kint), intent(in)                                  &
     &                    :: index_4_import_tmp(ntot_new_import)
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
          if(idx_home_sorted_import(inum) .gt. 0) cycle
!
          jst = istack_trimmed_import_item(inum-1) + 1
          jed = istack_trimmed_import_item(inum)
          jsort = index_4_import_tmp(jst)
          idx_home_sorted_import(inum) = jsort
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
!  ---------------------------------------------------------------------
!
      subroutine find_home_import_item_by_trim                          &
     &         (nprocs, ntot_new_import, index_4_import_tmp,            &
     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,      &
     &          istack_trimmed_import_item, idx_home_sorted_import,     &
     &          idx_home_for_import, num_miss)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_new_import
      integer(kind = kint), intent(in)                                  &
     &                    :: index_4_import_tmp(ntot_new_import)
!
      integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_pe(0:nprocs)
      integer(kind= kint), intent(in)                                   &
     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
      integer(kind = kint), intent(in)                                  &
     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_home_for_import(ntot_new_import)
      integer(kind = kint), intent(inout) :: num_miss
!
      integer(kind = kint) :: ist, ied, inum, ip
      integer(kind = kint) :: jst, jed, jnum, jrank, jsort, jnod
!
!
!$omp parallel workshare
      idx_home_for_import(1:ntot_new_import) = -1
!$omp end parallel workshare
!
      do ip = 1, nprocs
        ist = istack_trimmed_import_pe(ip-1) + 1
        ied = istack_trimmed_import_pe(ip)
        do inum = ist, ied
          jst = istack_trimmed_import_item(inum-1) + 1
          jed = istack_trimmed_import_item(inum)
!
          do jnum = jst, jed
            jnod = index_4_import_tmp(jnum)
            idx_home_for_import(jnod) = idx_home_sorted_import(inum)
          end do
        end do
      end do
!
      num_miss = 0
      do inum = 1, ntot_new_import
        if(idx_home_for_import(inum) .lt. 0) num_miss = num_miss + 1
      end do
!
      end subroutine find_home_import_item_by_trim
!
!  ---------------------------------------------------------------------
!
      end module trim_redundant_import_item
