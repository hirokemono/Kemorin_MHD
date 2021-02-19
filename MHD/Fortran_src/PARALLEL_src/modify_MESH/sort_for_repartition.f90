!>@file   sort_for_repartition.f90
!!@brief  module sort_for_repartition
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Re-distribute group data
!!
!!@verbatim
!!      subroutine sort_by_domain_and_index_list                        &
!!     &         (nprocs, id_start_rank, ntot, irank_org, id_org,       &
!!     &          irank_sorted, id_sorted, idx_sort, num_each_pe)
!!      subroutine mark_overlapped_import_node                          &
!!     &         (nprocs, my_rank, ntot_import, num_recv_tmp, inod_sort,&
!!     &          num_recv_tmp2, iflag_dup)
!!      subroutine mark_overlapped_import_ele(nprocs, ntot_import,      &
!!     &          nele_recv_tmp, iele_sort, iflag_dup)
!!
!!      subroutine push_off_redundant_element(nprocs, nele_recv_tmp,    &
!!     &          ntot_import, idx_sort, iflag_dup,                     &
!!     &          item_import, irev_import, idx_sort2, new_numele)
!!@endverbatim
!
      module sort_for_repartition
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
      subroutine sort_by_domain_and_index_list                          &
     &         (nprocs, id_start_rank, ntot, irank_org, id_org,         &
     &          irank_sorted, id_sorted, idx_sort, num_each_pe)
!
      use quicksort
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: id_start_rank
      integer(kind = kint), intent(in) :: ntot
      integer(kind = kint), intent(in) :: irank_org(ntot)
      integer(kind = kint), intent(in) :: id_org(ntot)
      integer(kind = kint), intent(inout) :: irank_sorted(ntot)
      integer(kind = kint), intent(inout) :: id_sorted(ntot)
      integer(kind = kint), intent(inout) :: idx_sort(ntot)
      integer(kind = kint), intent(inout) :: num_each_pe(nprocs)
!
      integer(kind = kint), allocatable :: istack_tmp(:)
      integer(kind = kint) :: i, j, ip, ist, num
!
!
!$omp parallel do private(i)
      do i = 1, ntot
        irank_sorted(i) = mod(irank_org(i)+nprocs-id_start_rank,nprocs)
      end do
!$omp end parallel do
!
      if(ntot .gt. 1) then
        call quicksort_w_index(ntot, irank_sorted,                      &
     &                         ione, ntot, idx_sort)
      end if
!
!$omp parallel do private(i,j)
      do i = 1, ntot
        j = idx_sort(i)
        id_sorted(i) = id_org(j)
      end do
!$omp end parallel do
!
!$omp parallel workshare
      num_each_pe(1:nprocs) = 0
!$omp end parallel workshare
      do i = 1, ntot
        ip = mod(irank_sorted(i)+id_start_rank,nprocs)
        num_each_pe(ip+1) = num_each_pe(ip+1) + 1
      end do
!
      allocate(istack_tmp(0:nprocs))
      istack_tmp(0) = 0
      do i = 1, nprocs
        ip = mod(i+id_start_rank-1,nprocs)
        istack_tmp(i) = istack_tmp(i-1) + num_each_pe(ip+1)
      end do
!
!$omp parallel do private(i,ist,num)
      do i = 1, nprocs
        ist = istack_tmp(i-1)
        num = istack_tmp(i  ) - istack_tmp(i-1)
        if(num .gt. 1) then
          call quicksort_w_index(num, id_sorted(ist+1),                 &
     &        ione, num, idx_sort(ist+1))
        end if
      end do
!$omp end parallel do
!
      deallocate(istack_tmp)
!
      end subroutine sort_by_domain_and_index_list
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine mark_overlapped_import_node                            &
     &         (nprocs, my_rank, ntot_import, num_recv_tmp, inod_sort,  &
     &          num_recv_tmp2, iflag_dup)
!
      integer, intent(in) :: nprocs, my_rank
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: num_recv_tmp(nprocs)
      integer(kind = kint), intent(in) :: inod_sort(ntot_import)
!
      integer(kind = kint), intent(inout) :: num_recv_tmp2(nprocs)
      integer(kind = kint), intent(inout) :: iflag_dup(ntot_import)
!
      integer(kind = kint) :: i, ist, icou, ip
!
!
!$omp parallel workshare
      iflag_dup(1:ntot_import) = 1
!$omp end parallel workshare
!
      ist = 0
      do i = 1, nprocs-1
        ip = mod(i+my_rank,nprocs)
        do icou = 2, num_recv_tmp(ip+1)
          if(inod_sort(ist+icou) .eq. inod_sort(ist+icou-1)) then
            iflag_dup(ist+icou) = 0
          end if
        end do
        do icou = 1, num_recv_tmp(ip+1)
          num_recv_tmp2(ip+1) = num_recv_tmp2(ip+1) + iflag_dup(ist+icou)
        end do
        ist = ist + num_recv_tmp(ip+1)
      end do
!
      end subroutine mark_overlapped_import_node
!
! ----------------------------------------------------------------------
!
      subroutine mark_overlapped_import_ele(nprocs, ntot_import,        &
     &          nele_recv_tmp, iele_sort, iflag_dup)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: nele_recv_tmp(nprocs)
      integer(kind = kint), intent(in) :: iele_sort(ntot_import)
!
      integer(kind = kint), intent(inout) :: iflag_dup(ntot_import)
!
      integer(kind = kint) :: icou, ist, ip
!
!
!$omp parallel workshare
      iflag_dup(1:ntot_import) = 1
!$omp end parallel workshare
!
      ist = 0
      do ip = 1, nprocs
        if(nele_recv_tmp(ip) .gt. 1) then
          do icou = 2, nele_recv_tmp(ip)
            if(iele_sort(ist+icou) .eq. iele_sort(ist+icou-1)) then
              iflag_dup(ist+icou) = 0
            end if
          end do
        end if
        ist = ist + nele_recv_tmp(ip)
      end do
!
      end subroutine mark_overlapped_import_ele
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine push_off_redundant_element(nprocs, nele_recv_tmp,      &
     &          ntot_import, idx_sort, iflag_dup,                       &
     &          item_import, irev_import, idx_sort2, new_numele)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: nele_recv_tmp(nprocs)
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: idx_sort(ntot_import)
      integer(kind = kint), intent(in) :: iflag_dup(ntot_import)
!
      integer(kind = kint), intent(inout) :: new_numele
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
      integer(kind = kint), intent(inout) :: irev_import(ntot_import)
      integer(kind = kint), intent(inout) :: idx_sort2(ntot_import)
!
      integer(kind = kint) :: icou, i, ist, ip, iele, j, jele
!
!
      new_numele = sum(iflag_dup)
      ist = 0
      iele = 0
      jele = new_numele
      do ip = 1, nprocs
        if(nele_recv_tmp(ip) .le. 0) cycle
!
        do icou = 1, nele_recv_tmp(ip)
          if(iflag_dup(ist+icou) .eq. 0) then
            jele = jele + 1
            idx_sort2(jele) = idx_sort(ist+icou)
          else if(iflag_dup(ist+icou) .gt. 0) then
            iele = iele + 1
            idx_sort2(iele) = idx_sort(ist+icou)
          end if
        end do
        ist = ist + nele_recv_tmp(ip)
      end do
!
      do i = 1, ntot_import
        j = idx_sort2(i)
        item_import(j) = i
        irev_import(i) = j
      end do
!
      end subroutine push_off_redundant_element
!
! ----------------------------------------------------------------------
!
      end module sort_for_repartition
