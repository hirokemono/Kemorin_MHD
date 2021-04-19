!> @file  t_sort_data_for_sleeve_trim.f90
!!      module t_sort_data_for_sleeve_trim
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine alloc_sort_data_sleeve_ext(ntot_comm, sorted_import)
!!      subroutine dealloc_sort_data_sleeve_ext(sorted_import)
!!        integer(kind = kint), intent(in) :: ntot_comm,
!!        type(sort_data_for_sleeve_trim), intent(inout) :: sorted_import
!!
!!      subroutine sort_import_by_pe_and_local_id                       &
!!     &         (nprocs, nod_comm, expand_comm, irank_comm_new_import, &
!!     &          sort_import)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: expand_comm
!!        integer, intent(in) :: nprocs
!!        integer(kind= kint), intent(in)                               &
!!     &            :: irank_comm_new_import(expand_comm%ntot_import)
!!        type(sort_data_for_sleeve_trim), intent(inout) :: sort_import
!!@endverbatim
!
      module t_sort_data_for_sleeve_trim
!
      use m_precision
!
!>
      type sort_data_for_sleeve_trim
        integer(kind = kint), allocatable :: num_sorted_by_pe(:)
        integer(kind = kint), allocatable :: istack_sorted_by_pe(:)
!>        Numper of node
        integer(kind = kint) :: nitem_sort
        integer(kind = kint), allocatable :: iref_lc_import(:)
        integer(kind = kint), allocatable :: irank_import_sort(:)
        integer(kind = kint), allocatable :: isorted_to_org(:)
        integer(kind = kint), allocatable :: irank_orgin_pe(:)
      end type sort_data_for_sleeve_trim
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sort_data_sleeve_ext                             &
     &         (nprocs, ntot_comm, sorted_import)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_comm
      type(sort_data_for_sleeve_trim), intent(inout) :: sorted_import
!
!
      sorted_import%nitem_sort =  ntot_comm
!
      allocate(sorted_import%num_sorted_by_pe(nprocs))
      allocate(sorted_import%istack_sorted_by_pe(0:nprocs))
!
      sorted_import%num_sorted_by_pe(1:nprocs) =    0
      sorted_import%istack_sorted_by_pe(0:nprocs) = 0
!
      allocate(sorted_import%iref_lc_import(sorted_import%nitem_sort))
      allocate(sorted_import%irank_import_sort(sorted_import%nitem_sort))
      allocate(sorted_import%isorted_to_org(sorted_import%nitem_sort))
      allocate(sorted_import%irank_orgin_pe(sorted_import%nitem_sort))
!
      if(sorted_import%nitem_sort .le. 0) return
!
!$omp parallel workshare
      sorted_import%iref_lc_import(1:sorted_import%nitem_sort) =     0
      sorted_import%irank_import_sort(1:sorted_import%nitem_sort) = -1
      sorted_import%isorted_to_org(1:sorted_import%nitem_sort) =     0
      sorted_import%irank_orgin_pe(1:sorted_import%nitem_sort) =    -1
!$omp end parallel workshare
!
      end subroutine alloc_sort_data_sleeve_ext
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sort_data_sleeve_ext(sorted_import)
!
      type(sort_data_for_sleeve_trim), intent(inout) :: sorted_import
!
!
      if(allocated(sorted_import%isorted_to_org) .eqv. .FALSE.) return
      deallocate(sorted_import%iref_lc_import)
      deallocate(sorted_import%irank_import_sort)
      deallocate(sorted_import%isorted_to_org)
      deallocate(sorted_import%irank_orgin_pe)
!
      deallocate(sorted_import%num_sorted_by_pe)
      deallocate(sorted_import%istack_sorted_by_pe)
!
      end subroutine dealloc_sort_data_sleeve_ext
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sort_import_by_pe_and_local_id                         &
     &         (nprocs, nod_comm, expand_comm, irank_comm_new_import,   &
     &          sort_import)
!
      use t_comm_table
      use quicksort
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_comm
      integer, intent(in) :: nprocs
      integer(kind= kint), intent(in)                                   &
     &            :: irank_comm_new_import(expand_comm%ntot_import)
!
      type(sort_data_for_sleeve_trim), intent(inout) :: sort_import
!
      integer(kind = kint) :: i, irank, ist, ied, icou, ip
!
!
!$omp parallel do private(i)
      do i = 1, expand_comm%ntot_import
        sort_import%isorted_to_org(i) = i
        sort_import%irank_import_sort(i) = irank_comm_new_import(i)
        sort_import%irank_orgin_pe(i) = -1
      end do
!$omp end parallel do
!
!$omp parallel private(i,irank,ist,ied)
      do i = 1, nod_comm%num_neib
        irank = nod_comm%id_neib(i)
        ist = expand_comm%istack_import(i-1) + 1
        ied = expand_comm%istack_import(i)
!$omp workshare
        sort_import%irank_orgin_pe(ist:ied) = irank
!$omp end workshare nowait
      end do
!$omp end parallel
!
      if(expand_comm%ntot_import .gt. 1) then
        call quicksort_w_index                                          &
     &     (expand_comm%ntot_import, sort_import%irank_import_sort,     &
     &      ione, expand_comm%ntot_import, sort_import%isorted_to_org)
      end if
!
!$omp parallel do private(i,icou)
      do i = 1, expand_comm%ntot_import
        icou = sort_import%isorted_to_org(i)
        sort_import%iref_lc_import(i) = expand_comm%item_import(icou)
      end do
!$omp end parallel do
!
!$omp parallel workshare
      sort_import%num_sorted_by_pe(1:nprocs) = 0
!$omp end parallel workshare
      do i = 1, expand_comm%ntot_import
        irank = sort_import%irank_import_sort(i)
        sort_import%num_sorted_by_pe(irank+1)                           &
     &        = sort_import%num_sorted_by_pe(irank+1) + 1
      end do
      do ip = 1, nprocs
        sort_import%istack_sorted_by_pe(ip)                             &
     &      = sort_import%istack_sorted_by_pe(ip-1)                     &
     &       + sort_import%num_sorted_by_pe(ip)
      end do
!
      do ip = 1, nprocs
        ist = sort_import%istack_sorted_by_pe(ip-1)
        if(sort_import%num_sorted_by_pe(ip) .gt. 1) then
          call quicksort_w_index                                        &
     &       (sort_import%num_sorted_by_pe(ip),                         &
     &        sort_import%iref_lc_import(ist+1),                        &
     &        ione, sort_import%num_sorted_by_pe(ip),                   &
     &        sort_import%isorted_to_org(ist+1))
        end if
      end do
!
      end subroutine sort_import_by_pe_and_local_id
!
!  ---------------------------------------------------------------------
!
      subroutine sort_mix_import_by_pe_inod_lc                          &
     &         (inod_dbl, nod_comm, expand_comm,                        &
     &          irank_comm_new_import, sort_import)
!
      use calypso_mpi
      use t_comm_table
      use t_repart_double_numberings
      use quicksort
!
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_comm
      integer(kind= kint), intent(in)                                   &
     &            :: irank_comm_new_import(expand_comm%ntot_import)
!
      type(sort_data_for_sleeve_trim), intent(inout) :: sort_import
!
      integer(kind = kint) :: i, irank, ist, ied, inum, icou, ip
      integer(kind = kint) :: jst, inod
!
!
!$omp parallel private(i,irank,ist,ied)
      do i = 1, nod_comm%num_neib
        irank = nod_comm%id_neib(i)
        ist = nod_comm%istack_import(i-1) + 1
        ied = nod_comm%istack_import(i)
!$omp do private(inum)
        do inum = ist, ied
          sort_import%isorted_to_org(inum) = -inum
          sort_import%irank_orgin_pe(inum) = irank
          sort_import%irank_import_sort(inum) = irank
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      jst = nod_comm%ntot_import
!$omp parallel private(i,irank,ist,ied)
      do i = 1, nod_comm%num_neib
        irank = nod_comm%id_neib(i)
        ist = expand_comm%istack_import(i-1) + 1
        ied = expand_comm%istack_import(i)
!$omp do private(inum)
        do inum = ist, ied
          sort_import%isorted_to_org(inum+jst) = inum
          sort_import%irank_orgin_pe(inum+jst) = irank
          sort_import%irank_import_sort(inum+jst)                       &
     &                                = irank_comm_new_import(inum)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      if(expand_comm%ntot_import .gt. 1) then
        call quicksort_w_index                                          &
     &     (sort_import%nitem_sort, sort_import%irank_import_sort,      &
     &      ione, sort_import%nitem_sort, sort_import%isorted_to_org)
      end if
!
!$omp parallel do private(i,icou,inod)
      do i = 1, sort_import%nitem_sort
        icou = sort_import%isorted_to_org(i)
        if(icou .lt. 0) then
          inod = nod_comm%item_import(-icou)
          sort_import%iref_lc_import(i) = inod_dbl%index(inod)
        else
          sort_import%iref_lc_import(i) = expand_comm%item_import(icou)
        end if
      end do
!$omp end parallel do
!
!$omp parallel workshare
      sort_import%num_sorted_by_pe(1:nprocs) = 0
!$omp end parallel workshare
      do i = 1, sort_import%nitem_sort
        irank = sort_import%irank_import_sort(i)
        sort_import%num_sorted_by_pe(irank+1)                           &
     &        = sort_import%num_sorted_by_pe(irank+1) + 1
      end do
      sort_import%istack_sorted_by_pe(0) = 0
      do ip = 1, nprocs
        sort_import%istack_sorted_by_pe(ip)                             &
     &      = sort_import%istack_sorted_by_pe(ip-1)                     &
     &       + sort_import%num_sorted_by_pe(ip)
      end do
!
      do ip = 1, nprocs
        ist = sort_import%istack_sorted_by_pe(ip-1)
        if(sort_import%num_sorted_by_pe(ip) .gt. 1) then
          call quicksort_w_index                                        &
     &       (sort_import%num_sorted_by_pe(ip),                         &
     &        sort_import%iref_lc_import(ist+1),                        &
     &        ione, sort_import%num_sorted_by_pe(ip),                   &
     &        sort_import%isorted_to_org(ist+1))
        end if
      end do
!
      end subroutine sort_mix_import_by_pe_inod_lc
!
!  ---------------------------------------------------------------------
!
      end module t_sort_data_for_sleeve_trim
