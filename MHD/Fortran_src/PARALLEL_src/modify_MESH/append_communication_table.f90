!>@file   append_communication_table.f90
!!@brief  module append_communication_table
!!
!!@author H. Matsui
!!@date Programmed in March, 2021, 2007
!
!>@brief  Append and construct communicatino table
!!
!!@verbatim
!!      subroutine s_append_communication_table                         &
!!     &         (org_comm, add_comm, new_comm)
!!        type(communication_table), intent(in) :: org_comm
!!        type(calypso_comm_table), intent(in) :: add_comm
!!        type(communication_table), intent(inout) :: new_comm
!!@endverbatim
!
      module append_communication_table
!
      use m_precision
      use t_comm_table
      use t_calypso_comm_table
!
      implicit none
!
      private :: set_pe_flags_to_merge_comm, count_added_neib_processes
      private :: merge_neib_processes, count_merged_comm_table
      private :: append_merged_import_item, append_merged_export_item
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_append_communication_table                           &
     &         (org_comm, add_comm, new_comm)
!
      use calypso_mpi
      use cal_minmax_and_stacks
!
      type(communication_table), intent(in) :: org_comm
      type(calypso_comm_table), intent(in) :: add_comm
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint), allocatable :: ineib_org(:)
      integer(kind = kint), allocatable :: ineib_add_import(:)
      integer(kind = kint), allocatable :: ineib_add_export(:)
!
!
      allocate(ineib_org(nprocs))
      allocate(ineib_add_import(nprocs))
      allocate(ineib_add_export(nprocs))
!
      call set_pe_flags_to_merge_comm(nprocs, org_comm, add_comm,       &
     &    ineib_org, ineib_add_import, ineib_add_export)
!
      call count_added_neib_processes(nprocs, ineib_org,                &
     &    ineib_add_import, ineib_add_export, new_comm%num_neib)
      call alloc_comm_table_num(new_comm)
      call merge_neib_processes(my_rank, nprocs, ineib_org,             &
     &                          ineib_add_import, ineib_add_export,     &
     &                          new_comm%num_neib, new_comm%id_neib)
!
      call alloc_import_num(new_comm)
      call count_merged_comm_table(nprocs, ineib_org, ineib_add_import, &
     &    org_comm%num_neib, org_comm%istack_import,                    &
     &    add_comm%nrank_import, add_comm%istack_import,                &
     &    new_comm%num_neib, new_comm%id_neib, new_comm%num_import)
      call s_cal_total_and_stacks                                       &
     &   (new_comm%num_neib, new_comm%num_import, izero,                &
     &    new_comm%istack_import, new_comm%ntot_import)
!
      call alloc_export_num(new_comm)
      call count_merged_comm_table(nprocs, ineib_org, ineib_add_export, &
     &    org_comm%num_neib, org_comm%istack_export,                    &
     &    add_comm%nrank_export, add_comm%istack_export,                &
     &    new_comm%num_neib, new_comm%id_neib, new_comm%num_export)
      call s_cal_total_and_stacks                                       &
     &   (new_comm%num_neib, new_comm%num_export, izero,                &
     &    new_comm%istack_export, new_comm%ntot_export)
!
!
      call alloc_import_item(new_comm)
      call append_merged_import_item                                    &
     &   (org_comm, add_comm, nprocs, ineib_org, ineib_add_import,      &
     &    new_comm%num_neib, new_comm%id_neib, new_comm%istack_import,  &
     &    new_comm%ntot_import, new_comm%item_import)
!
      call alloc_export_item(new_comm)
      call append_merged_export_item                                    &
     &   (org_comm, add_comm, nprocs, ineib_org, ineib_add_export,      &
     &    new_comm%num_neib, new_comm%id_neib, new_comm%istack_export,  &
     &    new_comm%ntot_export, new_comm%item_export)
!
      deallocate(ineib_org, ineib_add_import, ineib_add_export)
!
      end subroutine s_append_communication_table
!
! ----------------------------------------------------------------------
!
      subroutine append_communication_table_org                         &
     &         (org_comm, add_comm, new_comm)
!
      use calypso_mpi
      use cal_minmax_and_stacks
!
      type(communication_table), intent(in) :: org_comm, add_comm
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint), allocatable :: ineib_org(:)
      integer(kind = kint), allocatable :: ineib_add(:)
!
!
      allocate(ineib_org(nprocs))
      allocate(ineib_add(nprocs))
!
      call set_pe_flags_to_merge_comm_org                               &
     &   (nprocs, org_comm, add_comm, ineib_org, ineib_add)
!
      call count_added_neib_processes_org(nprocs, ineib_org, ineib_add, &
     &                                new_comm%num_neib)
      call alloc_comm_table_num(new_comm)
      call merge_neib_processes_org(my_rank, nprocs, ineib_org, ineib_add,  &
     &                          new_comm%num_neib, new_comm%id_neib)
!
      call alloc_import_num(new_comm)
      call alloc_export_num(new_comm)
      call count_merged_comm_table_org                                      &
     &   (org_comm, add_comm, nprocs, ineib_org, ineib_add,             &
     &    new_comm%num_neib, new_comm%id_neib,                          &
     &    new_comm%num_import, new_comm%num_export)
      call s_cal_total_and_stacks                                       &
     &   (new_comm%num_neib, new_comm%num_import, izero,                &
     &    new_comm%istack_import, new_comm%ntot_import)
      call s_cal_total_and_stacks                                       &
     &   (new_comm%num_neib, new_comm%num_export, izero,                &
     &    new_comm%istack_export, new_comm%ntot_export)
!
      call alloc_import_item(new_comm)
      call append_merged_import_item_org                                &
     &   (org_comm, add_comm, nprocs, ineib_org, ineib_add,             &
     &    new_comm%num_neib, new_comm%id_neib, new_comm%istack_import,  &
     &    new_comm%ntot_import, new_comm%item_import)
!
      call alloc_export_item(new_comm)
      call append_merged_export_item_org                                &
     &   (org_comm, add_comm, nprocs, ineib_org, ineib_add,             &
     &    new_comm%num_neib, new_comm%id_neib, new_comm%istack_export,  &
     &    new_comm%ntot_export, new_comm%item_export)
!
      deallocate(ineib_org, ineib_add)
!
      end subroutine append_communication_table_org
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_pe_flags_to_merge_comm_org                         &
     &         (nprocs, org_comm, add_comm, ineib_org, ineib_add)
!
      integer, intent(in) :: nprocs
      type(communication_table), intent(in) :: org_comm, add_comm
      integer(kind = kint), intent(inout) :: ineib_org(nprocs)
      integer(kind = kint), intent(inout) :: ineib_add(nprocs)
!
      integer :: i, irank
!
!$omp parallel workshare
      ineib_org(1:nprocs) = 0
      ineib_add(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(i,irank)
      do i = 1, org_comm%num_neib
        irank = org_comm%id_neib(i)
        ineib_org(irank+1) = i
      end do
!$omp end parallel do
!
!$omp parallel do private(i,irank)
      do i = 1, add_comm%num_neib
        irank = add_comm%id_neib(i)
        ineib_add(irank+1) = i
      end do
!$omp end parallel do
!
      end subroutine set_pe_flags_to_merge_comm_org
!
! ----------------------------------------------------------------------
!
      subroutine set_pe_flags_to_merge_comm(nprocs, org_comm, add_comm, &
     &          ineib_org, ineib_add_import, ineib_add_export)
!
      integer, intent(in) :: nprocs
      type(communication_table), intent(in) :: org_comm
      type(calypso_comm_table), intent(in) :: add_comm
      integer(kind = kint), intent(inout) :: ineib_org(nprocs)
      integer(kind = kint), intent(inout) :: ineib_add_import(nprocs)
      integer(kind = kint), intent(inout) :: ineib_add_export(nprocs)
!
      integer :: i, irank
!
!$omp parallel workshare
      ineib_org(1:nprocs) = 0
      ineib_add_import(1:nprocs) = 0
      ineib_add_export(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(i,irank)
      do i = 1, org_comm%num_neib
        irank = org_comm%id_neib(i)
        ineib_org(irank+1) = i
      end do
!$omp end parallel do
!
!$omp parallel do private(i,irank)
      do i = 1, add_comm%nrank_import
        irank = add_comm%irank_import(i)
        ineib_add_import(irank+1) = i
      end do
!$omp end parallel do
!
!$omp parallel do private(i,irank)
      do i = 1, add_comm%nrank_export
        irank = add_comm%irank_export(i)
        ineib_add_export(irank+1) = i
      end do
!$omp end parallel do
!
      end subroutine set_pe_flags_to_merge_comm
!
! ----------------------------------------------------------------------
!
      subroutine count_added_neib_processes_org                         &
     &         (nprocs, ineib_org, ineib_add, num_new_neib)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(inout) :: num_new_neib
!
      integer :: icou, irank
!
      icou = 0
      do irank = 0, nprocs-1
        if(ineib_org(irank+1).gt.0 .or. ineib_add(irank+1).gt.0) then
          icou = icou + 1
        end if
      end do
      num_new_neib = icou
!
      end subroutine count_added_neib_processes_org
!
! ----------------------------------------------------------------------
!
      subroutine count_added_neib_processes(nprocs, ineib_org,          &
     &           ineib_add_import, ineib_add_export, num_new_neib)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add_import(nprocs)
      integer(kind = kint), intent(in) :: ineib_add_export(nprocs)
      integer(kind = kint), intent(inout) :: num_new_neib
!
      integer :: icou, irank, iflag
!
      icou = 0
      do irank = 0, nprocs-1
        iflag = ineib_org(irank+1)                                      &
     &         + ineib_add_import(irank+1) + ineib_add_export(irank+1)
        if(iflag .gt. 0) icou = icou + 1
      end do
      num_new_neib = icou
!
      end subroutine count_added_neib_processes
!
! ----------------------------------------------------------------------
!
      subroutine merge_neib_processes_org(my_rank, nprocs,              &
     &          ineib_org, ineib_add, num_new_neib, id_new_neib)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib
!
      integer(kind = kint), intent(inout) :: id_new_neib(num_new_neib)
!
      integer :: icou, i, irank
!
      icou = 0
      do i = 1, nprocs
        irank = mod(my_rank+i,nprocs)
        if(ineib_org(irank+1).gt.0 .or. ineib_add(irank+1).gt.0) then
          icou = icou + 1
          id_new_neib(icou) = int(irank,KIND(num_new_neib))
        end if
      end do
!
      end subroutine merge_neib_processes_org
!
! ----------------------------------------------------------------------
!
      subroutine merge_neib_processes(my_rank, nprocs,                  &
     &          ineib_org, ineib_add_import, ineib_add_export,          &
     &          num_new_neib, id_new_neib)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add_import(nprocs)
      integer(kind = kint), intent(in) :: ineib_add_export(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib
!
      integer(kind = kint), intent(inout) :: id_new_neib(num_new_neib)
!
      integer :: icou, i, irank, iflag
!
      icou = 0
      do i = 1, nprocs
        irank = mod(my_rank+i,nprocs)
        iflag = ineib_org(irank+1)                                      &
     &         + ineib_add_import(irank+1) + ineib_add_export(irank+1)
        if(iflag .gt. 0) then
          icou = icou + 1
          id_new_neib(icou) = int(irank,KIND(num_new_neib))
        end if
      end do
!
      end subroutine merge_neib_processes
!
! ----------------------------------------------------------------------
!
      subroutine count_merged_comm_table_org(org_comm, add_comm,            &
     &          nprocs, ineib_org, ineib_add, num_new_neib,             &
     &          id_new_neib, num_new_import, num_new_export)
!
      type(communication_table), intent(in) :: org_comm, add_comm
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
!
      integer(kind = kint), intent(inout)                               &
     &                      :: num_new_import(num_new_neib)
      integer(kind = kint), intent(inout)                               &
     &                      :: num_new_export(num_new_neib)
!
      integer(kind = kint) :: i, irank, ineib, jneib
!
!
!$omp parallel do private(i,irank,ineib,jneib)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)
        num_new_import(i) = 0
        num_new_export(i) = 0
        if(ineib .gt. 0) then
          num_new_import(i) = num_new_import(i)                         &
     &                       + org_comm%istack_import(ineib)            &
     &                        - org_comm%istack_import(ineib-1)
          num_new_export(i) = num_new_export(i)                         &
     &                       + org_comm%istack_export(ineib)            &
     &                        - org_comm%istack_export(ineib-1)
        end if
        if(jneib .gt. 0) then
          num_new_import(i) = num_new_import(i)                         &
     &                       + add_comm%istack_import(jneib)            &
     &                        - add_comm%istack_import(jneib-1)
          num_new_export(i) = num_new_export(i)                         &
     &                       + add_comm%istack_export(jneib)            &
     &                        - add_comm%istack_export(jneib-1)
        end if
      end do
!$omp end parallel do
!
      end subroutine count_merged_comm_table_org
!
! ----------------------------------------------------------------------
!
      subroutine count_merged_comm_table(nprocs, ineib_org, ineib_add,  &
     &          num_neib_org, istack_comm_org,                          &
     &          nrank_neib_add, istack_comm_add,                        &
     &          num_new_neib, id_new_neib, num_new_import)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
!
      integer(kind = kint), intent(in) :: num_neib_org
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_comm_org(0:num_neib_org)
      integer(kind = kint), intent(in) :: nrank_neib_add
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_comm_add(0:nrank_neib_add)
!
      integer(kind = kint), intent(in) :: num_new_neib
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
!
      integer(kind = kint), intent(inout)                               &
     &                      :: num_new_import(num_new_neib)
!
      integer(kind = kint) :: i, irank, ineib, jneib
!
!
!$omp parallel do private(i,irank,ineib,jneib)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)
        num_new_import(i) = 0
        if(ineib .gt. 0) then
          num_new_import(i) = num_new_import(i)                         &
     &              + istack_comm_org(ineib) - istack_comm_org(ineib-1)
        end if
        if(jneib .gt. 0) then
          num_new_import(i) = num_new_import(i)                         &
     &              + istack_comm_add(jneib) - istack_comm_add(jneib-1)
        end if
      end do
!$omp end parallel do
!
      end subroutine count_merged_comm_table
!
! ----------------------------------------------------------------------
!
      subroutine append_merged_import_item_org                          &
     &         (org_comm, add_comm, nprocs, ineib_org, ineib_add,       &
     &          num_new_neib, id_new_neib, istack_new_import,           &
     &          ntot_new_import, item_new_import)
!
      type(communication_table), intent(in) :: org_comm, add_comm
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib, ntot_new_import
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
      integer(kind = kint), intent(in)                                  &
     &                      :: istack_new_import(0:num_new_neib)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_new_import(ntot_new_import)
!
      integer :: i, irank, ineib, jneib, jst, ist, num, inum
!
!
!$omp parallel do private(i,irank,ineib,jneib,jst,ist,num,inum)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)

        jst = istack_new_import(i-1)
        if(ineib .gt. 0) then
          ist = org_comm%istack_import(ineib-1)
          num = org_comm%istack_import(ineib) - ist
          do inum = 1, num
            item_new_import(inum+jst) = org_comm%item_import(inum+ist)
          end do
          jst = jst + num
        end if
        if(jneib .gt. 0) then
          ist = add_comm%istack_import(jneib-1)
          num = add_comm%istack_import(jneib) - ist
          do inum = 1, num
            item_new_import(inum+jst) = add_comm%item_import(inum+ist)
          end do
        end if
      end do
!$omend p parallel do
!
      end subroutine append_merged_import_item_org
!
! ----------------------------------------------------------------------
!
      subroutine append_merged_import_item                              &
     &         (org_comm, add_comm, nprocs, ineib_org, ineib_add,       &
     &          num_new_neib, id_new_neib, istack_new_import,           &
     &          ntot_new_import, item_new_import)
!
      type(communication_table), intent(in) :: org_comm
      type(calypso_comm_table), intent(in) ::  add_comm
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib, ntot_new_import
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
      integer(kind = kint), intent(in)                                  &
     &                      :: istack_new_import(0:num_new_neib)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_new_import(ntot_new_import)
!
      integer :: i, irank, ineib, jneib, jst, ist, num, inum
!
!
!$omp parallel do private(i,irank,ineib,jneib,jst,ist,num,inum)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)

        jst = istack_new_import(i-1)
        if(ineib .gt. 0) then
          ist = org_comm%istack_import(ineib-1)
          num = org_comm%istack_import(ineib) - ist
          do inum = 1, num
            item_new_import(inum+jst) = org_comm%item_import(inum+ist)
          end do
          jst = jst + num
        end if
        if(jneib .gt. 0) then
          ist = add_comm%istack_import(jneib-1)
          num = add_comm%istack_import(jneib) - ist
          do inum = 1, num
            item_new_import(inum+jst) = add_comm%item_import(inum+ist)
          end do
        end if
      end do
!$omend p parallel do
!
      end subroutine append_merged_import_item
!
! ----------------------------------------------------------------------
!
      subroutine append_merged_export_item_org                          &
     &         (org_comm, add_comm, nprocs, ineib_org, ineib_add,       &
     &          num_new_neib, id_new_neib, istack_new_export,           &
     &          ntot_new_export, item_new_export)
!
      type(communication_table), intent(in) :: org_comm, add_comm
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib, ntot_new_export
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
      integer(kind = kint), intent(in)                                  &
     &                      :: istack_new_export(0:num_new_neib)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_new_export(ntot_new_export)
!
      integer :: i, irank, ineib, jneib, jst, ist, num, inum
!
!
!$omp parallel do private(i,irank,ineib,jneib,jst,ist,num,inum)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)

        jst = istack_new_export(i-1)
        if(ineib .gt. 0) then
          ist = org_comm%istack_export(ineib-1)
          num = org_comm%istack_export(ineib) - ist
          do inum = 1, num
            item_new_export(inum+jst) = org_comm%item_export(inum+ist)
          end do
          jst = jst + num
        end if
        if(jneib .gt. 0) then
          ist = add_comm%istack_export(jneib-1)
          num = add_comm%istack_export(jneib) - ist
          do inum = 1, num
            item_new_export(inum+jst) = add_comm%item_export(inum+ist)
          end do
        end if
      end do
!$omend p parallel do
!
      end subroutine append_merged_export_item_org
!
! ----------------------------------------------------------------------
!
      subroutine append_merged_export_item                              &
     &         (org_comm, add_comm, nprocs, ineib_org, ineib_add,       &
     &          num_new_neib, id_new_neib, istack_new_export,           &
     &          ntot_new_export, item_new_export)
!
      type(communication_table), intent(in) :: org_comm
      type(calypso_comm_table), intent(in) :: add_comm
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib, ntot_new_export
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
      integer(kind = kint), intent(in)                                  &
     &                      :: istack_new_export(0:num_new_neib)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_new_export(ntot_new_export)
!
      integer :: i, irank, ineib, jneib, jst, ist, num, inum
!
!
!$omp parallel do private(i,irank,ineib,jneib,jst,ist,num,inum)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)

        jst = istack_new_export(i-1)
        if(ineib .gt. 0) then
          ist = org_comm%istack_export(ineib-1)
          num = org_comm%istack_export(ineib) - ist
          do inum = 1, num
            item_new_export(inum+jst) = org_comm%item_export(inum+ist)
          end do
          jst = jst + num
        end if
        if(jneib .gt. 0) then
          ist = add_comm%istack_export(jneib-1)
          num = add_comm%istack_export(jneib) - ist
          do inum = 1, num
            item_new_export(inum+jst) = add_comm%item_export(inum+ist)
          end do
        end if
      end do
!$omend p parallel do
!
      end subroutine append_merged_export_item
!
! ----------------------------------------------------------------------
!
      end module append_communication_table
