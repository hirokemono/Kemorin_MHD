!>@file   append_communication_table.f90
!!@brief  module append_communication_table
!!
!!@author H. Matsui
!!@date Programmed in March, 2021, 2007
!
!>@brief  Append and construct communicatino table
!!
!!@verbatim
!!      subroutine append_nod_communication_table                       &
!!     &         (nod_comm, add_nod_comm, trim_import_xx,               &
!!     &          new_nod_comm, dist_4_comm)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(calypso_comm_table), intent(in) :: add_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in) :: trim_import_xx
!!        type(communication_table), intent(inout) :: new_nod_comm
!!        type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
!!      subroutine append_ele_communication_table                       &
!!     &         (ele_comm, add_ele_comm, new_ele_comm)
!!        type(communication_table), intent(in) :: ele_comm
!!        type(calypso_comm_table), intent(in) :: add_ele_comm
!!        type(communication_table), intent(inout) :: new_ele_comm
!!@endverbatim
!
      module append_communication_table
!
      use m_precision
      use t_comm_table
      use t_calypso_comm_table
      use t_comm_table_for_each_pe
      use t_mesh_for_sleeve_extend
!
      implicit none
!
      private :: append_communication_tbl, append_distance_in_export
      private :: set_pe_flags_to_merge_comm
      private :: count_added_neib_processes, merge_neib_processes
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_nod_communication_table                         &
     &         (nod_comm, add_nod_comm, trim_import_xx,                 &
     &          new_nod_comm, dist_4_comm)
!
      use calypso_mpi
!
      type(communication_table), intent(in) :: nod_comm
      type(calypso_comm_table), intent(in) :: add_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: trim_import_xx
!
      type(communication_table), intent(inout) :: new_nod_comm
      type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
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
      call append_communication_tbl                                     &
     &   (ineib_org, ineib_add_import, ineib_add_export,                &
     &    nod_comm, add_nod_comm, new_nod_comm)
      call append_distance_in_export(ineib_org, ineib_add_export,       &
     &    nod_comm, add_nod_comm, new_nod_comm, trim_import_xx,         &
     &    dist_4_comm)
!
      deallocate(ineib_org, ineib_add_import, ineib_add_export)
!
      end subroutine append_nod_communication_table
!
! ----------------------------------------------------------------------
!
      subroutine append_ele_communication_table                         &
     &         (ele_comm, add_ele_comm, new_ele_comm)
!
      use calypso_mpi
!
      type(communication_table), intent(in) :: ele_comm
      type(calypso_comm_table), intent(in) :: add_ele_comm
      type(communication_table), intent(inout) :: new_ele_comm
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
      call append_communication_tbl                                     &
     &   (ineib_org, ineib_add_import, ineib_add_export,                &
     &    ele_comm, add_ele_comm, new_ele_comm)
!
      deallocate(ineib_org, ineib_add_import, ineib_add_export)
!
      end subroutine append_ele_communication_table
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine append_communication_tbl                               &
     &         (ineib_org, ineib_add_import, ineib_add_export,          &
     &          org_comm, add_comm, new_comm)
!
      use calypso_mpi
      use cal_minmax_and_stacks
      use append_comm_items
!
      type(communication_table), intent(in) :: org_comm
      type(calypso_comm_table), intent(in) :: add_comm
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint), intent(inout) :: ineib_org(nprocs)
      integer(kind = kint), intent(inout) :: ineib_add_import(nprocs)
      integer(kind = kint), intent(inout) :: ineib_add_export(nprocs)
!
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
      end subroutine append_communication_tbl
!
! ----------------------------------------------------------------------
!
      subroutine append_distance_in_export(ineib_org, ineib_add_export, &
     &          nod_comm, add_nod_comm, new_nod_comm, trim_import_xx,   &
     &          dist_4_comm)
!
      use calypso_mpi
      use reverse_SR_real
!
      type(communication_table), intent(in) :: nod_comm
      type(calypso_comm_table), intent(in) :: add_nod_comm
      type(communication_table), intent(in) :: new_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: trim_import_xx
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add_export(nprocs)
!
      type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
!
      type(dist_from_wall_in_export) :: dist_tmp, dist_add
!
!
      call alloc_dist_from_wall_export                                  &
     &   (add_nod_comm%ntot_export, dist_add)
      call real_items_send_recv                                         &
     &   (add_nod_comm%nrank_import, add_nod_comm%irank_import,         &
     &    add_nod_comm%istack_import, trim_import_xx%distance,          &
     &    add_nod_comm%nrank_export, add_nod_comm%irank_export,         &
     &    add_nod_comm%istack_export, add_nod_comm%iflag_self_copy,     &
     &    dist_add%distance_in_export)
!
      call alloc_dist_from_wall_export                                  &
     &   (nod_comm%ntot_export, dist_tmp)
!$omp parallel workshare
      dist_tmp%distance_in_export(1:dist_tmp%ntot)                      &
     &              = dist_4_comm%distance_in_export(1:dist_tmp%ntot)
!$omp end parallel workshare
      call dealloc_dist_from_wall_export(dist_4_comm)
!
      dist_4_comm%ntot = new_nod_comm%ntot_export
      allocate(dist_4_comm%distance_in_export(dist_4_comm%ntot))
      call append_merged_export_distance(nod_comm, add_nod_comm,        &
     &    dist_tmp%distance_in_export, dist_add%distance_in_export,     &
     &    nprocs, ineib_org, ineib_add_export, new_nod_comm%num_neib,   &
     &    new_nod_comm%id_neib, new_nod_comm%istack_export,             &
     &    dist_4_comm%ntot, dist_4_comm%distance_in_export)
      call dealloc_dist_from_wall_export(dist_tmp)
      call dealloc_dist_from_wall_export(dist_add)
!
      end subroutine append_distance_in_export
!
! ----------------------------------------------------------------------
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
      end module append_communication_table
