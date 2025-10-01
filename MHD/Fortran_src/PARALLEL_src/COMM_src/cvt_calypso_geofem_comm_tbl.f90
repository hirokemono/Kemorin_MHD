!> @file  cvt_calypso_geofem_comm_tbl.f90
!!      module cvt_calypso_geofem_comm_tbl
!!
!! @author  H. Matsui
!! @date Programmed in Apr., 2020
!
!>@brief Convert between Calypso sommunication table 
!!@n      and Geofem Communication table
!!
!!@verbatim
!!      subroutine dup_calypso_comm_to_comm_tbl(my_rank, nprocs,        &
!!     &                                        cps_tbl, comm_tbl)
!!        integer, intent(in) :: my_rank, nprocs
!!        type(calypso_comm_table), intent(in) :: cps_tbl
!!        type(communication_table), intent(inout) :: comm_tbl
!!      subroutine dup_comm_tbl_to_calypso_comm(my_rank, nprocs, NP,    &
!!     &                                        comm_tbl, cps_tbl)
!!        integer, intent(in) :: my_rank, nprocs
!!        type(communication_table), intent(in) :: comm_tbl
!!        type(calypso_comm_table), intent(inout) :: cps_tbl
!!@endverbatim
!
      module cvt_calypso_geofem_comm_tbl
!
      use m_precision
      use t_comm_table
      use t_calypso_comm_table
!
      private :: count_comm_table_rank
      private :: count_calypso_comm_rank, set_comm_rank_and_flag
      private :: count_comm_item_by_pe, shuffle_comm_item_by_pe
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine dup_calypso_comm_to_comm_tbl(my_rank, nprocs,          &
     &                                        cps_tbl, comm_tbl)
!
      integer, intent(in) :: my_rank, nprocs
      type(calypso_comm_table), intent(in) :: cps_tbl
      type(communication_table), intent(inout) :: comm_tbl
!
      integer(kind = kint), allocatable :: iflag_pe(:)
!
!
      allocate(iflag_pe(nprocs))
      call count_comm_table_rank                                        &
     &   (nprocs, cps_tbl, comm_tbl%num_neib, iflag_pe)
!
      call alloc_neighbouring_id(comm_tbl)
      call set_comm_rank_and_flag(my_rank, nprocs,                      &
     &    comm_tbl%num_neib, comm_tbl%id_neib, iflag_pe)
!
      call alloc_import_num(comm_tbl)
      call count_comm_item_by_pe                                        &
     &   (nprocs, iflag_pe, cps_tbl%nrank_import,                       &
     &    cps_tbl%irank_import, cps_tbl%istack_import,                  &
     &    comm_tbl%num_neib, comm_tbl%num_import)
      call s_cal_total_and_stacks                                       &
     &   (comm_tbl%num_neib, comm_tbl%num_import, izero,                &
     &    comm_tbl%istack_import, comm_tbl%ntot_import)
      call alloc_import_item(comm_tbl)
!
      call shuffle_comm_item_by_pe(nprocs, iflag_pe,                    &
     &    cps_tbl%nrank_import, cps_tbl%irank_import,                   &
     &    cps_tbl%istack_import, cps_tbl%item_import,                   &
     &    comm_tbl%num_neib, comm_tbl%istack_import,                    &
     &    comm_tbl%item_import)
!
      call alloc_export_num(comm_tbl)
      call count_comm_item_by_pe                                        &
     &   (nprocs, iflag_pe, cps_tbl%nrank_export,                       &
     &    cps_tbl%irank_export, cps_tbl%istack_export,                  &
     &    comm_tbl%num_neib, comm_tbl%num_export)
      call s_cal_total_and_stacks                                       &
     &   (comm_tbl%num_neib, comm_tbl%num_export, izero,                &
     &    comm_tbl%istack_export, comm_tbl%ntot_export)
      call alloc_export_item(comm_tbl)
!
      call shuffle_comm_item_by_pe(nprocs, iflag_pe,                    &
     &    cps_tbl%nrank_export, cps_tbl%irank_export,                   &
     &    cps_tbl%istack_export, cps_tbl%item_export,                   &
     &    comm_tbl%num_neib, comm_tbl%istack_export,                    &
     &    comm_tbl%item_export)
      deallocate(iflag_pe)
!
      end subroutine dup_calypso_comm_to_comm_tbl
!
!------------------------------------------------------------------
!
      subroutine dup_comm_tbl_to_calypso_comm(my_rank, nprocs, NP,      &
     &                                        comm_tbl, cps_tbl)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: NP
      type(communication_table), intent(in) :: comm_tbl
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
      integer(kind = kint), allocatable :: iflag_pe(:)
!
      allocate(iflag_pe(nprocs))
!
      call count_calypso_comm_rank(nprocs, comm_tbl%num_neib,           &
     &    comm_tbl%id_neib, comm_tbl%istack_import,                     &
     &    cps_tbl%nrank_import, iflag_pe)
      call alloc_calypso_import_num(cps_tbl)
!
      cps_tbl%iflag_self_copy = 0
      if(iflag_pe(my_rank+1) .gt. 0) cps_tbl%iflag_self_copy = 1
!
      call set_comm_rank_and_flag(my_rank, nprocs,                      &
     &    cps_tbl%nrank_import, cps_tbl%irank_import, iflag_pe)
      call count_comm_item_by_pe(nprocs, iflag_pe,                      &
     &    comm_tbl%num_neib, comm_tbl%id_neib, comm_tbl%istack_import,  &
     &    cps_tbl%nrank_import, cps_tbl%num_import)
      call s_cal_total_and_stacks                                       &
     &   (cps_tbl%nrank_import, cps_tbl%num_import, izero,              &
     &    cps_tbl%istack_import, cps_tbl%ntot_import)
      call alloc_calypso_import_item(cps_tbl)
      call alloc_calypso_import_rev(NP, cps_tbl)
!
      call shuffle_comm_item_by_pe(nprocs, iflag_pe,                    &
     &    comm_tbl%num_neib, comm_tbl%id_neib,                          &
     &    comm_tbl%istack_import, comm_tbl%item_import,                 &
     &    cps_tbl%nrank_import, cps_tbl%istack_import,                  &
     &    cps_tbl%item_import)
!
!
      call count_calypso_comm_rank(nprocs, comm_tbl%num_neib,           &
     &    comm_tbl%id_neib, comm_tbl%istack_export,                     &
     &    cps_tbl%nrank_export, iflag_pe)
      call alloc_calypso_export_num(cps_tbl)
!
      call set_comm_rank_and_flag(my_rank, nprocs,                      &
     &    cps_tbl%nrank_export, cps_tbl%irank_export, iflag_pe)
      call count_comm_item_by_pe(nprocs, iflag_pe,                      &
     &    comm_tbl%num_neib, comm_tbl%id_neib, comm_tbl%istack_export,  &
     &    cps_tbl%nrank_export, cps_tbl%num_export)
      call s_cal_total_and_stacks                                       &
     &   (cps_tbl%nrank_export, cps_tbl%num_export, izero,              &
     &    cps_tbl%istack_export, cps_tbl%ntot_export)
      call alloc_calypso_export_item(cps_tbl)
!
      call shuffle_comm_item_by_pe(nprocs, iflag_pe,                    &
     &    comm_tbl%num_neib, comm_tbl%id_neib,                          &
     &    comm_tbl%istack_export, comm_tbl%item_export,                 &
     &    cps_tbl%nrank_export, cps_tbl%istack_export,                  &
     &    cps_tbl%item_export)
      deallocate(iflag_pe)
!
      end subroutine dup_comm_tbl_to_calypso_comm
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_calypso_comm_rank                                &
     &         (nprocs, nrank_in, id_neib_in, istack_comm_in,           &
     &          nrank_out, iflag_pe)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: nrank_in
      integer(kind = kint), intent(in) :: id_neib_in(nrank_in)
      integer(kind = kint), intent(in) :: istack_comm_in(0:nrank_in)
!
      integer(kind = kint), intent(inout) :: iflag_pe(nprocs)
      integer(kind = kint), intent(inout) :: nrank_out
!
      integer(kind = kint) :: i, ip, num
!
!
!$omp parallel workshare
      iflag_pe(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(i,ip,num)
      do i = 1, nrank_in
        ip =  id_neib_in(i) + 1
        num = istack_comm_in(i) - istack_comm_in(i-1)
        if(num .gt. 0) iflag_pe(ip) = 1
      end do
!$omp end parallel do
      nrank_out = sum(iflag_pe)
!
      end subroutine count_calypso_comm_rank
!
!------------------------------------------------------------------
!
      subroutine count_comm_table_rank                                &
     &         (nprocs, c_comm, nrank_out, iflag_pe)
!
      integer, intent(in) :: nprocs
      type(calypso_comm_table), intent(in) :: c_comm
!
      integer(kind = kint), intent(inout) :: iflag_pe(nprocs)
      integer(kind = kint), intent(inout) :: nrank_out
!
      integer(kind = kint) :: i, ip
!
!
!$omp parallel workshare
      iflag_pe(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(i,ip)
      do i = 1, c_comm%nrank_import
        ip = c_comm%irank_import(i) + 1
        iflag_pe(ip) = 1
      end do
!$omp end parallel do
!$omp parallel do private(i,ip)
      do i = 1, c_comm%nrank_export
        ip = c_comm%irank_export(i) + 1
        iflag_pe(ip) = 1
      end do
!$omp end parallel do
      nrank_out = sum(iflag_pe)
!
      end subroutine count_comm_table_rank
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_comm_rank_and_flag                                 &
     &         (my_rank, nprocs, nrank_out, id_neib_out, iflag_pe)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: nrank_out
      integer(kind = kint), intent(inout) :: id_neib_out(nrank_out)
      integer(kind = kint), intent(inout) :: iflag_pe(nprocs)
!
      integer(kind = kint) :: icou, ip, i_rank
!
!
      icou = 0
      do ip = 1, nprocs
        i_rank = mod(ip+my_rank,nprocs)
        if(iflag_pe(i_rank+1) .gt. 0) then
          icou = icou + 1
          id_neib_out(icou) = i_rank
          iflag_pe(i_rank+1) = icou
        end if
      end do
!
      end subroutine set_comm_rank_and_flag
!
!------------------------------------------------------------------
!
      subroutine count_comm_item_by_pe(nprocs, iflag_pe,                &
     &          nrank_in, id_neib_in, istack_comm_in,                   &
     &          nrank_out, nitem_comm_out)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: iflag_pe(nprocs)
!
      integer(kind = kint), intent(in) :: nrank_in
      integer(kind = kint), intent(in) :: id_neib_in(nrank_in)
      integer(kind = kint), intent(in) :: istack_comm_in(0:nrank_in)
!
      integer(kind = kint), intent(in) :: nrank_out
      integer(kind = kint), intent(inout) :: nitem_comm_out(nrank_out)
!
      integer(kind = kint) :: i, ip, num, j
!
!
!$omp parallel do private(i,ip,num,j)
      do i = 1, nrank_in
        ip =  id_neib_in(i) + 1
        num = istack_comm_in(i) - istack_comm_in(i-1)
        if(num .gt. 0) then
          j = iflag_pe(ip)
          nitem_comm_out(j) = num
        end if
      end do
!$omp end parallel do
!
      end subroutine count_comm_item_by_pe
!
!------------------------------------------------------------------
!
      subroutine shuffle_comm_item_by_pe(nprocs, iflag_pe,              &
     &          nrank_in, id_neib_in, istack_comm_in, item_comm_in,     &
     &          nrank_out, istack_comm_out, item_comm_out)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: iflag_pe(nprocs)
!
      integer(kind = kint), intent(in) :: nrank_in
      integer(kind = kint), intent(in) :: id_neib_in(nrank_in)
      integer(kind = kint), intent(in) :: istack_comm_in(0:nrank_in)
      integer(kind = kint), intent(in)                                  &
     &              :: item_comm_in(istack_comm_in(nrank_in))
!
      integer(kind = kint), intent(in) :: nrank_out
      integer(kind = kint), intent(in) :: istack_comm_out(0:nrank_out)
!
      integer(kind = kint), intent(inout)                               &
     &              :: item_comm_out(istack_comm_out(nrank_out))
!
      integer(kind = kint) :: i, ip, num, ist, j, jst
!
!
      do i = 1, nrank_in
        ip =  id_neib_in(i) + 1
        num = istack_comm_in(i) - istack_comm_in(i-1)
        if(num .gt. 0) then
          j = iflag_pe(ip)
          jst = istack_comm_out(j-1)
          ist = istack_comm_in(i-1)
!$omp parallel workshare
          item_comm_out(jst+1:jst+num) = item_comm_in(ist+1:ist+num)
!$omp end parallel workshare
        end if
      end do
!
      end subroutine shuffle_comm_item_by_pe
!
!------------------------------------------------------------------
!
      end module cvt_calypso_geofem_comm_tbl
