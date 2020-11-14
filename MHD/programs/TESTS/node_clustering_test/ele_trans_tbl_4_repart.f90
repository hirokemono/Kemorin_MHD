!>@file   ele_trans_tbl_4_repart.f90
!!@brief  module ele_trans_tbl_4_repart
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Construct commnunication table to new partitionning
!!
!!@verbatim
!!      subroutine const_ele_trans_tbl_for_repart(node, ele, part_tbl,  &
!!     &                                          idomain_new, ele_tbl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(calypso_comm_table), intent(inout) :: ele_tbl
!!      subroutine check_element_transfer_tbl(my_rank, ele, ele_tbl)
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!@endverbatim
!
      module ele_trans_tbl_4_repart
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_geometry_data
      use t_calypso_comm_table
!
      implicit none
!
      private :: count_num_send_ele_repart, set_import_ele_for_repart
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_ele_trans_tbl_for_repart(node, ele, part_tbl,    &
     &                                          idomain_new, ele_tbl)
!
      use calypso_mpi_int
      use set_comm_tbl_to_new_part
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: part_tbl
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
!
      type(calypso_comm_table), intent(inout) :: ele_tbl
!
      integer(kind = kint), allocatable :: num_send_ele(:)
      integer(kind = kint), allocatable :: num_recv_ele(:)
!
!
      allocate(num_send_ele(nprocs))
      allocate(num_recv_ele(nprocs))
!$omp parallel workshare
      num_send_ele(1:nprocs) = 0
      num_recv_ele(1:nprocs) = 0
!$omp end parallel workshare
!
      call count_num_send_ele_repart(nprocs, node, ele,                 &
     &    part_tbl, idomain_new, num_send_ele)
!
      call calypso_mpi_alltoall_one_int(num_send_ele, num_recv_ele)
!
!      write(*,*) my_rank, 'num_send_ele',  mesh%ele%internal_ele,      &
!     &  sum(num_send_ele)
!      write(100+my_rank,*) my_rank, 'num_send_ele', num_send_ele
!      write(100+my_rank,*) my_rank, 'num_recv_ele', num_recv_ele
!
      ele_tbl%iflag_self_copy = 0
      call count_num_export_for_repart(my_rank, nprocs, num_send_ele,   &
     &    ele_tbl%iflag_self_copy, ele_tbl%nrank_export)
      call count_num_import_for_repart                                  &
     &   (nprocs, num_recv_ele, ele_tbl%nrank_import)
!
      call alloc_calypso_export_num(ele_tbl)
      call alloc_calypso_import_num(ele_tbl)
!
      call set_istack_export_for_repart(my_rank, nprocs, num_send_ele,  &
     &    ele_tbl%nrank_export, ele_tbl%ntot_export,                    &
     &    ele_tbl%irank_export, ele_tbl%num_export,                     &
     &    ele_tbl%istack_export)
      call set_istack_import_for_repart(my_rank, nprocs, num_recv_ele,  &
     &    ele_tbl%nrank_import, ele_tbl%ntot_import,                    &
     &    ele_tbl%irank_import, ele_tbl%num_import,                     &
     &    ele_tbl%istack_import)
!
      call alloc_calypso_export_item(ele_tbl)
      call alloc_calypso_import_item                                    &
     &   (ele_tbl%ntot_import, ele_tbl)
      call set_import_item_for_repart                                   &
     &   (ele_tbl%ntot_import, ele_tbl%ntot_import,                     &
     &    ele_tbl%item_import, ele_tbl%irev_import)
!
      call set_import_ele_for_repart(node, ele, part_tbl, idomain_new,  &
     &    ele_tbl%nrank_export, ele_tbl%ntot_export,                    &
     &    ele_tbl%irank_export, ele_tbl%istack_export,                  &
     &    ele_tbl%item_export)
      deallocate(num_send_ele, num_recv_ele)
!
      end subroutine const_ele_trans_tbl_for_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_num_send_ele_repart(nprocs, node, ele,           &
     &          part_tbl, idomain_new, num_send_ele)
!
      integer, intent(in) :: nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: part_tbl
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
!
      integer(kind = kint), intent(inout) :: num_send_ele(nprocs)
!
      integer(kind = kint), allocatable :: iflag_ele(:)
      integer(kind = kint) :: ntot_internal
      integer(kind = kint) :: i, ip, inod, iele, k1
!
!
!$omp parallel workshare
        num_send_ele(1:nprocs) = 0
!$omp end parallel workshare
!
      allocate(iflag_ele(ele%numele))
      do i = 1, part_tbl%nrank_export
!$omp parallel workshare
        iflag_ele(1:ele%numele) = 0
!$omp end parallel workshare
!
        ip =  part_tbl%irank_export(i)
        do iele = 1, ele%numele
!          if(ele%ie(iele,1) .gt. node%internal_node) cycle
          do k1 = 1, ele%nnod_4_ele
            inod = ele%ie(iele,k1)
            if(idomain_new(inod) .eq. ip) then
              iflag_ele(iele) = 1
              exit
            end if
          end do
        end do
!
        ntot_internal = 0
!$omp parallel do private(iele) reduction(+:ntot_internal)
        do iele = 1, ele%numele
          if(iflag_ele(iele) .gt. 0) then
            ntot_internal = ntot_internal + 1
          end if
        end do
!$omp end parallel do
        num_send_ele(ip+1) = ntot_internal
      end do
      deallocate(iflag_ele)
!
      end subroutine count_num_send_ele_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_import_ele_for_repart                              &
     &         (node, ele, part_tbl, idomain_new,                       &
     &          nrank_export, ntot_export, irank_export,                &
     &          istack_export, item_export)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: part_tbl
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
!
      integer(kind = kint), intent(in) :: nrank_export, ntot_export
      integer(kind = kint), intent(in) :: irank_export(nrank_export)
      integer(kind = kint), intent(in) :: istack_export(0:nrank_export)
!
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
!
      integer(kind = kint), allocatable :: iflag_ele(:)
      integer(kind = kint) :: i, j, icou, ip, inod, iele, k1, ipart
!
!
      allocate(iflag_ele(ele%numele))
!
      do i = 1, nrank_export
        ipart = 0
        do j = i, part_tbl%nrank_export
          if(irank_export(i) .eq. part_tbl%irank_export(i)) then
            ipart = j
            exit
          end if
        end do
!
!$omp parallel workshare
        iflag_ele(1:ele%numele) = 0
!$omp end parallel workshare
!
        ip =  irank_export(i)
        do iele = 1, ele%numele
          do k1 = 1, ele%nnod_4_ele
            inod = ele%ie(iele,k1)
            if(idomain_new(inod) .eq. ip) then
              iflag_ele(iele) = 1
              exit
            end if
          end do
        end do
!
        icou = istack_export(i-1)
        do iele = 1, ele%numele
          if(iflag_ele(iele) .gt. 0) then
            icou = icou + 1
            item_export(icou) = iele
          end if
        end do
      end do
      deallocate(iflag_ele)
!
      end subroutine set_import_ele_for_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_element_transfer_tbl(my_rank, ele, ele_tbl)
!
      integer, intent(in) :: my_rank
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
!
      integer(kind = kint) :: icou, i, ist, ied
!
!
      write(100+my_rank,*) 'ele_tbl%nrank_export', ele_tbl%nrank_export
      do i = 1, ele_tbl%nrank_export
        write(100+my_rank,*) i, 'ele_tbl%istack_export',                &
     &   ele_tbl%irank_export(i), ele_tbl%istack_export(i-1:i)
        ist = ele_tbl%istack_export(i-1)+1
        ied = ele_tbl%istack_export(i)
        do icou = ist, ied
          write(100+my_rank,*) icou, 'ele_tbl%item_export',             &
     &          ele_tbl%item_export(icou), ele%numele
        end do
      end do
!
      write(100+my_rank,*) 'ele_tbl%nrank_import', ele_tbl%nrank_import
      do i = 1, ele_tbl%nrank_import
        write(100+my_rank,*) i, 'ele_tbl%istack_import',                &
     &  ele_tbl%irank_import(i), ele_tbl%istack_import(i-1:i)
        ist = ele_tbl%istack_import(i-1)+1
        ied = ele_tbl%istack_import(i)
        do icou = ist, ied
          write(100+my_rank,*) icou, 'ele_tbl%item_import',             &
     &          ele_tbl%item_import(icou), ele_tbl%ntot_import
        end do
      end do
!
      end subroutine check_element_transfer_tbl
!
! ----------------------------------------------------------------------
!
      end module ele_trans_tbl_4_repart
