!>@file   const_comm_tbl_to_new_mesh.f90
!!@brief  module const_comm_tbl_to_new_mesh
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Construct commnunication table to new partitionning
!!
!!@verbatim
!!      subroutine const_comm_tbl_to_new_part                           &
!!     &         (part_grp, nloop, num_send_tmp, num_recv_tmp, part_tbl)
!!        type(group_data), intent(in) :: part_grp
!!        type(calypso_comm_table), intent(inout) :: part_tbl(nloop)
!!
!!      subroutine gather_num_trans_for_repart                          &
!!     &         (nloop, part_grp, num_send_tmp, num_recv_tmp)
!!        integer(kind = kint), intent(in) :: nloop
!!        type(group_data), intent(in) :: part_grp
!!      subroutine send_back_istack_import_repart(part_grp, part_tbl,   &
!!     &          nloop, num_recv_tmp, num_send_tmp)
!!        type(group_data), intent(in) :: part_grp
!!        type(calypso_comm_table), intent(in) :: part_tbl(nloop)
!!@endverbatim
!
      module const_comm_tbl_to_new_mesh
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_group_data
      use t_calypso_comm_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_comm_tbl_to_new_part                             &
     &         (part_grp, nloop, num_send_tmp, num_recv_tmp, part_tbl)
!
      use set_comm_tbl_to_new_part
!
      type(group_data), intent(in) :: part_grp
      integer(kind = kint), intent(in) :: nloop
      integer(kind = kint), intent(in)                                  &
     &                     :: num_send_tmp(part_grp%num_grp)
      integer(kind = kint), intent(in) :: num_recv_tmp(nprocs,nloop)
!
      type(calypso_comm_table), intent(inout) :: part_tbl(nloop)
!
      integer(kind = kint) :: iloop
!
!
      do iloop = 1, nloop
        part_tbl(iloop)%iflag_self_copy = 0
      end do
      call count_num_export_for_repart                                  &
     &   (my_rank, part_grp%num_grp, num_send_tmp,                      &
     &    part_tbl(1)%iflag_self_copy, part_tbl(1)%nrank_export)
!
      do iloop = 1, nloop
        call alloc_calypso_export_num(part_tbl(iloop))
      end do
      call set_istack_export_for_repart                                 &
     &   (my_rank, part_grp%num_grp, num_send_tmp,                      &
     &    part_tbl(1)%nrank_export, part_tbl(1)%ntot_export,            &
     &    part_tbl(1)%irank_export, part_tbl(1)%num_export,             &
     &    part_tbl(1)%istack_export)
!
      do iloop = 1, nloop
        call alloc_calypso_export_item(part_tbl(iloop))
      end do
!
      call set_export_item_for_repart(my_rank, part_grp, num_send_tmp,  &
     &    part_tbl(1)%nrank_export, part_tbl(1)%ntot_export,            &
     &    part_tbl(1)%istack_export, part_tbl(1)%item_export)
!
!
      do iloop = 1, nloop
        call count_num_import_for_repart                                &
     &     (iloop, nprocs, part_grp%num_grp, num_recv_tmp(1,iloop),     &
     &      part_tbl(iloop)%nrank_import)
        call alloc_calypso_import_num(part_tbl(iloop))
!
        call set_istack_import_for_repart(iloop,                        &
     &      my_rank, nprocs, part_grp%num_grp, num_recv_tmp(1,iloop),   &
     &      part_tbl(iloop)%nrank_import, part_tbl(iloop)%ntot_import,  &
     &      part_tbl(iloop)%irank_import, part_tbl(iloop)%num_import,   &
     &      part_tbl(iloop)%istack_import)
!
        call alloc_calypso_import_item                                  &
     &     (part_tbl(iloop)%ntot_import, part_tbl(iloop))
        call set_import_item_for_repart                                 &
     &     (part_tbl(iloop)%ntot_import, part_tbl(iloop)%ntot_import,   &
     &      part_tbl(iloop)%item_import, part_tbl(iloop)%irev_import)
      end do
!
      end subroutine const_comm_tbl_to_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gather_num_trans_for_repart                            &
     &         (nloop, part_grp, num_send_tmp, num_recv_tmp)
!
      use calypso_mpi_int
!
      integer(kind = kint), intent(in) :: nloop
      type(group_data), intent(in) :: part_grp
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_send_tmp(part_grp%num_grp)
      integer(kind = kint), intent(inout) :: num_recv_tmp(nprocs,nloop)
!
      integer(kind = kint) :: i, iloop
      integer :: irank_recv
!
      do i = 1, part_grp%num_grp
        iloop = 1 + (i-1) / nprocs
        irank_recv = mod(i-1,nprocs)
        num_send_tmp(i)                                                 &
     &      = part_grp%istack_grp(i) - part_grp%istack_grp(i-1)
        call calypso_mpi_gather_one_int                                 &
     &     (num_send_tmp(i), num_recv_tmp(1,iloop), irank_recv)
      end do
!
      end subroutine gather_num_trans_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine send_back_istack_import_repart                         &
     &         (part_grp, nloop, num_recv_tmp, num_send_tmp)
!
      use calypso_mpi_int
!
      type(group_data), intent(in) :: part_grp
!
      integer(kind = kint), intent(in) :: nloop
      integer(kind = kint), intent(in) :: num_recv_tmp(nprocs,nloop)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_send_tmp(part_grp%num_grp)
!
      integer(kind = kint) :: i, iloop
      integer :: irank_recv
!
!
      do i = 1, part_grp%num_grp
        iloop = 1 + (i-1) / nprocs
        irank_recv = mod(i-1,nprocs)
        call calypso_mpi_scatter_one_int                                &
     &     (num_recv_tmp(1,iloop), num_send_tmp(i), irank_recv)
      end do
!
      end subroutine send_back_istack_import_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_istack_import_repart                               &
     &         (part_tbl, nloop, num_recv_tmp)
!
      use calypso_mpi_int
!
      type(calypso_comm_table), intent(in) :: part_tbl(nloop)
      integer(kind = kint), intent(in) :: nloop
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_recv_tmp(nprocs,nloop)
!
      integer(kind = kint) :: i, iloop, ip
      integer :: irank_recv
!
!
!$omp parallel
      do iloop = 1, nloop
!$omp do private(ip)
        do ip = 1, nprocs
          num_recv_tmp(ip,iloop) = -1
        end do
!$omp end do
!$omp do private(i,ip)
        do i = 1, part_tbl(iloop)%nrank_import
          ip = part_tbl(iloop)%irank_import(i)
          num_recv_tmp(ip,iloop) = part_tbl(iloop)%istack_import(ip-1) 
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine set_istack_import_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_ext_istack_import_repart                         &
     &         (part_tbl, ext_tbl, nloop, num_recv_tmp)
!
      use calypso_mpi_int
!
      type(calypso_comm_table), intent(in) :: part_tbl(nloop)
      type(calypso_comm_table), intent(in) :: ext_tbl(nloop)
      integer(kind = kint), intent(in) :: nloop
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_recv_tmp(nprocs,nloop)
!
      integer(kind = kint) :: i, iloop, ip
      integer :: irank_recv
!
!
!$omp parallel
      do iloop = 1, nloop
!$omp do private(ip)
        do ip = 1, nprocs
          num_recv_tmp(ip,iloop) = -1
        end do
!$omp end do
!$omp do private(i,ip)
        do i = 1, ext_tbl(iloop)%nrank_import
          ip = ext_tbl(iloop)%irank_import(i)
          num_recv_tmp(ip,iloop) = ext_tbl(iloop)%istack_import(ip-1)   &
     &                            + part_tbl(iloop)%ntot_import
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine set_ext_istack_import_repart
!
! ----------------------------------------------------------------------
!
      end module const_comm_tbl_to_new_mesh
