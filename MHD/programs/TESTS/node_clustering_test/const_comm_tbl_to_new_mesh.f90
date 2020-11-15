!>@file   const_comm_tbl_to_new_mesh.f90
!!@brief  module const_comm_tbl_to_new_mesh
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Construct commnunication table to new partitionning
!!
!!@verbatim
!!      subroutine const_int_comm_tbl_to_new_part(part_grp, part_tbl)
!!        type(group_data), intent(in) :: part_grp
!!        type(calypso_comm_table), intent(inout) :: part_tbl
!!      subroutine const_ext_comm_tbl_to_new_part                       &
!!     &         (ext_int_grp, part_tbl, ext_tbl)
!!        type(group_data), intent(in) :: ext_int_grp
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(calypso_comm_table), intent(inout) :: ext_tbl
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
      integer(kind = kint), allocatable :: num_send_tmp(:)
      integer(kind = kint), allocatable :: num_recv_tmp(:)
!
      private :: num_send_tmp, num_recv_tmp
      private :: const_comm_tbl_to_new_part
      private :: gather_num_trans_for_repart
      private :: const_int_comm_tbl_to_new_part
      private :: send_back_ext_istack_import
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_int_comm_tbl_to_new_part(part_grp, part_tbl)
!
      type(group_data), intent(in) :: part_grp
      type(calypso_comm_table), intent(inout) :: part_tbl
!
!
      allocate(num_send_tmp(part_grp%num_grp))
      allocate(num_recv_tmp(part_grp%num_grp))
!
      call gather_num_trans_for_repart                                  &
     &   (part_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (part_grp, num_send_tmp, num_recv_tmp, part_tbl)
!      call send_back_istack_import_repart                              &
!     &   (part_grp, part_tbl, num_recv_tmp, num_send_tmp)
      deallocate(num_send_tmp, num_recv_tmp)
!
      end subroutine const_int_comm_tbl_to_new_part
!
! ----------------------------------------------------------------------
!
      subroutine const_ext_comm_tbl_to_new_part                         &
     &         (ext_int_grp, part_tbl, ext_tbl)
!
      type(group_data), intent(in) :: ext_int_grp
      type(calypso_comm_table), intent(in) :: part_tbl
!
      type(calypso_comm_table), intent(inout) :: ext_tbl
!
!
      allocate(num_send_tmp(ext_int_grp%num_grp))
      allocate(num_recv_tmp(ext_int_grp%num_grp))
!
      call gather_num_trans_for_repart                                  &
     &   (ext_int_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (ext_int_grp, num_send_tmp, num_recv_tmp, ext_tbl)
!      call send_back_ext_istack_import                                 &
!     &   (ext_int_grp, part_tbl, ext_tbl, num_recv_tmp, num_send_tmp)
      deallocate(num_send_tmp, num_recv_tmp)
!
      end subroutine const_ext_comm_tbl_to_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_comm_tbl_to_new_part                             &
     &         (part_grp, num_send_tmp, num_recv_tmp, part_tbl)
!
      use set_comm_tbl_to_new_part
!
      type(group_data), intent(in) :: part_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: num_send_tmp(part_grp%num_grp)
      integer(kind = kint), intent(in)                                  &
     &                     :: num_recv_tmp(part_grp%num_grp)
!
      type(calypso_comm_table), intent(inout) :: part_tbl
!
!
      part_tbl%iflag_self_copy = 0
      call count_num_export_for_repart                                  &
     &   (my_rank, int(part_grp%num_grp), num_send_tmp,                 &
     &    part_tbl%iflag_self_copy, part_tbl%nrank_export)
!
      call alloc_calypso_export_num(part_tbl)
      call set_istack_export_for_repart                                 &
     &   (my_rank, int(part_grp%num_grp), num_send_tmp,                 &
     &    part_tbl%nrank_export, part_tbl%ntot_export,                  &
     &    part_tbl%irank_export, part_tbl%num_export,                   &
     &    part_tbl%istack_export)
!
      call alloc_calypso_export_item(part_tbl)
      call set_export_item_for_repart(my_rank, nprocs, part_grp,        &
     &    num_send_tmp, part_tbl%nrank_export, part_tbl%ntot_export,    &
     &    part_tbl%istack_export, part_tbl%item_export)
!
!
      call count_num_import_for_repart(int(part_grp%num_grp),           &
     &    num_recv_tmp, part_tbl%nrank_import)
!
      call alloc_calypso_import_num(part_tbl)
      call set_istack_import_for_repart(my_rank, nprocs, num_recv_tmp,  &
     &    part_tbl%nrank_import, part_tbl%ntot_import,                  &
     &    part_tbl%irank_import, part_tbl%num_import,                   &
     &    part_tbl%istack_import)
!
      call alloc_calypso_import_item(part_tbl%ntot_import, part_tbl)
      call set_import_item_for_repart                                   &
     &     (part_tbl%ntot_import, part_tbl%ntot_import,                 &
     &      part_tbl%item_import, part_tbl%irev_import)
!
      end subroutine const_comm_tbl_to_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gather_num_trans_for_repart                            &
     &         (part_grp, num_send_tmp, num_recv_tmp)
!
      use calypso_mpi_int
!
      type(group_data), intent(in) :: part_grp
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_send_tmp(part_grp%num_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: num_recv_tmp(part_grp%num_grp)
!
      integer(kind = kint) :: i
!
!$omp parallel do private(i)
      do i = 1, nprocs
        num_send_tmp(i)                                                 &
     &      = part_grp%istack_grp(i) - part_grp%istack_grp(i-1)
      end do
!$omp end parallel do
!
      call calypso_mpi_alltoall_one_int                                 &
     &   (num_send_tmp(1), num_recv_tmp(1))
!
      end subroutine gather_num_trans_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine send_back_istack_import_repart                         &
     &         (part_grp, part_tbl, num_recv_tmp, num_send_tmp)
!
      use calypso_mpi_int
!
      type(group_data), intent(in) :: part_grp
      type(calypso_comm_table), intent(in) :: part_tbl
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_recv_tmp(part_grp%num_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: num_send_tmp(part_grp%num_grp)
!
      integer(kind = kint) :: i, ip
!
!
!$omp parallel do private(ip)
      do ip = 1, nprocs
        num_recv_tmp(ip) = -1
      end do
!$omp end parallel do
!$omp parallel do private(i,ip)
      do i = 1, part_tbl%nrank_import
        ip = part_tbl%irank_import(i) + 1
        num_recv_tmp(ip) = part_tbl%istack_import(i-1) 
      end do
!$omp end parallel do
!
      call calypso_mpi_alltoall_one_int                                 &
     &   (num_recv_tmp(1), num_send_tmp(1))
!
      end subroutine send_back_istack_import_repart
!
! ----------------------------------------------------------------------
!
      subroutine send_back_ext_istack_import(part_grp,                  &
     &          part_tbl, ext_tbl, num_recv_tmp, num_send_tmp)
!
      use calypso_mpi_int
!
      type(group_data), intent(in) :: part_grp
      type(calypso_comm_table), intent(in) :: part_tbl
      type(calypso_comm_table), intent(in) :: ext_tbl
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_recv_tmp(part_grp%num_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: num_send_tmp(part_grp%num_grp)
!
      integer(kind = kint) :: i, ip
!
!
!$omp do private(ip)
        do ip = 1, nprocs
          num_recv_tmp(ip) = -1
        end do
!$omp end do
!$omp do private(i,ip)
        do i = 1, ext_tbl%nrank_import
          ip = ext_tbl%irank_import(i) + 1
          num_recv_tmp(ip) = ext_tbl%istack_import(i-1)                 &
     &                            + part_tbl%ntot_import
        end do
!$omp end do
!
      call calypso_mpi_alltoall_one_int                                 &
     &   (num_recv_tmp(1), num_send_tmp(1))
!
      end subroutine send_back_ext_istack_import
!
! ----------------------------------------------------------------------
!
      end module const_comm_tbl_to_new_mesh
