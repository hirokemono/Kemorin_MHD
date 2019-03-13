!>@file   domain_data_IO_b.f90
!!@brief  module domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for doimain data IO
!!
!!@verbatim
!!      subroutine read_domain_info_b                                   &
!!     &         (id_rank, bin_flags, comm_IO)
!!      subroutine read_import_data_b(bin_flags, comm_IO)
!!      subroutine read_export_data_b(bin_flags, comm_IO)
!!        type(file_IO_flags), intent(inout) :: bin_flags
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine write_domain_info_b(id_rank, comm_IO)
!!      subroutine write_import_data_b(comm_IO)
!!      subroutine write_export_data_b(comm_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!@endverbatim
!!
!@param id_file file ID
!
      module domain_data_IO_b
!
      use m_precision
!
      use t_comm_table
      use binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_domain_info_b                                     &
     &         (id_rank, bin_flags, comm_IO)
!
      use m_error_IDs
!
      integer, intent(in) :: id_rank
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: irank_read
      integer(kind = kint_gl) :: num64
!
!
      bin_flags%ierr_IO = 0
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    irank_read, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
       if(irank_read .ne. id_rank) then
         bin_flags%ierr_IO = ierr_mesh
         return
       end if
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    comm_IO%num_neib, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call alloc_neighbouring_id(comm_IO)
!
      num64 = comm_IO%num_neib
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    num64, comm_IO%id_neib, bin_flags%ierr_IO)
!
      end subroutine read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_import_data_b(bin_flags, comm_IO)
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call alloc_import_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
!
        num64 = comm_IO%num_neib
        call read_integer_stack_b(bin_flags%iflag_bin_swap,             &
     &      num64, comm_IO%istack_import,                               &
     &      comm_IO%ntot_import, bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
!
        call alloc_import_item(comm_IO)
        num64 = comm_IO%ntot_import
        call read_mul_integer_b(bin_flags%iflag_bin_swap,               &
     &      num64, comm_IO%item_import, bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
      else
        comm_IO%ntot_import = 0
        call alloc_import_item(comm_IO)
      end if
!
      end subroutine read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data_b(bin_flags, comm_IO)
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call alloc_export_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
        num64 = comm_IO%num_neib
        call read_integer_stack_b(bin_flags%iflag_bin_swap,             &
     &      num64, comm_IO%istack_export,                               &
     &      comm_IO%ntot_export, bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
!
        call alloc_export_item(comm_IO)
        num64 = comm_IO%ntot_export
        call read_mul_integer_b(bin_flags%iflag_bin_swap,               &
     &      num64, comm_IO%item_export, bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
      else
        comm_IO%ntot_export = 0
        call alloc_export_item(comm_IO)
      end if
!
      end subroutine read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_domain_info_b(id_rank, comm_IO)
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call write_one_integer_b(id_rank)
      call write_one_integer_b(comm_IO%num_neib)
!
      num64 = comm_IO%num_neib
      call write_mul_integer_b(num64, comm_IO%id_neib)
!
      end subroutine write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_import_data_b(comm_IO)
!
      type(communication_table), intent(in) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = comm_IO%num_neib
      call write_integer_stack_b                                        &
    &    (num64, comm_IO%istack_import)
      num64 = comm_IO%ntot_import
      call write_mul_integer_b                                          &
    &    (num64, comm_IO%item_import)
!
      end subroutine write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data_b(comm_IO)
!
      type(communication_table), intent(in) :: comm_IO
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = comm_IO%num_neib
      call write_integer_stack_b                                        &
     &   (num64, comm_IO%istack_export)
      num64 = comm_IO%ntot_export
      call write_mul_integer_b                                          &
     &   (num64, comm_IO%item_export)
!
      end subroutine write_export_data_b
!
! -----------------------------------------------------------------------!
      end module domain_data_IO_b
