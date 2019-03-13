!>@file   gz_domain_data_IO_b.f90
!!@brief  module gz_domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine gz_read_domain_info_b                                &
!!     &         (id_rank, gz_flags, comm_IO)
!!      subroutine gz_read_import_data_b(gz_flags, comm_IO)
!!      subroutine gz_read_export_data_b(gz_flags, comm_IO)
!!        type(file_IO_flags), intent(inout) :: gz_flags
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine gz_write_domain_info_b(id_rank, comm_IO)
!!      subroutine gz_write_import_data_b(comm_IO)
!!      subroutine gz_write_export_data_b(comm_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!@endverbatim
!!
!@param id_file file ID
!
      module gz_domain_data_IO_b
!
      use m_precision
!
      use t_comm_table
      use binary_IO
      use gz_binary_IO
      use transfer_to_long_integers
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_domain_info_b                                  &
     &         (id_rank, gz_flags, comm_IO)
!
      use m_error_IDs
!
      integer, intent(in) :: id_rank
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: irank_read
!
!
      gz_flags%ierr_IO = 0
      call gz_read_one_integer_b                                        &
     &   (gz_flags%iflag_bin_swap, irank_read, gz_flags%ierr_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      if(irank_read .ne. id_rank) then
        gz_flags%ierr_IO = ierr_mesh
        return
      end if
      call gz_read_one_integer_b                                        &
     &   (gz_flags%iflag_bin_swap, comm_IO%num_neib, gz_flags%ierr_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
!
      call alloc_neighbouring_id(comm_IO)
!
      call gz_read_mul_integer_b(gz_flags%iflag_bin_swap,               &
     &    cast_long(comm_IO%num_neib), comm_IO%id_neib,                 &
     &    gz_flags%ierr_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      end subroutine gz_read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_import_data_b(gz_flags, comm_IO)
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(communication_table), intent(inout) :: comm_IO
!
!
      call alloc_import_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
!
        call gz_read_integer_stack_b(gz_flags%iflag_bin_swap,           &
     &      cast_long(comm_IO%num_neib), comm_IO%istack_import,         &
     &      comm_IO%ntot_import, gz_flags%ierr_IO)
        if(gz_flags%ierr_IO .gt. 0) return
!
        call alloc_import_item(comm_IO)
        call gz_read_mul_integer_b(gz_flags%iflag_bin_swap,             &
     &      cast_long(comm_IO%ntot_import), comm_IO%item_import,        &
     &      gz_flags%ierr_IO)
        if(gz_flags%ierr_IO .gt. 0) return
!
      else
        comm_IO%ntot_import = 0
        call alloc_import_item(comm_IO)
      end if
!
      end subroutine gz_read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_export_data_b(gz_flags, comm_IO)
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(communication_table), intent(inout) :: comm_IO
!
!
      call alloc_export_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
        call gz_read_integer_stack_b(gz_flags%iflag_bin_swap,           &
     &      cast_long(comm_IO%num_neib), comm_IO%istack_export,         &
     &      comm_IO%ntot_export, gz_flags%ierr_IO)
        if(gz_flags%ierr_IO .gt. 0) return
!
        call alloc_export_item(comm_IO)
        call gz_read_mul_integer_b(gz_flags%iflag_bin_swap,             &
     &      cast_long(comm_IO%ntot_export), comm_IO%item_export,        &
     &      gz_flags%ierr_IO)
        if(gz_flags%ierr_IO .gt. 0) return
      else
        comm_IO%ntot_export = 0
        call alloc_export_item(comm_IO)
      end if
!
      end subroutine gz_read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_domain_info_b(id_rank, comm_IO)
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
!
!
      call gz_write_one_integer_b(id_rank)
      call gz_write_one_integer_b(comm_IO%num_neib)
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(comm_IO%num_neib), comm_IO%id_neib)
!
      end subroutine gz_write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_import_data_b(comm_IO)
!
      type(communication_table), intent(in) :: comm_IO
!
!
      call gz_write_integer_stack_b                                     &
     &   (cast_long(comm_IO%num_neib), comm_IO%istack_import)
      call gz_write_mul_integer_b                                       &
     &   (cast_long(comm_IO%ntot_import), comm_IO%item_import)
!
      end subroutine gz_write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_export_data_b(comm_IO)
!
      type(communication_table), intent(in) :: comm_IO
!
!
      call gz_write_integer_stack_b                                     &
     &   (cast_long(comm_IO%num_neib), comm_IO%istack_export)
      call gz_write_mul_integer_b                                       &
     &   (cast_long(comm_IO%ntot_export), comm_IO%item_export)
!
      end subroutine gz_write_export_data_b
!
! -----------------------------------------------------------------------
!
      end module gz_domain_data_IO_b
