!>@file   gz_domain_data_IO_b.f90
!!@brief  module gz_domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine gz_read_domain_info_b
!!      subroutine gz_read_import_data_b
!!      subroutine gz_read_export_data_b
!!
!!      subroutine gz_write_domain_info_b
!!      subroutine gz_write_import_data_b
!!      subroutine gz_write_export_data_b
!!@endverbatim
!!
!@param id_file file ID
!
      module gz_domain_data_IO_b
!
      use m_precision
!
      use m_comm_data_IO
      use gz_binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_domain_info_b
!
!
      call gz_read_one_integer_b(my_rank_IO)
      call gz_read_one_integer_b(comm_IO%num_neib)
!
      call allocate_neib_domain_IO
!
      call gz_read_mul_integer_b(comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine gz_read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_import_data_b
!
!
      call allocate_import_stack_IO
      if (comm_IO%num_neib .gt. 0) then
!
        call gz_read_integer_stack_b(comm_IO%num_neib,                  &
     &      istack_import_IO, comm_IO%ntot_import)
!
        call allocate_import_item_IO
        call gz_read_mul_integer_b                                      &
     &     (comm_IO%ntot_import, comm_IO%item_import)
!
      else
        comm_IO%ntot_import = 0
        call allocate_import_item_IO
      end if
!
      end subroutine gz_read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_export_data_b
!
!
      call allocate_export_stack_IO
      if (comm_IO%num_neib .gt. 0) then
        call gz_read_integer_stack_b(comm_IO%num_neib,                  &
     &      comm_IO%istack_export, comm_IO%ntot_export)
!
        call allocate_export_item_IO
        call gz_read_mul_integer_b                                      &
     &     (comm_IO%ntot_export, comm_IO%item_export)
      else
        comm_IO%ntot_export = 0
        call allocate_export_item_IO
      end if
!
      end subroutine gz_read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_domain_info_b
!
!
      call gz_write_one_integer_b(my_rank_IO)
      call gz_write_one_integer_b(comm_IO%num_neib)
!
      call gz_write_mul_integer_b                                       &
     &   (comm_IO%num_neib, comm_IO%id_neib)
!
      call deallocate_neib_domain_IO
!
      end subroutine gz_write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_import_data_b
!
!
      call gz_write_integer_stack_b                                     &
     &   (comm_IO%num_neib, istack_import_IO)
      call gz_write_mul_integer_b                                       &
     &   (comm_IO%ntot_import, comm_IO%item_import)
!
      call deallocate_import_item_IO
!
      end subroutine gz_write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_export_data_b
!
!
      call gz_write_integer_stack_b                                     &
     &  (comm_IO%num_neib, comm_IO%istack_export)
      call gz_write_mul_integer_b                                       &
     &   (comm_IO%ntot_export, comm_IO%item_export)
!
      call deallocate_export_item_IO
!
      end subroutine gz_write_export_data_b
!
! -----------------------------------------------------------------------
!
      end module gz_domain_data_IO_b
