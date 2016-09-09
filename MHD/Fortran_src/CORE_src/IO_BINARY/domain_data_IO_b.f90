!>@file   domain_data_IO_b.f90
!!@brief  module domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for doimain data IO
!!
!!@verbatim
!!      subroutine read_domain_info_b
!!      subroutine read_import_data_b
!!      subroutine read_export_data_b
!!
!!      subroutine write_domain_info_b
!!      subroutine write_import_data_b
!!      subroutine write_export_data_b
!!@endverbatim
!!
!@param id_file file ID
!
      module domain_data_IO_b
!
      use m_precision
!
      use m_comm_data_IO
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
      subroutine read_domain_info_b
!
!
      call read_one_integer_b(my_rank_IO)
      call read_one_integer_b(comm_IO%num_neib)
!
      call allocate_neib_domain_IO
!
      call read_mul_integer_b(comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_import_data_b
!
!
      call allocate_import_stack_IO
      if (comm_IO%num_neib .gt. 0) then
!
        call read_integer_stack_b(comm_IO%num_neib,                     &
     &      istack_import_IO, comm_IO%ntot_import)
!
        call allocate_import_item_IO
        call read_mul_integer_b(comm_IO%ntot_import, item_import_IO)
!
      else
        comm_IO%ntot_import = 0
        call allocate_import_item_IO
      end if
!
      end subroutine read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data_b
!
!
      call allocate_export_stack_IO
      if (comm_IO%num_neib .gt. 0) then
        call read_integer_stack_b(comm_IO%num_neib,                     &
     &      istack_export_IO, comm_IO%ntot_export)
!
        call allocate_export_item_IO
        call read_mul_integer_b(comm_IO%ntot_export, item_export_IO)
      else
        comm_IO%ntot_export = 0
        call allocate_export_item_IO
      end if
!
      end subroutine read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_domain_info_b
!
!
      call write_one_integer_b(my_rank_IO)
      call write_one_integer_b(comm_IO%num_neib)
!
      call write_mul_integer_b(comm_IO%num_neib, comm_IO%id_neib)
!
      call deallocate_neib_domain_IO
!
      end subroutine write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_import_data_b
!
!
      call write_integer_stack_b(comm_IO%num_neib, istack_import_IO)
      call write_mul_integer_b(comm_IO%ntot_import, item_import_IO)
!
      call deallocate_import_item_IO
!
      end subroutine write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data_b
!
!
      call write_integer_stack_b(comm_IO%num_neib, istack_export_IO)
      call write_mul_integer_b(comm_IO%ntot_export, item_export_IO)
!
      call deallocate_export_item_IO
!
      end subroutine write_export_data_b
!
! -----------------------------------------------------------------------!
      end module domain_data_IO_b
