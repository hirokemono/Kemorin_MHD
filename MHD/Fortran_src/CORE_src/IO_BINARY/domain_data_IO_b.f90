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
      use binary_IO
!
!
      call read_fld_inthead_b(my_rank_IO)
      call read_fld_inthead_b(num_neib_domain_IO)
!
      call allocate_neib_domain_IO
!
      if (num_neib_domain_IO .gt. 0) then
        call write_fld_mul_inthead_b                                    &
     &     (num_neib_domain_IO, id_neib_domain_IO)
      end if
!
      end subroutine read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_import_data_b
!
      use binary_IO
!
!
      call allocate_import_stack_IO
      if (num_neib_domain_IO .gt. 0) then
!
        call read_fld_intstack_b(num_neib_domain_IO,                    &
     &      istack_import_IO, ntot_import_IO)
!
        call allocate_import_item_IO
        call read_fld_mul_inthead_b(ntot_import_IO, item_import_IO)
      else
        ntot_import_IO = 0
        call allocate_import_item_IO
      end if
!
      end subroutine read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data_b
!
      use binary_IO
!
!
      call allocate_export_stack_IO
!
      if (num_neib_domain_IO .gt. 0) then
        call read_fld_intstack_b(num_neib_domain_IO,                    &
     &      istack_export_IO, ntot_export_IO)
!
        call allocate_export_item_IO
        call read_fld_mul_inthead_b(ntot_export_IO, item_export_IO)
      else
        ntot_export_IO = 0
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
      use binary_IO
!
!
      call write_fld_inthead_b(my_rank_IO)
      call write_fld_inthead_b(num_neib_domain_IO)
!
      if (num_neib_domain_IO .gt. 0) then
        call read_fld_mul_inthead_b                                     &
     &     (num_neib_domain_IO, id_neib_domain_IO)
      end if
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
      use binary_IO
!
!
      call write_fld_intstack_b(num_neib_domain_IO, istack_import_IO)
      call write_fld_mul_inthead_b(ntot_import_IO, item_import_IO)
!
      call deallocate_import_item_IO
!
      end subroutine write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data_b
!
      use binary_IO
!
!
      call write_fld_intstack_b(num_neib_domain_IO, istack_export_IO)
      call write_fld_mul_inthead_b(ntot_export_IO, item_export_IO)
!
      call deallocate_export_item_IO
!
      end subroutine write_export_data_b
!
! -----------------------------------------------------------------------!
      end module domain_data_IO_b
