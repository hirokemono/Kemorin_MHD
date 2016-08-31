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
      call gz_read_fld_inthead_b(my_rank_IO)
      call gz_read_fld_inthead_b(num_neib_domain_IO)
!
      call allocate_neib_domain_IO
!
      call gz_read_fld_mul_inthead_b                                    &
     &   (num_neib_domain_IO, id_neib_domain_IO)
!
      write(*,*) 'num_neib_domain_IO', num_neib_domain_IO
      write(*,*) 'id_neib_domain_IO', id_neib_domain_IO
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
      if (num_neib_domain_IO .gt. 0) then
!
        call gz_read_fld_intstack_b(num_neib_domain_IO,                 &
     &      istack_import_IO, ntot_import_IO)
!
        call allocate_import_item_IO
        call gz_read_fld_mul_inthead_b(ntot_import_IO, item_import_IO)
!
      else
        ntot_import_IO = 0
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
      if (num_neib_domain_IO .gt. 0) then
        call gz_read_fld_intstack_b(num_neib_domain_IO,                 &
     &      istack_export_IO, ntot_export_IO)
!
        call allocate_export_item_IO
        call gz_read_fld_mul_inthead_b(ntot_export_IO, item_export_IO)
      else
        ntot_export_IO = 0
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
      call gz_write_fld_inthead_b(my_rank_IO)
      call gz_write_fld_inthead_b(num_neib_domain_IO)
!
      call gz_write_fld_mul_inthead_b                                   &
     &   (num_neib_domain_IO, id_neib_domain_IO)
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
      call gz_write_fld_intstack_b                                      &
     &   (num_neib_domain_IO, istack_import_IO)
      call gz_write_fld_mul_inthead_b(ntot_import_IO, item_import_IO)
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
      call gz_write_fld_intstack_b                                      &
     &  (num_neib_domain_IO, istack_export_IO)
      call gz_write_fld_mul_inthead_b(ntot_export_IO, item_export_IO)
!
      call deallocate_export_item_IO
!
      end subroutine gz_write_export_data_b
!
! -----------------------------------------------------------------------!
      end module gz_domain_data_IO_b
